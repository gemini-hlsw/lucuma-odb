// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gnirs
package spectroscopy

import cats.Monad
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.data.NonEmptyVector
import cats.data.State
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.order.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFpuOther
import lucuma.core.enums.GnirsPixelScale
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.model.sequence.gnirs.GnirsGratingWavelength
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.refined.numeric.NonZeroInt
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.AtomBuilder

import java.util.UUID

object Science:

  val ScienceCycleTitle: NonEmptyString =
    NonEmptyString.unsafeFrom("Science Cycle")

  val NighttimeCalTitle: NonEmptyString =
    NonEmptyString.unsafeFrom("Nighttime Calibrations")

  val DaytimePinholeTitle: NonEmptyString =
    NonEmptyString.unsafeFrom("Daytime Pinhole")

  /**
   * The pinhole FPU used for a daytime pinhole flat, chosen by pixel scale:
   * the small pinhole for the long (0.05"/pix) cameras and the large pinhole
   * for the short (0.15"/pix) cameras.
   */
  def pinholeFpu(camera: GnirsCamera): GnirsFpuOther =
    camera.pixelScale match
      case GnirsPixelScale.PixelScale_0_05 => GnirsFpuOther.Pinhole1
      case GnirsPixelScale.PixelScale_0_15 => GnirsFpuOther.Pinhole3

  /** A visit shouldn't take more than this before breaking for a telluric. */
  val MaxVisitLength: TimeSpan =
    3.hourTimeSpan

  /** Maximum time that may pass between (inline) flats. */
  val MaxSciencePeriod: TimeSpan =
    90.minuteTimeSpan

  private val Two: NonZeroInt = NonZeroInt.unsafeFrom(2)

  private object SeqState extends gnirs.GnirsSequenceState

  /** `cals` holds the flat, the arc or both; empty for telluric sequences. */
  case class StepDefinition(
    wavelength:   ScienceWavelength,
    scienceSteps: NonEmptyList[ProtoStep[GnirsDynamicConfig]],
    cals:         Option[NonEmptyList[ProtoStep[GnirsDynamicConfig]]]
  ):
    /**
     * Cycle count: round up so that we always deliver at least the requested
     * number of on-source exposures. Sky steps don't contribute to the S/N, so
     * cycles with sky offsets require extra repeats.
     * On-source means guided, the dither positions are guided, the large sky offsets unguided.
     * Keyed on the per-offset guide state, so the configured guiding
     * drives the cycle count for both slit and IFU.
     */
    def cycleCount(t: IntegrationTime): Either[String, NonNegInt] =
      calculateCycleCount[GnirsDynamicConfig](s => s.telescopeConfig.guiding.isGuided, scienceSteps.toList, t)

  object StepDefinition:

    // PreDef is a StepDefinition before SmartGcal expansion.
    case class PreDef(
      wavelength:   ScienceWavelength,
      scienceSteps: NonEmptyList[ProtoStep[GnirsDynamicConfig]],
      // Unexpanded SmartGcal (flat, arc), absent for telluric sequences.
      cals:         Option[(ProtoStep[GnirsDynamicConfig], ProtoStep[GnirsDynamicConfig])]
    ):
      def expand[F[_]: Monad](
        static:   GnirsStaticConfig,
        expander: SmartGcalExpander[F, GnirsStaticConfig, GnirsDynamicConfig]
      ): EitherT[F, String, StepDefinition] =

        // The read mode of a calibration step is determined by its exposure
        // time, which comes from the SmartGcal lookup (and so may differ from
        // the science read mode).
        def adjustReadMode(s: ProtoStep[GnirsDynamicConfig]): ProtoStep[GnirsDynamicConfig] =
          s.copy(value = s.value.copy(readMode = GnirsReadMode.forExposureTime(s.value.exposure)))

        cals.fold(EitherT.pure(StepDefinition(wavelength, scienceSteps, none))): (flat, arc) =>
          // 111/LXD, for instance, has arcs but no slit flat.
          EitherT(expander.expandFlatAndOrArc(static, flat, arc))
            .map(cs => StepDefinition(wavelength, scienceSteps, cs.map(adjustReadMode).some))

    object PreDef:

      def apply(
        config:  Config,
        sw:      ScienceWavelength,
        time:    IntegrationTime,
        calRole: Option[CalibrationRole]
      ): PreDef =
        // Configure the dynamic config for a science step, then traverse the
        // telescope configs from the observing mode in order, producing one
        // science ProtoStep per offset.  The same (science) dynamic config is
        // used for the calibration steps so the smart gcal lookup matches.
        val resolvedReadMode = config.explicitReadMode.getOrElse(GnirsReadMode.forExposureTime(time.exposureTime))
        val acqMirror        = GnirsAcquisitionMirrorMode.Out(
          config.prism,
          config.grating,
          GnirsGratingWavelength(sw.centralWavelength)
        )

        val sciClass = calRole.sciClass

        // Telluric sequences are standard-star observations and do not carry
        // their own flats & arcs; those come with the associated science.  We
        // still build the (cheap) unexpanded flat/arc placeholders, but only
        // hand them to PreDef when they're wanted — when absent, `expand`
        // skips the SmartGcal lookup entirely.
        val includeCals = !calRole.contains(CalibrationRole.Telluric)

        SeqState.eval:
          for
            _  <- State.modify[GnirsDynamicConfig]: dyn =>
                    dyn.copy(
                      exposure          = time.exposureTime,
                      coadds            = sw.coadds,
                      filter            = config.filter,
                      decker            = config.decker,
                      fpu               = config.fpu,
                      acquisitionMirror = acqMirror,
                      camera            = config.camera,
                      focus             = config.focus,
                      readMode          = resolvedReadMode
                    )
            ss <- config.telescopeConfigs.traverse(SeqState.scienceStep(_, sciClass))
            ct  = ss.last.telescopeConfig.copy(guiding = StepGuideState.Disabled)
            f  <- SeqState.flatStep(ct, ObserveClass.NightCal)
            r  <- SeqState.arcStep(ct, ObserveClass.NightCal)
          yield PreDef(
            sw,
            ss,
            Option.when(includeCals)((f, r))
          )

    def compute[F[_]: Monad](
      config:   Config,
      sw:       ScienceWavelength,
      time:     IntegrationTime,
      static:   GnirsStaticConfig,
      expander: SmartGcalExpander[F, GnirsStaticConfig, GnirsDynamicConfig],
      calRole:  Option[CalibrationRole]
    ): EitherT[F, String, StepDefinition] =
      PreDef(config, sw, time, calRole).expand(static, expander)

    /**
     * One step definition per central wavelength.  Each gets its own SmartGcal
     * expansion, since the flat and arc lookups are keyed on the wavelength.
     */
    def computeAll[F[_]: Monad](
      config:   Config,
      times:    NonEmptyList[(ScienceWavelength, IntegrationTime)],
      static:   GnirsStaticConfig,
      expander: SmartGcalExpander[F, GnirsStaticConfig, GnirsDynamicConfig],
      calRole:  Option[CalibrationRole]
    ): EitherT[F, String, NonEmptyList[StepDefinition]] =
      times.traverse((sw, t) => compute(config, sw, t, static, expander, calRole))

  end StepDefinition

  /**
   * One central wavelength's contribution to the science sequence: its steps
   * (science plus, unless this is a telluric, its own flat and/or arc), the
   * estimated duration of a single science cycle at that wavelength, and the
   * number of cycles needed to reach the requested signal-to-noise there.
   */
  case class WavelengthBlock(
    steps:         StepDefinition,
    cycleEstimate: TimeSpan,
    goalCycles:    NonNegInt
  )

  /**
   * Generates the science sequence across every central wavelength.
   *
   * Each wavelength is a separate configuration whose flats and arcs are looked
   * up by wavelength, so its exposures run as a contiguous segment followed by
   * its own calibrations.  The segments are then round-robined -- λ1, λ2, ... λN,
   * λ1, ... -- until every wavelength has met its goal, following the GMOS
   * wavelength-dither generator.  Running each wavelength to completion instead
   * would push the later ones into fresh visits (paying a full acquisition each
   * time) and would leave an interrupted program with nothing at all for the
   * wavelengths it never reached.
   */
  case class Generator(
    blocks:  NonEmptyVector[WavelengthBlock],
    builder: AtomBuilder[GnirsDynamicConfig]
  ) extends SequenceGenerator[GnirsDynamicConfig]:

    private val multi: Boolean = blocks.length > 1

    /**
     * Nominal on-sky time given to one wavelength before moving to the next.
     * The visit-length budget is shared out, so N wavelengths still break for a
     * telluric at the same cadence a single one would.  For N = 1 this is
     * exactly `MaxVisitLength`.
     */
    private val segmentBudget: TimeSpan =
      MaxVisitLength /| NonZeroInt.unsafeFrom(blocks.length)

    // Computes the atoms in one wavelength's segment, limited to `maxCycles`.
    // A "Nighttime Calibrations" atom (flat + arc) closes the segment and, when
    // the segment is long enough, appears around its midpoint aligned to a cycle
    // boundary.  Telluric sequences (`cals.isEmpty`) omit them entirely.
    private def atomsInSegment(
      b:         WavelengthBlock,
      maxCycles: NonNegInt
    ): (Int, List[ProtoAtom[ProtoStep[GnirsDynamicConfig]]]) =

      def cyclesIn(timeSpan: TimeSpan): Int =
        (timeSpan.toMicroseconds / b.cycleEstimate.toMicroseconds).toInt

      // `1 max` guarantees forward progress: once the visit budget is split N
      // ways a single cycle can be longer than one segment.  The cycle is still
      // bounded by MaxSciencePeriod, checked at instantiation.
      val cycles: Int = (1 max cyclesIn(segmentBudget)) min maxCycles.value

      val scienceTime: TimeSpan = b.cycleEstimate *| cycles

      val scienceAtom: ProtoAtom[ProtoStep[GnirsDynamicConfig]] =
        ProtoAtom(atomTitle(ScienceCycleTitle, b.steps.wavelength, multi).some, b.steps.scienceSteps)

      b.steps.cals.fold(cycles -> List.fill(cycles)(scienceAtom)): cals =>
        val gcalAtom: ProtoAtom[ProtoStep[GnirsDynamicConfig]] =
          ProtoAtom(atomTitle(NighttimeCalTitle, b.steps.wavelength, multi).some, cals)

        cycles ->
          Option
            .when(scienceTime >= MaxSciencePeriod)(scienceTime /| Two)
            .fold(
              // The science time is not long enough to warrant a mid-science cal in this segment.
              List.fill(cycles)(scienceAtom) ++ Option.when(cycles > 0)(gcalAtom).toList
            ): timeUntilMidScienceCals =>

              val fullPreCalCycles: Int        = cyclesIn(timeUntilMidScienceCals)
              val leftOverPreCalTime: TimeSpan = timeUntilMidScienceCals -| (b.cycleEstimate *| fullPreCalCycles)

              // If the nominal cal time falls in the middle of a science cycle, make it so
              // the break to do calibrations falls closest to a cycle boundary.
              val extraPreCalCycle: Int =
                if leftOverPreCalTime >= (b.cycleEstimate /| Two) then 1 min cycles else 0

              val preCalCycles:  Int = fullPreCalCycles + extraPreCalCycle
              val postCalCycles: Int = cycles - preCalCycles

              List.fill(preCalCycles)(scienceAtom).appended(gcalAtom) ++
              List.fill(postCalCycles)(scienceAtom)                   ++
              Option.when(postCalCycles > 0)(gcalAtom).toList

    override def generate: Stream[Pure, Atom[GnirsDynamicConfig]] =

      val n = blocks.length

      // Round-robin the wavelengths, skipping any that have met their goal, until
      // all have.  Atom ids come from the atom's index within a single builder,
      // so every segment must feed the same `buildStream` -- separate builders
      // would emit duplicate ids.
      val atoms: Stream[Pure, ProtoAtom[ProtoStep[GnirsDynamicConfig]]] =
        Stream
          .unfold((blocks.map(_.goalCycles.value).toVector, 0)): (remaining, pos) =>
            Option.when(remaining.exists(_ > 0)):
              val i = LazyList.from(0).map(k => (pos + k) % n).find(remaining(_) > 0).get
              val (used, as) = atomsInSegment(blocks.getUnsafe(i), NonNegInt.unsafeFrom(remaining(i)))

              // Sanity check....
              assert(used > 0, "No progress made generating future GNIRS Spectroscopy sequence!")

              (as, (remaining.updated(i, remaining(i) - used), (i + 1) % n))
          .flatMap(Stream.emits)

      builder.buildStream(atoms)

  private def definitionError(oid: Observation.Id, msg: String): OdbError =
    OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $msg".some)

  private def nm(sw: ScienceWavelength): String =
    f"${sw.centralWavelength.toNanometers.value.value.toDouble}%.0f nm"

  /**
   * Atom title for a block of steps taken at one central wavelength: bare when
   * the observation has a single wavelength (so existing sequences are
   * unchanged), suffixed with the wavelength when it has several, so the
   * observer can tell the segments apart.
   */
  private def atomTitle(base: NonEmptyString, sw: ScienceWavelength, multi: Boolean): NonEmptyString =
    if multi then NonEmptyString.unsafeFrom(s"${base.value} (${nm(sw)})") else base

  // "GNIRS Spectroscopy" rather than "Long Slit": this generator serves the IFU too.
  private def zeroExposureTime(oid: Observation.Id): OdbError =
    definitionError(oid, "GNIRS Spectroscopy requires a positive exposure time.")

  private def missingItcResult(oid: Observation.Id, sw: ScienceWavelength): OdbError =
    definitionError(oid, s"No ITC result for central wavelength ${nm(sw)}.")

  private def exposureTimeTooLong(oid: Observation.Id, sw: ScienceWavelength, estimate: TimeSpan): OdbError =
    definitionError(oid, s"Estimated science cycle time (${estimate.toMinutes} minutes) at ${nm(sw)} for $oid must be less than ${MaxSciencePeriod.toMinutes} minutes.")

  /**
   * Generates the sequence for a daytime pinhole flat calibration: a single
   * smart (day baseline) GCAL flat taken with the pinhole FPU and the science
   * grating / cross-disperser, used to trace the cross-dispersed spectral
   * orders. The exposure and lamp come from SmartGcal; the steps are DayCal
   * (and so do not count against the program's time).
   */
  private def daytimePinhole[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig],
    static:        GnirsStaticConfig,
    namespace:     UUID,
    expander:      SmartGcalExpander[F, GnirsStaticConfig, GnirsDynamicConfig],
    config:        Config
  ): F[Either[OdbError, SequenceGenerator[GnirsDynamicConfig]]] =

    // A pinhole flat traces the cross-dispersed order positions, which move with
    // the grating angle, so a single flat is only valid for one setting: take one
    // per distinct central wavelength.  These are DayCal and so cost no program
    // time.
    def flat(sw: ScienceWavelength): ProtoStep[GnirsDynamicConfig] =
      SeqState.eval:
        for
          _ <- State.modify[GnirsDynamicConfig]: dyn =>
                 dyn.copy(
                   coadds            = sw.coadds,
                   filter            = config.filter,
                   decker            = config.decker,
                   fpu               = GnirsFpu.Other(pinholeFpu(config.camera)),
                   acquisitionMirror = GnirsAcquisitionMirrorMode.Out(
                                         config.prism,
                                         config.grating,
                                         GnirsGratingWavelength(sw.centralWavelength)
                                       ),
                   camera            = config.camera,
                   focus             = config.focus,
                   readMode          = config.explicitReadMode.getOrElse(GnirsReadMode.Bright)
                 )
          f <- SeqState.flatStep(TelescopeConfig(Offset.Zero, StepGuideState.Disabled), ObserveClass.DayCal)
        yield f

    val distinctWavelengths: NonEmptyList[ScienceWavelength] =
      NonEmptyList.fromListUnsafe(config.wavelengths.toList.distinctBy(_.centralWavelength))

    val multi: Boolean = distinctWavelengths.length > 1

    distinctWavelengths
      .traverse: sw =>
        EitherT(expander.expandStep(static, flat(sw)))
          .map: steps =>
            ProtoAtom(atomTitle(DaytimePinholeTitle, sw, multi).some, steps)
      .bimap(
        m => definitionError(observationId, m),
        atoms =>
          val builder = AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science)
          new SequenceGenerator[GnirsDynamicConfig]:
            def generate: Stream[Pure, Atom[GnirsDynamicConfig]] =
              builder.buildStream(Stream.emits(atoms.toList))
      ).value

  def instantiate[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig],
    static:        GnirsStaticConfig,
    namespace:     UUID,
    expander:      SmartGcalExpander[F, GnirsStaticConfig, GnirsDynamicConfig],
    config:        Config,
    times:         Either[OdbError, NonEmptyMap[Wavelength, IntegrationTime]],
    calRole:       Option[CalibrationRole]
  ): F[Either[OdbError, SequenceGenerator[GnirsDynamicConfig]]] =
    calRole match
      case Some(CalibrationRole.DaytimePinhole) =>
        daytimePinhole(observationId, estimator, static, namespace, expander, config)
      case _ =>
        instantiateScience(observationId, estimator, static, namespace, expander, config, times, calRole)

  private def instantiateScience[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig],
    static:        GnirsStaticConfig,
    namespace:     UUID,
    expander:      SmartGcalExpander[F, GnirsStaticConfig, GnirsDynamicConfig],
    config:        Config,
    times:         Either[OdbError, NonEmptyMap[Wavelength, IntegrationTime]],
    calRole:       Option[CalibrationRole]
  ): F[Either[OdbError, SequenceGenerator[GnirsDynamicConfig]]] =

    // Pair each configured wavelength with its ITC result, in configuration
    // order.  A wavelength with no result is a programming error upstream, not
    // something to silently drop.
    val pairs: EitherT[F, OdbError, NonEmptyList[(ScienceWavelength, IntegrationTime)]] =
      EitherT.fromEither:
        times.flatMap: m =>
          config.wavelengths.traverse: sw =>
            m(sw.centralWavelength)
              .toRight(missingItcResult(observationId, sw))
              .flatMap: t =>
                Either.cond(t.exposureTime.toNonNegMicroseconds.value > 0, (sw, t), zeroExposureTime(observationId))

    // A science cycle must fit inside the calibration validity period at every
    // wavelength; the error names the offending one.
    def cycleEstimate(steps: StepDefinition): EitherT[F, OdbError, TimeSpan] =
      val estimate = StepTimeEstimateCalculator.runEmpty(estimator.estimateTotalNel(static, steps.scienceSteps))
      EitherT.fromEither:
        Either.cond(estimate < MaxSciencePeriod, estimate, exposureTimeTooLong(observationId, steps.wavelength, estimate))

    val gen = for
      ts <- pairs
      ds <- StepDefinition.computeAll(config, ts, static, expander, calRole).leftMap(m => definitionError(observationId, m))
      bs <- ds.zip(ts).traverse: (d, wt) =>
              for
                e <- cycleEstimate(d)
                c <- EitherT.fromEither(d.cycleCount(wt._2).leftMap(m => definitionError(observationId, m)))
              yield WavelengthBlock(d, e, c)
    yield Generator(
      bs.toNev,
      AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science)
    ): SequenceGenerator[GnirsDynamicConfig]

    gen.value
