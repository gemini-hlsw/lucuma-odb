// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gnirs
package longslit

import cats.Monad
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.data.State
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
import lucuma.core.enums.StepGuideState.Disabled
import lucuma.core.math.Offset
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

  case class StepDefinition(
    scienceSteps: NonEmptyList[ProtoStep[GnirsDynamicConfig]],
    // Inline nighttime calibrations (flat + arc).  Empty for telluric sequences.
    cals:         Option[NonEmptyList[ProtoStep[GnirsDynamicConfig]]]
  ):
    /**
     * Cycle count: round up so that we always deliver at least the requested
     * number of exposures. Sky-only offsets in the cycle (if any) are not
     * given special treatment — we cycle blindly through whatever the
     * observing mode defines.
     */
    def cycleCount(t: IntegrationTime): NonNegInt =
      val required = t.exposureCount.value
      val perCycle = scienceSteps.length
      NonNegInt.unsafeFrom((required + (perCycle - 1)) / perCycle)

  object StepDefinition:

    // PreDef is a StepDefinition before SmartGcal expansion.
    case class PreDef(
      scienceSteps: NonEmptyList[ProtoStep[GnirsDynamicConfig]],
      // Unexpanded SmartGcal (flat, arc), absent for telluric sequences.
      cals:         Option[(ProtoStep[GnirsDynamicConfig], ProtoStep[GnirsDynamicConfig])]
    ):
      def expand[F[_]: Monad](
        static:   GnirsStaticConfig,
        expander: SmartGcalExpander[F, GnirsStaticConfig, GnirsDynamicConfig]
      ): EitherT[F, String, StepDefinition] =
        cals.fold(EitherT.pure(StepDefinition(scienceSteps, none))): (flat, arc) =>
          for
            fs <- EitherT(expander.expandStep(static, flat))
            rs <- EitherT(expander.expandStep(static, arc))
          yield StepDefinition(scienceSteps, (fs ::: rs).some)

    object PreDef:

      def apply(
        config:  Config,
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
          GnirsGratingWavelength(config.centralWavelength)
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
                      coadds            = config.coadds,
                      filter            = config.filter,
                      decker            = config.decker,
                      fpu               = GnirsFpu.Slit(config.fpu),
                      acquisitionMirror = acqMirror,
                      camera            = config.camera,
                      focus             = config.focus,
                      readMode          = resolvedReadMode
                    )
            ss <- config.telescopeConfigs.telescopeConfigs.traverse(SeqState.scienceStep(_, sciClass))
            ct  = ss.last.telescopeConfig.copy(guiding = Disabled)
            f  <- SeqState.flatStep(ct, ObserveClass.NightCal)
            r  <- SeqState.arcStep(ct, ObserveClass.NightCal)
          yield PreDef(ss, Option.when(includeCals)((f, r)))

    def compute[F[_]: Monad](
      config:   Config,
      time:     IntegrationTime,
      static:   GnirsStaticConfig,
      expander: SmartGcalExpander[F, GnirsStaticConfig, GnirsDynamicConfig],
      calRole:  Option[CalibrationRole]
    ): EitherT[F, String, StepDefinition] =
      PreDef(config, time, calRole).expand(static, expander)

  end StepDefinition

  case class Generator(
    steps:         StepDefinition,
    cycleEstimate: TimeSpan,
    builder:       AtomBuilder[GnirsDynamicConfig],
    goalCycles:    NonNegInt
  ) extends SequenceGenerator[GnirsDynamicConfig]:

    // Computes the atoms remaining in a 3 hour science block, limited to
    // `maxCycles` at most.  A "Nighttime Calibrations" atom (flat + arc) is
    // inserted at the end of the block and, when the block is long enough,
    // around its midpoint, aligned to a science cycle boundary.  Telluric
    // sequences (`steps.cals.isEmpty`) omit the calibration atoms entirely.
    private def atomsInBlock(maxCycles: NonNegInt): (Int, List[ProtoAtom[ProtoStep[GnirsDynamicConfig]]]) =

      def cyclesIn(timeSpan: TimeSpan): Int =
        (timeSpan.toMicroseconds / cycleEstimate.toMicroseconds).toInt

      val cycles: Int = cyclesIn(MaxVisitLength) min maxCycles.value

      val scienceTime: TimeSpan = cycleEstimate *| cycles

      val scienceAtom: ProtoAtom[ProtoStep[GnirsDynamicConfig]] = ProtoAtom(ScienceCycleTitle.some, steps.scienceSteps)

      steps.cals.fold(cycles -> List.fill(cycles)(scienceAtom)): cals =>
        val gcalAtom: ProtoAtom[ProtoStep[GnirsDynamicConfig]] = ProtoAtom(NighttimeCalTitle.some, cals)

        cycles ->
          Option
            .when(scienceTime >= MaxSciencePeriod)(scienceTime /| Two)
            .fold(
              // The science time is not long enough to warrant a mid-science cal in this block.
              List.fill(cycles)(scienceAtom) ++ Option.when(cycles > 0)(gcalAtom).toList
            ): timeUntilMidScienceCals =>

              val fullPreCalCycles: Int        = cyclesIn(timeUntilMidScienceCals)
              val leftOverPreCalTime: TimeSpan = timeUntilMidScienceCals -| (cycleEstimate *| fullPreCalCycles)

              // If the nominal cal time falls in the middle of a science cycle, make it so
              // the break to do calibrations falls closest to a cycle boundary.
              val extraPreCalCycle: Int =
                if leftOverPreCalTime >= (cycleEstimate /| Two) then 1 min cycles else 0

              val preCalCycles:  Int = fullPreCalCycles + extraPreCalCycle
              val postCalCycles: Int = cycles - preCalCycles

              List.fill(preCalCycles)(scienceAtom).appended(gcalAtom) ++
              List.fill(postCalCycles)(scienceAtom)                   ++
              Option.when(postCalCycles > 0)(gcalAtom).toList

    override def generate: Stream[Pure, Atom[GnirsDynamicConfig]] =

      // Add future blocks until we've completed all the cycles.
      val atoms: List[ProtoAtom[ProtoStep[GnirsDynamicConfig]]] =
        Stream
          .unfold(0): c =>
            Option.when(goalCycles.value - c > 0):
              val (cycles, atoms) = atomsInBlock(NonNegInt.unsafeFrom(goalCycles.value - c))

              // Sanity check....
              assert(cycles > 0, "No progress made generating future GNIRS Long Slit sequence!")

              (atoms, c + cycles)
          .flatMap(Stream.emits)
          .toList

      builder.buildStream(Stream.emits(atoms))

  private def definitionError(oid: Observation.Id, msg: String): OdbError =
    OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $msg".some)

  private def zeroExposureTime(oid: Observation.Id): OdbError =
    definitionError(oid, "GNIRS Long Slit requires a positive exposure time.")

  private def exposureTimeTooLong(oid: Observation.Id, estimate: TimeSpan): OdbError =
    definitionError(oid, s"Estimated science cycle time (${estimate.toMinutes} minutes) for $oid must be less than ${MaxSciencePeriod.toMinutes} minutes.")

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

    val flat: ProtoStep[GnirsDynamicConfig] =
      SeqState.eval:
        for
          _ <- State.modify[GnirsDynamicConfig]: dyn =>
                 dyn.copy(
                   coadds            = config.coadds,
                   filter            = config.filter,
                   decker            = config.decker,
                   fpu               = GnirsFpu.Other(pinholeFpu(config.camera)),
                   acquisitionMirror = GnirsAcquisitionMirrorMode.Out(
                                         config.prism,
                                         config.grating,
                                         GnirsGratingWavelength(config.centralWavelength)
                                       ),
                   camera            = config.camera,
                   focus             = config.focus,
                   readMode          = config.explicitReadMode.getOrElse(GnirsReadMode.Bright)
                 )
          f <- SeqState.flatStep(TelescopeConfig(Offset.Zero, Disabled), ObserveClass.DayCal)
        yield f

    EitherT(expander.expandStep(static, flat))
      .bimap(
        m => definitionError(observationId, m),
        steps =>
          val atom    = ProtoAtom(DaytimePinholeTitle.some, steps)
          val builder = AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science)
          new SequenceGenerator[GnirsDynamicConfig]:
            def generate: Stream[Pure, Atom[GnirsDynamicConfig]] =
              builder.buildStream(Stream.emit(atom))
      ).value

  def instantiate[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig],
    static:        GnirsStaticConfig,
    namespace:     UUID,
    expander:      SmartGcalExpander[F, GnirsStaticConfig, GnirsDynamicConfig],
    config:        Config,
    time:          Either[OdbError, IntegrationTime],
    calRole:       Option[CalibrationRole]
  ): F[Either[OdbError, SequenceGenerator[GnirsDynamicConfig]]] =
    calRole match
      case Some(CalibrationRole.DaytimePinhole) =>
        daytimePinhole(observationId, estimator, static, namespace, expander, config)
      case _ =>
        instantiateScience(observationId, estimator, static, namespace, expander, config, time, calRole)

  private def instantiateScience[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig],
    static:        GnirsStaticConfig,
    namespace:     UUID,
    expander:      SmartGcalExpander[F, GnirsStaticConfig, GnirsDynamicConfig],
    config:        Config,
    time:          Either[OdbError, IntegrationTime],
    calRole:       Option[CalibrationRole]
  ): F[Either[OdbError, SequenceGenerator[GnirsDynamicConfig]]] =

    val posTime: EitherT[F, OdbError, IntegrationTime] =
      EitherT.fromEither:
        time.filterOrElse(_.exposureTime.toNonNegMicroseconds.value > 0, zeroExposureTime(observationId))

    def cycleEstimate(steps: StepDefinition): EitherT[F, OdbError, TimeSpan] =
      val estimate = StepTimeEstimateCalculator.runEmpty(estimator.estimateTotalNel(static, steps.scienceSteps))
      EitherT.fromEither:
        Either.cond(estimate < MaxSciencePeriod, estimate, exposureTimeTooLong(observationId, estimate))

    val gen = for
      t <- posTime
      s <- StepDefinition.compute(config, t, static, expander, calRole).leftMap(m => definitionError(observationId, m))
      e <- cycleEstimate(s)
    yield Generator(
      s,
      e,
      AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science),
      s.cycleCount(t)
    ): SequenceGenerator[GnirsDynamicConfig]

    gen.value
