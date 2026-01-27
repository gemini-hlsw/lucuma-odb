// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package flamingos2
package longslit

import cats.Monad
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.order.*
import eu.timepit.refined.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState.Disabled
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig as F2
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.optics.syntax.lens.*
import lucuma.core.refined.numeric.NonZeroInt
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.util.AtomBuilder

import java.util.UUID

/**
 * Flamingos 2 long slit science sequence generation.
 */
object Science:

  /**
   * The name of the ABBA cycle atoms.
   */
  val AbbaCycleTitle: NonEmptyString = NonEmptyString.unsafeFrom("ABBA Cycle")

  /**
   * The name of the nighttime cal atoms.
   */
  val NighttimeCalTitle: NonEmptyString = NonEmptyString.unsafeFrom("Nighttime Calibrations")

  /**
   * A visit shouldn't take more than this, since we need to break to do a
   * telluric.
   */
  val MaxVisitLength: TimeSpan =
    3.hourTimeSpan

  /**
   * Maximum time that may pass between flats.
   */
  val MaxSciencePeriod: TimeSpan =
    90.minuteTimeSpan

  /**
   * Slit length.  Offsets larger than +/- 54 arcsec are off slit and unguided.
   */
  val SlitLength: Angle =
    Angle.fromBigDecimalArcseconds(108.0)

  private def isOnSlit(o: Offset): Boolean =
    (o.p.toAngle.toMicroarcseconds === 0) &&
    (Angle.signedDecimalArcseconds.get(o.q.toAngle).abs <= (Angle.signedDecimalArcseconds.get(SlitLength) / 2))

  private val Two: NonZeroInt = NonZeroInt.unsafeFrom(2)

  extension (start: Timestamp)
    def timeUntil(end: Timestamp): TimeSpan =
      if start < end then TimeSpan.between(start, end) else TimeSpan.Zero

  extension [A, B](lst: List[A])
    def removeFirstBy(b: B)(f: (A, B) => Boolean): List[A] =
      @annotation.tailrec
      def loop(rem: List[A], acc: List[A]): List[A] =
        rem match
          case Nil    => lst
          case h :: t => if f(h, b) then acc.reverse ++ t else loop(t, h :: acc)

      loop(lst, Nil)

  case class StepDefinition(
    a0:   ProtoStep[F2],
    b0:   ProtoStep[F2],
    b1:   ProtoStep[F2],
    a1:   ProtoStep[F2],
    cals: NonEmptyList[ProtoStep[F2]]
  ):
    /** ABBA science cycle. */
    def abbaCycle: NonEmptyList[ProtoStep[F2]] =
      NonEmptyList.of(a0, b0, b1, a1)

    def cycleCount(t: IntegrationTime): Either[String, NonNegInt] =
      val requiredExposures = t.exposureCount.value
      val exposuresPerCycle = abbaCycle.toList.count(s => isOnSlit(s.telescopeConfig.offset))
      Either.cond(
        exposuresPerCycle > 0,
        // Round up to a whole cycle even if it produces extra data.
        NonNegInt.unsafeFrom((requiredExposures + (exposuresPerCycle - 1)) / exposuresPerCycle),
        "At least one exposure must be taken on slit."
      )

  object StepDefinition extends SequenceState[F2] with Flamingos2InitialDynamicConfig:

    def f2ScienceStep(o: Offset): State[F2, ProtoStep[F2]] =
      val guideState = if isOnSlit(o) then Enabled else Disabled
      scienceStep(TelescopeConfig(o, guideState), ObserveClass.Science)

    // PreDef is a StepDefinition before SmartGcal expansion.
    case class PreDef(
      a0:   ProtoStep[F2],
      b0:   ProtoStep[F2],
      b1:   ProtoStep[F2],
      a1:   ProtoStep[F2],
      flat: ProtoStep[F2],  // Unexpanded SmartGcal Flat
      arc:  ProtoStep[F2]   // Unexpanded SmartGcal Arc
    ):
      def expand[F[_]: Monad](
        expander: SmartGcalExpander[F, F2]
      ): EitherT[F, String, StepDefinition] =

        // Flamingos2 needs read mode and reads to be consistent with exposure time
        def adjustReadMode(f2: ProtoStep[F2]): ProtoStep[F2] =
          val mode = Flamingos2ReadMode.forExposureTime(f2.value.exposure)
          f2.copy(value = f2.value.copy(readMode = mode, reads = mode.readCount))

        for
          fs <- EitherT(expander.expandStep(flat))
          rs <- EitherT(expander.expandStep(arc))
        yield StepDefinition(a0, b0, b1, a1, fs.map(adjustReadMode) ::: rs.map(adjustReadMode))

    object PreDef:

      def apply(
         config: Config,
         time:   IntegrationTime,
         a0Off:  Offset,
         b0Off:  Offset,
         b1Off:  Offset,
         a1Off:  Offset
      ): PreDef =

        val readMode =
          config
            .explicitReadMode
            .getOrElse:
              Flamingos2ReadMode.forExposureTime(time.exposureTime)

        eval:
          for
            _  <- F2.exposure    := time.exposureTime
            _  <- F2.disperser   := config.disperser.some
            _  <- F2.filter      := config.filter
            _  <- F2.readMode    := readMode
            _  <- F2.lyotWheel   := Flamingos2LyotWheel.F16
            _  <- F2.fpu         := Flamingos2FpuMask.builtin(config.fpu)
            _  <- F2.decker      := config.decker
            _  <- F2.readoutMode := config.readoutMode
            _  <- F2.reads       := config.explicitReads.getOrElse(readMode.readCount)
            a0 <- f2ScienceStep(a0Off)
            b0 <- f2ScienceStep(b0Off)
            b1 <- f2ScienceStep(b1Off)
            a1 <- f2ScienceStep(a1Off)
            f  <- flatStep(a1.telescopeConfig.copy(guiding = Disabled), ObserveClass.NightCal)
            r  <- arcStep(a1.telescopeConfig.copy(guiding = Disabled), ObserveClass.NightCal)
          yield PreDef(a0, b0, b1, a1, f, r)

    def compute[F[_]: Monad](
      config:    Config,
      time:      IntegrationTime,
      expander:  SmartGcalExpander[F, F2]
    ): EitherT[F, String, StepDefinition] =
      for
        p <- EitherT.fromEither:
               config.offsets match
                 case List(a0, b0, b1, a1) => PreDef(config, time, a0, b0, b1, a1).asRight
                 // This case should be caught when validating arguments to the mode
                 // construction / update.  Nevertheless, we'll guarantee it here.
                 case _                    => s"Exactly 4 offset positions are needed for Flamingos 2 Long Slit (${config.offsets.size} provided).".asLeft
        d <- p.expand(expander)
      yield d

  end StepDefinition

  case class Generator(
    steps:         StepDefinition,
    cycleEstimate: TimeSpan,
    estimate:      (NonEmptyList[ProtoStep[F2]], TimeEstimateCalculator.Last[F2]) => TimeSpan,
    builder:       AtomBuilder[F2],
    goalCycles:    NonNegInt
  ) extends SequenceGenerator[F2]:

    // Computes the atoms remaining in a 3 hour science blocklimited to `maxCycles`
    // at most.
    private def atomsInBlock(maxCycles: NonNegInt): (Int, List[ProtoAtom[ProtoStep[F2]]]) =

      // How many full ABBA cycles would fit in the given time span?
      def cyclesIn(timeSpan: TimeSpan): Int =
        (timeSpan.toMicroseconds / cycleEstimate.toMicroseconds).toInt

      // Roughly, how many more cycles can we fit in the remaining time?
      val cycles: Int = cyclesIn(MaxVisitLength) min maxCycles.value

      // Remaining science time in this visit. If this is over 1.5 hours we need
      // to insert a flat roughly after science-time / 2.
      val scienceTime: TimeSpan = cycleEstimate *| cycles

      val abbaAtom: ProtoAtom[ProtoStep[F2]] = ProtoAtom(AbbaCycleTitle.some, steps.abbaCycle)
      val gcalAtom: ProtoAtom[ProtoStep[F2]] = ProtoAtom(NighttimeCalTitle.some, steps.cals)

      cycles ->
        Option
          .when(scienceTime >= MaxSciencePeriod)(scienceTime /| Two)
          .fold(
            // The science time is not long enough to warrant a mid-science cal in this block.
            List.fill(cycles)(abbaAtom) ++ Option.when(cycles > 0)(gcalAtom).toList
          ): timeUntilMidScienceCals =>

            // How many more full cycles can we do before the mid-science cal time?
            val fullPreCalCycles: Int        = cyclesIn(timeUntilMidScienceCals)
            val leftOverPreCalTime: TimeSpan = timeUntilMidScienceCals -| (cycleEstimate *| fullPreCalCycles)

            // If the nominal cal time falls in the middle of an ABBA cycle, make it so
            // the break to do calibrations falls closest to an ABBA cycle boundary.
            val extraPreCalCycle: Int =
              if leftOverPreCalTime >= (cycleEstimate /| Two) then 1 min cycles else 0

            // How many new ABBA cycles before and after the mid-science cals
            val preCalCycles:  Int = fullPreCalCycles + extraPreCalCycle
            val postCalCycles: Int = cycles - preCalCycles

            List.fill(preCalCycles)(abbaAtom).appended(gcalAtom) ++
            List.fill(postCalCycles)(abbaAtom)                   ++
            Option.when(postCalCycles > 0)(gcalAtom).toList

    override def generate: Stream[Pure, Atom[F2]] =

      // Add future blocks until we've completed all the cycles.
      val future =
        Stream
          .unfold(0): c =>
            PosInt.from(goalCycles.value - c).toOption.map: remainingCycles =>
              val (cycles, atoms) = atomsInBlock(NonNegInt.unsafeFrom(remainingCycles.value))

              // Sanity check....
              assert(cycles > 0, "No progress made generating future F2 Long Slit sequence!")

              (atoms, c + cycles)
          .flatMap(Stream.emits)

      // Convert the proto atoms into real atoms with ids and estimates.
      future
        .mapAccumulate((0, 0, TimeEstimateCalculator.Last.empty[F2])) { case ((a, s, calcState), protoAtom) =>
          val (csʹ, atom) = builder.build(protoAtom.description, a, s, protoAtom.steps).run(calcState).value
          ((a + 1, 0, csʹ), atom)
        }
        .map(_._2)

  end Generator

  private def definitionError(oid: Observation.Id, msg: String): OdbError =
     OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $msg".some)

  private def zeroExposureTime(oid: Observation.Id): OdbError =
    definitionError(oid, "Flamingos 2 Long Slit requires a positive exposure time.")

  private def exposureTimeTooLong(oid: Observation.Id, estimate: TimeSpan): OdbError =
    definitionError(oid, s"Estimated ABBA cycle time (${estimate.toMinutes} minutes) for $oid must be less than ${MaxSciencePeriod.toMinutes} minutes.")

  def instantiate[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     TimeEstimateCalculator[Flamingos2StaticConfig, F2],
    static:        Flamingos2StaticConfig,
    namespace:     UUID,
    expander:      SmartGcalExpander[F, F2],
    config:        Config,
    time:          Either[OdbError, IntegrationTime]
  ): F[Either[OdbError, SequenceGenerator[F2]]] =

    val posTime: EitherT[F, OdbError, IntegrationTime] =
      EitherT.fromEither:
        time.filterOrElse(_.exposureTime.toNonNegMicroseconds.value > 0, zeroExposureTime(observationId))

    def cycleEstimate(steps: StepDefinition): EitherT[F, OdbError, TimeSpan] =
      val estimate = TimeEstimateCalculator.runEmpty(estimator.estimateTotalNel(static, steps.abbaCycle))
      EitherT.fromEither:
        Either.cond(estimate < MaxSciencePeriod, estimate, exposureTimeTooLong(observationId, estimate))

    val gen = for
      t <- posTime
      s <- StepDefinition.compute(config, t, expander).leftMap(m => definitionError(observationId, m))
      e <- cycleEstimate(s)
      c <- EitherT.fromEither(s.cycleCount(t).leftMap(m => definitionError(observationId, m)))
    yield Generator(
      s,
      e,
      (nel, calcState) => estimator.estimateTotalNel(static, nel).runA(calcState).value,
      AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science),
      c
    ): SequenceGenerator[F2]

    gen.value