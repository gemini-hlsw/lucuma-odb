// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package flamingos2
package longslit

import cats.Eq
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
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState.Disabled
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.ExecutionEvent.SequenceEvent
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
import lucuma.core.util.TimestampInterval
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.AtomRecord
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StepRecord
import lucuma.odb.sequence.data.VisitRecord
import lucuma.odb.sequence.util.AtomBuilder
import lucuma.odb.sequence.util.IndexTracker

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

  // Combine two Option[TimestampInterval] into one that incorporates both.
  private val span: (Option[TimestampInterval], Option[TimestampInterval]) => Option[TimestampInterval] =
    case (None, None)         => None
    case (Some(t), None)      => t.some
    case (None, Some(t))      => t.some
    case (Some(t0), Some(t1)) => t0.span(t1).some

  // There are two kinds of atoms in F2 Long Slit.  The ABBA science cycle and
  // gcal calibrations.  There is an AtomTracker for each type.  They are state
  // machines which keep up with which part of these atoms has been seen,
  // whether it is complete and what would be remaining in order to complete it
  // from the current state.
  sealed trait AtomTracker:
    def title: NonEmptyString
    def interval: Option[TimestampInterval] = none
    def isComplete: Boolean                 = false
    def next(steps: StepDefinition, record: StepRecord[F2]): AtomTracker
    def remaining(steps: StepDefinition): List[ProtoStep[F2]]

  object AtomTracker:
    sealed trait Abba extends AtomTracker:
      def title: NonEmptyString = AbbaCycleTitle

    object Abba:
      case object A0 extends Abba:
        override def next(steps: StepDefinition, record: StepRecord[F2]): AtomTracker =
          if steps.a0.matches(record) && record.successfullyCompleted then B0(record.interval)
          else this

        override def remaining(steps: StepDefinition): List[ProtoStep[F2]] =
          List(steps.a0, steps.b0, steps.b1, steps.a1)

      case class B0(override val interval: Option[TimestampInterval]) extends Abba:
        override def next(steps: StepDefinition, record: StepRecord[F2]): AtomTracker =
          if steps.b0.matches(record) then
            if record.successfullyCompleted then B1(span(interval, record.interval)) else this
          else A0.next(steps, record)

        override def remaining(steps: StepDefinition): List[ProtoStep[F2]] =
          List(steps.b0, steps.b1, steps.a1)

      case class B1(override val interval: Option[TimestampInterval]) extends Abba:
        override def next(steps: StepDefinition, record: StepRecord[F2]): AtomTracker =
          if steps.b1.matches(record) then
            if record.successfullyCompleted then A1(span(interval, record.interval)) else this
          else A0.next(steps, record)

        override def remaining(steps: StepDefinition): List[ProtoStep[F2]] =
          List(steps.b1, steps.a1)

      case class A1(override val interval: Option[TimestampInterval]) extends Abba:
        override def next(steps: StepDefinition, record: StepRecord[F2]): AtomTracker =
          if steps.a1.matches(record) then
            if record.successfullyCompleted then End(span(interval, record.interval)) else this
          else A0.next(steps, record)

        override def remaining(steps: StepDefinition): List[ProtoStep[F2]] =
          List(steps.a1)

      case class End(override val interval: Option[TimestampInterval]) extends Abba:
        override def next(steps: StepDefinition, record: StepRecord[F2]): AtomTracker = this
        override def remaining(steps: StepDefinition): List[ProtoStep[F2]] = Nil
        override def isComplete: Boolean = true

    end Abba

    case class Calibrations(override val interval: Option[TimestampInterval], expected: List[ProtoStep[F2]]) extends AtomTracker:
      def title: NonEmptyString = NighttimeCalTitle

      override def next(steps: StepDefinition, record: StepRecord[F2]): AtomTracker =
        Calibrations(span(interval, record.interval), expected.removeFirstBy(record)(_.matches(_)))

      override def remaining(steps: StepDefinition): List[ProtoStep[F2]] =
        expected

      override val isComplete: Boolean =
        expected.isEmpty

    end Calibrations

    object Calibrations:
      def apply(steps: StepDefinition): Calibrations =
        Calibrations(none, steps.cals.toList)

  end AtomTracker

  // Computes the atoms remaining in a 3 hour science block, starting at `when`
  // assuming the block itself started at `start` and that there are `pending`
  // science cycles that need calibration.  The result is limited to `maxCycles`
  // at most.
  private def remainingAtomsInBlock(
    steps:         StepDefinition,
    when:          Timestamp,
    start:         Option[Timestamp],          // block start time
    pending:       Option[TimestampInterval],  // ABBA science time pending calibration
    cycleEstimate: TimeSpan,
    maxCycles:     NonNegInt
  ): (Int, List[ProtoAtom[ProtoStep[F2]]]) =

    // Adjust the requested reference time to ensure that it does not come
    // before the recorded times for this block.
    val whenʹ = start.fold(when)(_ max when)
    val now   = pending.map(_.end).fold(whenʹ)(_ max whenʹ)

    // How many full ABBA cycles would fit in the given time span?
    def cyclesIn(timeSpan: TimeSpan): Int =
      (timeSpan.toMicroseconds / cycleEstimate.toMicroseconds).toInt

    // Roughly, how many more cycles can we fit in the remaining time?
    val startʹ: Timestamp   = start.orElse(pending.map(_.start)).getOrElse(now)
    val end: Timestamp      = startʹ +| MaxVisitLength
    def remaining: TimeSpan = now.timeUntil(end)
    val cycles: Int         = cyclesIn(remaining) min maxCycles.value

    // Remaining science time in this visit.
    val remainingScience: TimeSpan = cycleEstimate *| cycles

    // Anticipated science time in all, including science we've done already
    // but for which there haven't been calibrations.
    val scienceTime: TimeSpan = remainingScience +| pending.map(_.timeSpan).getOrElse(TimeSpan.Zero)

    // If this is over 1.5 hours we need to insert a flat roughly at:
    //   science-start + science-time / 2
    val scienceStart: Timestamp = pending.map(_.start).getOrElse(now)

    val abbaAtom: ProtoAtom[ProtoStep[F2]] = ProtoAtom(AbbaCycleTitle.some, steps.abbaCycle)
    val gcalAtom: ProtoAtom[ProtoStep[F2]] = ProtoAtom(NighttimeCalTitle.some, steps.cals)

    cycles ->
      Option
        .when(scienceTime >= MaxSciencePeriod)(scienceStart +| (scienceTime /| Two))
        .fold(
          // The science time is not long enough to warrant a mid-science cal in this block.
          List.fill(cycles)(abbaAtom) ++ Option.when(cycles > 0 || pending.isDefined)(gcalAtom).toList
        ): nominalMidScienceCalTime =>

          // How many more full cycles can we do before the mid-science cal time?
          val timeUntilMidScienceCals: TimeSpan = now.timeUntil(nominalMidScienceCalTime)
          val fullPreCalCycles: Int             = cyclesIn(timeUntilMidScienceCals)
          val leftOverPreCalTime: TimeSpan      = timeUntilMidScienceCals -| (cycleEstimate *| fullPreCalCycles)

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

  // Calculates the atoms in a 3 hour science block, limited to at most `maxCycles`.
  def remainingAtomsInEmptyBlock(
    steps:         StepDefinition,
    cycleEstimate: TimeSpan,
    maxCycles:     PosInt
  ): (Int, List[ProtoAtom[ProtoStep[F2]]]) =
    remainingAtomsInBlock(
      steps,
      Timestamp.unsafeFromInstant(java.time.Instant.ofEpochMilli(0)),
      none,
      none,
      cycleEstimate,
      NonNegInt.unsafeFrom(maxCycles.value)
    )

  case class SequenceRecord(
    steps:           StepDefinition,
    current:         AtomTracker               = AtomTracker.Abba.A0,
    block:           Option[TimestampInterval] = none,
    pending:         Option[TimestampInterval] = none,
    stop:            Boolean                   = false,
    completedCycles: Int                       = 0
  ):

    private def recordCompletion: SequenceRecord =
      if !current.isComplete then this
      else current match
        case abba: AtomTracker.Abba =>
          copy(
            block           = span(block, abba.interval),
            pending         = span(pending, abba.interval),
            completedCycles = completedCycles + 1
          )
        case AtomTracker.Calibrations(interval, _)   =>
          copy(
            block   = span(block, interval),
            pending = none
          )

    def next(step: StepRecord[F2]): SequenceRecord =
      def newAbbaCycle: SequenceRecord =
        copy(current = AtomTracker.Abba.A0).next(step)

      def newGcalSet: SequenceRecord =
        copy(current = AtomTracker.Calibrations(steps)).next(step)

      def continue: SequenceRecord =
        copy(
          current = current.next(steps, step),
          block   = span(block, step.interval)
        ).recordCompletion

      current match
        case abba: AtomTracker.Abba =>
          if step.isScience then
            if current.isComplete then newAbbaCycle else continue
          else if step.isGcal then newGcalSet else this

        case AtomTracker.Calibrations(_, _) =>
          if step.isGcal then
            if current.isComplete then newGcalSet else continue
          else if step.isScience then newAbbaCycle else this

    def endBlockEarly: SequenceRecord =
      copy(stop = true)

    def resetVisit: SequenceRecord =
      copy(
        current = AtomTracker.Abba.A0,
        block   = none,
        pending = none,
        stop    = false
      )

  end SequenceRecord

  case class Generator(
    steps:         StepDefinition,
    cycleEstimate: TimeSpan,
    seqRecord:     SequenceRecord,
    estimate:      (NonEmptyList[ProtoStep[F2]], TimeEstimateCalculator.Last[F2]) => TimeSpan,
    calcState:     TimeEstimateCalculator.Last[F2],
    indices:       IndexTracker,
    builder:       AtomBuilder[F2],
    goalCycles:    NonNegInt
  ) extends SequenceGenerator[F2]:

    override def recordAtom(atom: AtomRecord): SequenceGenerator[F2] =
      copy(
        indices = indices.reset(atom)
      )

    override def recordStep(step: StepRecord[F2])(using Eq[F2]): SequenceGenerator[F2] =
      step.sequenceType match
        case SequenceType.Acquisition => copy(
          calcState = calcState.next(step.protoStep),
        )
        case SequenceType.Science     => copy(
          seqRecord = seqRecord.next(step),
          calcState = calcState.next(step.protoStep),
          indices   = indices.record(step)
        )

    override def recordVisit(visit: VisitRecord): SequenceGenerator[F2] =
      copy(seqRecord = seqRecord.resetVisit)

    override def recordSequenceEvent(event: SequenceEvent): SequenceGenerator[F2] =
      event.command match
        case SequenceCommand.Stop => copy(seqRecord = seqRecord.endBlockEarly)
        case _                    => this

    override def generate: Stream[Pure, Atom[F2]] =

      // Add future blocks until we've completed all the cycles.
      val future =
        Stream
          .unfold(0): c =>
            PosInt.from(goalCycles.value - c).toOption.map: remainingCycles =>
              val (cycles, atoms) = remainingAtomsInEmptyBlock(steps, cycleEstimate, remainingCycles)

              // Sanity check....
              assert(cycles > 0, "No progress made generating future F2 Long Slit sequence!")

              (atoms, c + cycles)
          .flatMap(Stream.emits)

      // Convert the proto atoms into real atoms with ids and estimates.
      val (aix, six) = indices.toTuple
      future
        .mapAccumulate((aix, six, calcState)) { case ((a, s, calcStateʹ), protoAtom) =>
          val (csʹ, atom) = builder.build(protoAtom.description, a, s, protoAtom.steps).run(calcStateʹ).value
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
      SequenceRecord(s),
      (nel, calcState) => estimator.estimateTotalNel(static, nel).runA(calcState).value,
      TimeEstimateCalculator.Last.empty[F2],
      IndexTracker.Zero,
      AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science),
      c
    ): SequenceGenerator[F2]

    gen.value