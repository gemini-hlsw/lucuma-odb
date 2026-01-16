// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit

import cats.Comparison.*
import cats.Eq
import cats.Monad
import cats.Order.catsKernelOrderingForOrder
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.data.State
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.order.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState
import lucuma.core.enums.StepType
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.optics.syntax.lens.*
import lucuma.core.optics.syntax.optional.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.AtomRecord
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StepRecord
import lucuma.odb.sequence.util.AtomBuilder
import lucuma.odb.sequence.util.IndexTracker
import monocle.Lens

import java.util.UUID
import scala.collection.immutable.SortedMap


/**
 * GMOS Long Slit science sequence generation.  GMOS Long Slit divides up the
 * time in nominal one hour blocks where each block is associated with a
 * wavelength dither.  Each block is given a nighttime arc and a flat for that
 * configuration followed by as many science datasets as we can fit in an hour
 * of exposure time, distributed across as many selected offset positions as
 * possible.  Calibrations are considered still valid within a block if they are
 * no longer than 1.5 hours old, so there is extra time to complete the hour of
 * science if necessary.
 *
 * Each block is placed in its own atom, but of course that doesn't preclude
 * stopping early.  All science steps for which there are valid calibrations in
 * the block are counted toward completion, regardless of the order that the
 * calibrations and science steps appear in the execution sequence or whether
 * unrelated steps are executed in the atom.
 */
object Science:

  /**
   * Defines how long a calibration remains valid.  After this amount of time,
   * the calibration must be taken again.
   */
  val CalValidityPeriod: TimeSpan =
    90.minuteTimeSpan

  /**
   * The nominal amount of time to spend at one wavelength dither. This will
   * help determine how many exposures we hope to obtain while processing a
   * single "wavelength block".
   */
  val SciencePeriod: TimeSpan =
    1.hourTimeSpan

  /**
   * Exposure time for a twilight flat observation.
   */
  val TwilightExposureTime: TimeSpan =
    30.secondTimeSpan

  extension (t: Timestamp)
    def plusCalValidityPeriod: Timestamp =
      t +| CalValidityPeriod

    def minusCalValidityPeriod: Timestamp =
      t -| CalValidityPeriod

  private val Zero: NonNegInt = NonNegInt.MinValue

  // Lens from the step to its telescope offset in q.
  private def protoStepOffset[D]: Lens[ProtoStep[D], Offset] =
    ProtoStep.telescopeConfig andThen TelescopeConfig.offset

  private def protoStepQ[D]: Lens[ProtoStep[D], Offset.Q] =
    protoStepOffset andThen Offset.q

  private def stepRecordOffset[D]: Lens[StepRecord[D], Offset] =
    StepRecord.telescopeConfig[D] andThen TelescopeConfig.offset

  extension [D](p: ProtoStep[D])
    def q: Offset.Q =
      protoStepQ.get(p)

    def withZeroOffset: ProtoStep[D] =
      protoStepOffset.replace(Offset.Zero)(p)

  extension [D](r: StepRecord[D])
    def q: Offset.Q =
      stepRecordOffset.get(r).q

    def withZeroOffset: StepRecord[D] =
      stepRecordOffset.replace(Offset.Zero)(r)

  /**
   * Science exposure count goals for a given wavelength dither.
   *
   * @param Δλ          the wavelength dither itself
   * @param index       order in which this wavelength block is executed
   * @param requirement number of science steps per spatial offset at this
   *                    wavelength dither
   */
  case class Goal(
    Δλ:          WavelengthDither,
    index:       NonNegInt,
    requirement: Remaining[Offset.Q]
  ):
    def description: NonEmptyString =
      NonEmptyString.unsafeFrom(s"${Δλ.toNanometers.value} nm")

  object Goal:

    def compute(
      dithers:   List[WavelengthDither],
      offsets:   List[Offset.Q],
      expTimeμs: PosLong,
      expCount:  PosInt
    ): NonEmptyList[Goal] =

      def nel[A](as: List[A], default: A): NonEmptyList[A] =
        NonEmptyList.fromList(as).getOrElse(NonEmptyList.one(default))

      val Δλs     = nel(dithers, WavelengthDither.Zero)
      val ΔλCount = Δλs.size

      val sci            = SciencePeriod.toMicroseconds
      val time           = sci min expTimeμs.value
      val maxExpPerBlock = (sci / time).toInt

      // First figure out how many steps per wavelength dither to assign.

      val expCountPerDither =
        NonEmptyList.fromListUnsafe:
          if expCount.value <= (ΔλCount * maxExpPerBlock) then
            // Spread the exposures we have as much as possible over the dithers.
            val base  = expCount.value / ΔλCount
            val extra = expCount.value % ΔλCount
            List.tabulate(ΔλCount): Δλidx =>
              base + (if Δλidx < extra then 1 else 0)
          else
            // Try to fill as many blocks as possible
            val fullBlocks = expCount.value / maxExpPerBlock
            val base       = fullBlocks / ΔλCount * maxExpPerBlock
            List.tabulate(ΔλCount): Δλidx =>
              val extra = Δλidx.comparison(fullBlocks % ΔλCount) match
                case LessThan    => maxExpPerBlock
                case EqualTo     => expCount.value % maxExpPerBlock
                case GreaterThan => 0
              base + extra

      // Now spread the offsets around as well as possible across the dithers.

      val qs      = nel(offsets, Offset.Q.Zero)
      val qCount  = qs.size

      val runningSums =
        NonEmptyList.fromListUnsafe:
          expCountPerDither.toList.scanLeft(0)(_ + _).init

      Δλs
        .zip(expCountPerDither)
        .zip(runningSums)
        .zipWithIndex
        .map { case (((dither, n), sum), idx) =>
          val base   = n / qCount
          val extra  = n % qCount

          // Start with 'base' steps (i.e., exposures) for every offset
          val counts = Array.fill(qCount)(base)

          // Figure out where to put the extras.  We'd like to spread them
          // across dithers as evenly as we can.
          (0 until extra).foreach: j =>
            counts((j + sum) % qCount) += 1

          Goal(dither, NonNegInt.unsafeFrom(idx), Remaining.from(qs.toList.zip(counts)))
        }

  end Goal

  /**
   * A description of the steps (arcs, flats, and science) that are associated
   * with a wavelength goal.  The collections of steps is executed together in
   * a block, or "atom".  Nominally, each block will contain an arc, a flat,
   * and as many science datasets as we can fit in a single hour.
   *
   * @param goal wavelength dither and count of science datasets we seek for
   *             the observation as a whole
   * @param arcs typically a single arc calibration, but smart gcal sometimes
   *             prescribes multiple concrete arcs per smart arc
   * @param flats typically a single flat calibration, but smart gcal sometimes
   *              prescribes multiple concrete flats per smart flat
   * @param science an instance of the science step that should be repeated
   */
  final case class StepDefinition[D](
    goal:    Goal,
    arcs:    List[ProtoStep[D]], // all with offset 0
    flats:   List[ProtoStep[D]], // all with offset 0
    science: ProtoStep[D]        // all with offset 0
  ):
    val arcCounts:  Map[ProtoStep[D], NonNegInt] = StepDefinition.calCounts(arcs)
    val flatCounts: Map[ProtoStep[D], NonNegInt] = StepDefinition.calCounts(flats)
    val allCals: List[ProtoStep[D]] = arcs ++ flats
    val allCalsCounts: Map[ProtoStep[D], NonNegInt]  = arcCounts ++ flatCounts

  object StepDefinition:

    private def calCounts[D](cal: List[ProtoStep[D]]): Map[ProtoStep[D], NonNegInt] =
      cal.groupMapReduce(identity)(_ => 1)(_ + _)
        .view
        .mapValues(NonNegInt.unsafeFrom)
        .toMap

    sealed trait Computer[D, G, L, U] extends GmosSequenceState[D, G, L, U]:

      def setup(config: Config[G, L, U], time: TimeSpan): State[D, Unit] =
        for {
          _ <- optics.exposure    := time
          _ <- optics.grating     := (config.grating, GmosGratingOrder.One, config.centralWavelength).some
          _ <- optics.filter      := config.filter
          _ <- optics.fpu         := GmosFpuMask.builtin.reverseGet(config.fpu).some

          _ <- optics.xBin        := config.xBin
          _ <- optics.yBin        := config.yBin
          _ <- optics.ampReadMode := config.ampReadMode
          _ <- optics.ampGain     := config.ampGain

          _ <- optics.roi         := config.roi
        } yield ()

      /**
       * Creates the BlockDefinition, assuming we find smart gcal definitions
       * that match the `config`.
       *
       * @param expander  smart gcal expander for GMOS (north or south as
       *                  appropriate)
       * @param config    observation configuration
       * @param expTimeμs integration time for science datasets as prescribed
       *                  by ITC
       * @param expCount  total step count prescribed by the ITC
       * @param calRole   calibration role, which determines whether arcs and/or
       *                  flats are needed
       */
      def compute[F[_]: Monad](
        oid:       Observation.Id,
        expander:  SmartGcalExpander[F, D],
        config:    Config[G, L, U],
        expTimeμs: PosLong,
        expCount:  PosInt,
        calRole:   Option[CalibrationRole]
      ): F[Either[OdbError, NonEmptyList[StepDefinition[D]]]] =

        val λ            = config.centralWavelength
        val isTwilight   = calRole.contains(CalibrationRole.Twilight)
        val includeFlats = !isTwilight
        val includeArcs  = calRole.isEmpty
        val goals        = Goal.compute(config.wavelengthDithers, config.spatialOffsets, expTimeμs, expCount)

        def define(g: Goal): EitherT[F, OdbError, StepDefinition[D]] =
          val (smartArc, smartFlat, science) =
            eval:
              for
                _ <- setup(config, TimeSpan.unsafeFromMicroseconds(expTimeμs.value))
                _ <- optics.wavelength := λ.offset(g.Δλ).getOrElse(λ)
                a <- arcStep(TelescopeConfig(Offset.Zero, StepGuideState.Disabled), if isTwilight then ObserveClass.DayCal else ObserveClass.NightCal)
                f <- flatStep(TelescopeConfig(Offset.Zero, StepGuideState.Disabled), if isTwilight then ObserveClass.DayCal else ObserveClass.NightCal)
                s <- scienceStep(TelescopeConfig(Offset.Zero, StepGuideState.Enabled), if isTwilight then ObserveClass.DayCal else ObserveClass.Science)
              yield (a, f, s)

          val defn = for
            fs <- if includeFlats then EitherT(expander.expandStep(smartFlat)).map(_.toList) else EitherT.pure(List.empty)
            as <- if includeArcs then EitherT(expander.expandStep(smartArc)).map(_.toList) else EitherT.pure(List.empty)
          yield StepDefinition(g, as.toList, fs, science)

          defn.leftMap(s => OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $s".some))

        goals.traverse(define).value

    end Computer

    object North extends GmosNorthSequenceState
                    with Computer[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]

    object South extends GmosSouthSequenceState
                    with Computer[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]
  end StepDefinition

  /**
   * Dither collects information associated with generation of steps for a
   * single wavelength dither.
   */
  case class Dither[D](
    definition: StepDefinition[D],
    remaining:  Remaining[Offset.Q]
  ):
    /** The description of this block (as taken from the goal / adjustment). */
    def desc: NonEmptyString =
      definition.goal.description

    /** Decreases the remaining count for each offset in the map. */
    def complete(m: Map[Offset.Q, Int]): Dither[D] =
      copy(remaining = remaining.decrementAll(m))

    def matches(step: StepRecord[D])(using Eq[D]): Boolean =
      step.isScienceSequence         && {
        val step0 = step.withZeroOffset  // ignore offset when considering a match
        step0.stepConfig.stepType match
          case StepType.Bias |
               StepType.Dark |
               StepType.SmartGcal => false
          case StepType.Gcal      => definition.arcs.exists(_.matches(step0)) ||
                                     definition.flats.exists(_.matches(step0))
          case StepType.Science   => definition.science.matches(step0)
      }

    /**
     * Generates a full wavelength block.
     */
    def generateFullBlock(blockSize: PosInt): (Dither[D], List[ProtoStep[D]]) =
      // When we generate a step, it is for a particular offset which must be
      // set.  In the step definition, cals and science datasets are stored
      // with zero offsets.
      def setQ(steps: List[(ProtoStep[D], Offset.Q)]): List[ProtoStep[D]] =
        steps.map((step, q) => protoStepQ.replace(q)(step))

      val (qs, remainingʹ) = remaining.take(blockSize.value)
      (
        copy(remaining = remainingʹ),
         qs.headOption match
           case None    => List.empty[ProtoStep[D]]
           case Some(q) => setQ(definition.allCals.tupleRight(q) ++ List.fill(qs.size)(definition.science).zip(qs))
      )

  end Dither

  object Dither:
    def init[D](definition: StepDefinition[D]): Dither[D] =
      Dither(definition, definition.goal.requirement)


  /**
   * The RecordWindow represents a subset (typically equal to the entire set) of
   * recorded steps for a block.  The time range covered by the steps is always
   * less than or equal to the calibration validity period.  We break the entire
   * collection of steps for a block into windows and use them to find datasets
   * which have all their required calibrations and to compute which
   * calibrations are missing otherwise.
   *
   * Usually there will be one window per block, but we should be able to handle
   * the case where datasets are added after long delays.
   */
  trait RecordWindow[D]:
    def missingCalCounts: Map[ProtoStep[D], NonNegInt]
    def missingCals: List[ProtoStep[D]]
    def pendingScience: Map[Step.Id, Offset.Q]
    def calibratedScience: Map[Step.Id, Offset.Q]

  object RecordWindow:
    def apply[D](
      definition: StepDefinition[D],
      steps:      SortedMap[Timestamp, StepRecord[D]]
    ): RecordWindow[D] =
      new RecordWindow[D]:
        // How many of each calibration are missing from the window
        lazy val missingCalCounts: Map[ProtoStep[D], NonNegInt] =
          steps
            .values
            .filter(s => s.successfullyCompleted && s.isGcal)
            .foldLeft(definition.allCalsCounts): (cals, step) =>
              // Count a calibration regardless of its offset.
              cals.updatedWith(step.protoStep.withZeroOffset)(_.flatMap(n => NonNegInt.unapply(n.value - 1)))

        // List, in order, of missing calibrations for this window.
        lazy val missingCals: List[ProtoStep[D]] =
          // Filter the ordered list of arcs + flats, removing still valid ones
          definition
            .allCals
            .foldLeft((List.empty[ProtoStep[D]], missingCalCounts)):
              case ((res, remaining), calStep) =>
                NonNegInt
                  .unapply(remaining.getOrElse(calStep, Zero).value - 1)
                  .fold((res, remaining)): n =>
                    (calStep :: res, remaining.updated(calStep, n))
            ._1
            .reverse

        // Science datasets that are completed.
        lazy val pendingScience: Map[Step.Id, Offset.Q] =
          steps
            .values
            .collect:
              case s if s.successfullyCompleted && s.isScience => (s.id, s.q)
            .toMap

        // Science datasets that have calibrations
        lazy val calibratedScience: Map[Step.Id, Offset.Q] =
          if missingCalCounts.values.exists(_.value > 0) then Map.empty
          else pendingScience

  end RecordWindow

  /**
   * DitherRecord tracks completion data for a given dither position, and all
   * the associated steps that have been executed.
   */
  case class DitherRecord[D](
    dither: Dither[D],
    steps:  SortedMap[Timestamp, StepRecord[D]],
  ):
    /** Time of the first step in this block, if any. */
    val startTime: Option[Timestamp] =
      steps.headOption.map(_._2.created)

    /** Time of the last step in this block, if any. */
    val endTime: Option[Timestamp] =
      steps.lastOption.map(_._2.created)

    /** Interval from start to end of this block. */
    val interval: Option[TimestampInterval] =
      (startTime, endTime).mapN(TimestampInterval.between)

    /** All the (cal validity time) windows included in this record. */
    def windows: Stream[Pure, RecordWindow[D]] =
      endTime.fold(Stream.empty): last =>
        Stream
          .emits[Pure, Timestamp](steps.keys.toList)
          .fproduct(_.plusCalValidityPeriod min last)
          .takeThrough((_, e) => e < last)
          .map: (s, e) =>
            RecordWindow(dither.definition, steps.rangeFrom(s).rangeTo(e))

    /**
     * A window which ends at the specified time but begins one cal validity
     * period earlier.
     */
    def windowEndingAt(timestamp: Timestamp): RecordWindow[D] =
      RecordWindow(
        dither.definition,
        steps.rangeFrom(timestamp.minusCalValidityPeriod).rangeTo(timestamp)
      )

    /**
     * All the calibrated science steps in this record.  We compute this window
     * by window.  In one window the science step may be missing cals but in
     * another it may have them.
     */
    lazy val calibratedScience: Map[Step.Id, Offset.Q] =
      windows
        .fold(Map.empty[Step.Id, Offset.Q])((m, w) => m ++ w.calibratedScience)
        .compile
        .toList
        .head

    /**
     * Marks the end of the record lifetime, books the calibrated science steps
     * and resets.
     */
    def settle: DitherRecord[D] =
      if steps.isEmpty then this
      else copy(
        dither = dither.complete(calibratedScience.toList.groupMapReduce(_._2)(_ => 1)(_ + _)),
        steps  = SortedMap.empty
      )

    /** Records the next step. */
    def record(step: StepRecord[D])(using Eq[D]): DitherRecord[D] =
      if !dither.matches(step) then this
      else copy(steps = steps + (step.created -> step))

    /**
     * How much time is left in the current iteration of this block at time
     * `timestamp`.
     */
    def remainingTimeAt(timestamp: Timestamp): TimeSpan =
      val limit = startTime.map(_.plusCalValidityPeriod).getOrElse(Timestamp.Min)
      val start = endTime.map(_ max timestamp).getOrElse(Timestamp.Max)
      Option.when(start <= limit)(TimeSpan.between(start, limit)).getOrElse(TimeSpan.Zero)

    /**
     * Generates the remaining steps for this block as if requested at the given
     * timestamp.  The answer will differ depending on the timestamp because
     * calibrations expire and have to be repeated.
     *
     * @param timestamp reference time for the computation
     * @param static static config of the observation (required for step time
     *               estimates)
     * @param estimator step time estimator
     * @param lastSteps state of the previous steps required for an accurate
     *                  estimation
     * @param blockSize caps the number of steps that may be produced
     * @tparam S static configration type
     * @return a `Dither` updated to account for the steps that were generated
     *         and the list of steps themselves
     */
    def generateBlockRemainder[S](
      timestamp: Timestamp,
      static:    S,
      estimator: TimeEstimateCalculator[S, D],
      lastSteps: TimeEstimateCalculator.Last[D],
      blockSize: PosInt
    ): (Dither[D], List[ProtoStep[D]]) =

      // What calibrations are missing in the last window?
      val window      = windowEndingAt(timestamp)
      val missingCals = window.missingCals

      // Which pending science in the last window can be completed by adding
      // missing calibrations? Previous windows may have uncalibrated science,
      // but those would be lost since we won't get the calibration in time.
      val uncalibratedScience =
        if missingCals.isEmpty then Map.empty[Step.Id, Offset.Q]
        else window.pendingScience -- calibratedScience.keys

      // How many, at a maximum, ignoring time, science steps could you do to
      // finish out the block?
      val currentCount = (calibratedScience ++ uncalibratedScience).size
      val maxRemaining = ((dither.remaining.total.value min blockSize.value) - currentCount) max 0

      // N.B., for simplicity sake, we'll proceed as if there were no offset
      // costs. They are small (~7 secs) and infrequent and the time limits are
      // not hard and fast anyway.

      // How long do we have left to fill with science?  Adjust lastSteps as
      // though one or more science steps have just happened.  If there were an
      // offset cost, it would be swamped by the science fold move anyway.
      val lastStepsʹ    = lastSteps.next(dither.definition.science)
      val calTime       = estimator.estimateTotal(static, missingCals).runA(lastStepsʹ).value
      val remainingTime = remainingTimeAt(timestamp) -| calTime

      // How long would the first science step take?  It may be different from
      // remaining steps if there is a science fold move to make.
      val firstStepTime = estimator.estimateOne(static, dither.definition.science).runA(lastSteps).value.total

      // First account for the science that will be added by what has been
      // observed so far.
      val ditherʹ = dither.complete((calibratedScience ++ uncalibratedScience)
                          .toList
                          .groupMapReduce(_._2)(_ => 1)(_ + _))

      if remainingTime < firstStepTime then
        // No time left for more science.  If there are some calibrations we
        // could add to save the last science though, add them.
        val lastCals = if uncalibratedScience.sizeIs == 0 then Nil
                       else missingCals.map(protoStepQ.replace(lastSteps.offset.q))
        (ditherʹ, lastCals)
      else
        // Okay, there's time for at least one more step.
        val remainingTimeʹ = remainingTime -| firstStepTime

        // How long would each subsequent science step take? This could be less
        // than the first step time because there's no need to move the science
        // fold.
        val otherStepTime  = estimator.estimateStep(static, lastStepsʹ, dither.definition.science).total

        // How many new science steps should we add then?  Do not go over the
        // max for the block or for the observation as a whole.
        val otherStepCount = (remainingTimeʹ.toMicroseconds / otherStepTime.toMicroseconds).toInt
        val newCount       = maxRemaining min (1 + otherStepCount)

        val (qs, remainingʹ) = ditherʹ.remaining.take(newCount)

        val lastQ = if currentCount === 0 then qs.headOption // use the first science offset pos
                    else lastSteps.offset.q.some // use the last step's offset position

        // Remaining.take will have no context about the past but we want to
        // avoid extra offsetting.  So, move any block of consecutive qs that
        // match the last spatial offset to the front.
        val qsʹ = lastQ.fold(qs): q =>
          val (initial, last) = qs.partition(_ === q)
          initial ++ last

        // Set the spatial offset in the science steps that we produce.
        val scienceSteps   =
          List
            .fill(newCount)(dither.definition.science)
            .zip(qsʹ)
            .map((s, q) => protoStepQ.replace(q)(s))

        // Order the steps according to whether we've just done any science.
        // We want to avoid switching the science fold more than necessary.
        val steps =
          if currentCount == 0 then
            val q = lastQ.getOrElse(Offset.Zero.q)
            missingCals.map(protoStepQ.replace(q)) ++ scienceSteps
          else
            val q = qsʹ.lastOption.orElse(lastQ).getOrElse(Offset.Zero.q)
            scienceSteps ++ missingCals.map(protoStepQ.replace(q))

        // Update the `Dither` will a remaining from which `newCount` steps
        // have been removed.
        (dither.copy(remaining = remainingʹ), steps)
      end if

    end generateBlockRemainder

  end DitherRecord

  object DitherRecord:
    def init[D](definition: StepDefinition[D]): DitherRecord[D] =
      DitherRecord(Dither.init(definition), SortedMap.empty)

  /**
   * The science generator keeps up with an ordered list of block records, and
   * works to compute the number of valid science steps have been obtained for
   * each block as step records are added.  When all step records are accounted
   * for, we ask for the remaining steps to fill out the block and follow it up
   * with all remaining steps.
   */
  private case class ScienceGenerator[S, D](
    estimator:   TimeEstimateCalculator[S, D],
    static:      S,
    lastSteps:   TimeEstimateCalculator.Last[D],
    atomBuilder: AtomBuilder[D],
    expMicroSec: PosLong,
    expCount:    PosInt,
    records:     NonEmptyVector[DitherRecord[D]],
    tracker:     IndexTracker,
    pos:         Int
  ) extends SequenceGenerator.Base[D]:

    val sci            = SciencePeriod.toMicroseconds
    val time           = sci min expMicroSec.value
    val maxExpPerBlock = PosInt.unsafeFrom((sci / time).toInt)

    val length: Int = records.length

    override def generate(timestamp: Timestamp): Stream[Pure, Atom[D]] =
      val (aix, six) = tracker.toTuple

      // The first atom will have any unfinished steps from the current atom, so
      // it is handled separately.
      val rec         = records.getUnsafe(pos)
      val dither      = rec.dither
      val (dʹ, steps) = if rec.steps.isEmpty then dither.generateFullBlock(maxExpPerBlock)
                        else rec.generateBlockRemainder(timestamp, static, estimator, lastSteps, maxExpPerBlock)
      val (cs, atom0) = atomBuilder.buildOption(dither.desc.some, aix, six, steps).run(lastSteps).value
      val dithers     = records.map(_.dither).updatedUnsafe(pos, dʹ)

      Stream
        .iterate((pos + 1) % length)(pos => (pos + 1) % length)
        .mapAccumulate((dithers, aix + 1, cs)) { case ((ds, aix, cs), pos) =>
          val dither      = ds.getUnsafe(pos)
          val (dʹ, steps) = dither.generateFullBlock(maxExpPerBlock)
          val (csʹ, atom) = atomBuilder.buildOption(dither.desc.some, aix, 0, steps).run(cs).value
          val dsʹ         = ds.updatedUnsafe(pos, dʹ)
          ((dsʹ, aix + 1, csʹ), atom)
        }
        .cons1((dithers, 0, TimeEstimateCalculator.Last.empty), atom0) // put the first atom back
        .takeThrough { case ((ds, _, _), _) => ds.foldMap(_.remaining.total.value) > 0 }
        .collect { case (_, Some(atom)) => atom }
    end generate

    // Advances the block index we're working on, setting it to the first block
    // from 'start' that matches the step.
    private def advancePos(start: Int, step: StepRecord[D])(using Eq[D]): Int =
      Stream
        .iterate(start%length)(p => (p + 1) % length)
        .take(length)
        .dropWhile(p => !records.getUnsafe(p).dither.matches(step))
        .head
        .compile
        .toList
        .headOption
        .getOrElse(pos)

    override def recordAtom(atom: AtomRecord): SequenceGenerator[D] =
      copy(
        records = records.map(_.settle),
        tracker = tracker.reset(atom)
      )

    override def recordStep(step: StepRecord[D])(using Eq[D]): SequenceGenerator[D] =
      if step.isAcquisitionSequence then
        this
      else
        val trackerʹ = tracker.record(step)

        // Advance the block we're focusing upon, if necessary.  This will happen
        // (potentially) for the first step recorded, or when a new atom is started
        val (recordsʹ, posʹ) =
          tracker match
            case IndexTracker.Reset(_) =>
              (records, advancePos(pos, step))
            case _                     =>
              if tracker.atomCount === trackerʹ.atomCount then (records, pos)
              else (records.map(_.settle), advancePos(pos+1, step))

        val recordsʹʹ =
          step.stepConfig.stepType match
            case StepType.Bias | StepType.Dark | StepType.SmartGcal =>
              // GMOS Longslit doesn't use biases or darks, and smart gcal has
              // been expanded so ignore these.
              recordsʹ
            case StepType.Gcal | StepType.Science                   =>
              // Record the step at the current index, settle all others
              recordsʹ.zipWithIndex.map: (rec, idx) =>
                if posʹ === idx then rec.record(step) else rec.settle

        copy(
          lastSteps = lastSteps.next(step.protoStep),
          records   = recordsʹʹ,
          tracker   = trackerʹ,
          pos       = posʹ
        )

    end recordStep

  end ScienceGenerator

  private object ScienceGenerator:

    private def calibrationObservationConfig[G, L, U](
      c: Config[G, L, U]
    ): Config[G, L, U] =
      val limit   = c.coverage.toPicometers.value.value / 10.0
      val dithers = if c.wavelengthDithers.exists(_.toPicometers.value.abs > limit) then c.wavelengthDithers
                    else List(WavelengthDither.Zero)
      (for {
        _ <- Config.explicitWavelengthDithers := dithers.some
        _ <- Config.explicitSpatialOffsets    := List(Offset.Q.Zero).some
      } yield ()).runS(c).value

    def instantiate[F[_]: Monad, S, D, G, L, U](
      oid:       Observation.Id,
      estimator: TimeEstimateCalculator[S, D],
      static:    S,
      namespace: UUID,
      expander:  SmartGcalExpander[F, D],
      stepDef:   StepDefinition.Computer[D, G, L, U],
      config:    Config[G, L, U],
      time:      Either[OdbError, IntegrationTime],
      calRole:   Option[CalibrationRole]
    ): F[Either[OdbError, SequenceGenerator[D]]] =

      def sequenceUnavailable(m: String): OdbError =
        OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $m".some)

      def extractTime: Either[OdbError, (PosLong, PosInt)] =
        time.flatMap: t =>
          PosLong
            .from(t.exposureTime.toNonNegMicroseconds.value)
            .bimap(
              _  => sequenceUnavailable(s"GMOS Long Slit science requires a positive exposure time."),
              μs => (μs, t.exposureCount)
            )

      // Adjust the config and integration time according to the calibration role.
      val configAndTime = calRole match
        case None                                     =>
          extractTime.tupleLeft(config)

        case Some(CalibrationRole.SpectroPhotometric) =>
          val configʹ = calibrationObservationConfig(config)
          extractTime.map: (μs, _) =>
            (configʹ, (μs, PosInt.unsafeFrom(configʹ.wavelengthDithers.length.max(1))))

        case Some(CalibrationRole.Twilight)           =>
          val configʹ = calibrationObservationConfig(config)
          val μs      = PosLong.unsafeFrom(TwilightExposureTime.toMicroseconds)
          val n       = PosInt.unsafeFrom(configʹ.wavelengthDithers.length.max(1))
          (configʹ, (μs, n)).asRight[OdbError]

        case Some(c)                                  =>
          sequenceUnavailable(s"GMOS Long Slit ${c.tag} not implemented").asLeft

      // If exposure time is longer than the science period, there will never be
      // time enough to do any science steps.
      val configAndTimeʹ = configAndTime.filterOrElse(
        _._2._1.value <= SciencePeriod.toMicroseconds,
        sequenceUnavailable(s"Exposure times over ${SciencePeriod.toMinutes} minutes are not supported.")
      )

      // Compute the generator
      val result = for
        (configʹ, (μs, n)) <- EitherT.fromEither[F](configAndTimeʹ)
        defs               <- EitherT(stepDef.compute(oid, expander, configʹ, μs, n, calRole))
      yield ScienceGenerator(
        estimator,
        static,
        TimeEstimateCalculator.Last.empty[D],
        AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science),
        μs,
        n,
        defs.map(DitherRecord.init).toNev,
        IndexTracker.Zero,
        0
      )

      result.widen[SequenceGenerator[D]].value
    end instantiate

  end ScienceGenerator

  def gmosNorth[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     TimeEstimateCalculator[StaticConfig.GmosNorth, GmosNorth],
    static:        StaticConfig.GmosNorth,
    namespace:     UUID,
    expander:      SmartGcalExpander[F, GmosNorth],
    config:        Config.GmosNorth,
    time:          Either[OdbError, IntegrationTime],
    calRole:       Option[CalibrationRole]
  ): F[Either[OdbError, SequenceGenerator[GmosNorth]]] =
    ScienceGenerator.instantiate(observationId, estimator, static, namespace, expander, StepDefinition.North, config, time, calRole)

  def gmosSouth[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     TimeEstimateCalculator[StaticConfig.GmosSouth, GmosSouth],
    static:        StaticConfig.GmosSouth,
    namespace:     UUID,
    expander:      SmartGcalExpander[F, GmosSouth],
    config:        Config.GmosSouth,
    time:          Either[OdbError, IntegrationTime],
    calRole:       Option[CalibrationRole]
  ): F[Either[OdbError, SequenceGenerator[GmosSouth]]] =
    ScienceGenerator.instantiate(observationId, estimator, static, namespace, expander, StepDefinition.South, config, time, calRole)
