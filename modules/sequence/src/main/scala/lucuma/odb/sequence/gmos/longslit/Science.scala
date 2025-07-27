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
import lucuma.core.math.Angle
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
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StepRecord
import lucuma.odb.sequence.util.AtomBuilder
import lucuma.odb.sequence.util.IndexTracker

import java.util.UUID
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

/**
 * GMOS Long Slit science sequence generation.  GMOS Long Slit divides up the
 * time in nominal one hour blocks where each block is associated with a
 * wavelength dither and a spatial offset.  Each block is given a nighttime arc
 * and a flat for that configuration followed by as many science datasets as we
 * can fit in an hour of exposure time. Calibrations are considered still valid
 * within a block if they are no longer than 1.5 hours old, so there is extra
 * time to complete the hour of science if necessary.
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
   * The nominal amount of time to spend at one (wavelength dither, spatial
   * offset) pair.  This will help determine how many exposures we hope to
   * obtain while processing a single "wavelength block".
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

  private val Zero: NonNegInt = NonNegInt.unsafeFrom(0)

  /**
   * Captures an adjustment to the fixed instrument and telescope position that
   * otherwise remains constant throughout a GMOS longslit observation.
   * (dither) and a small spatial offset (in q).
   *
   * @param Δλ small wavelength adjustment (dither)
   * @param q small spatial offset in q
   */
  case class Adjustment(Δλ: WavelengthDither, q: Offset.Q):
    def description: NonEmptyString =
      NonEmptyString.unsafeFrom(
        s"${Δλ.toNanometers.value} nm, ${Angle.signedDecimalArcseconds.get(q.toAngle)}″"
      )

  object Adjustment:

    /**
     * Computes the adjustments by zipping a repeating sequence of dithers with
     * a repeating sequence of offsets and taking the first n instances where
     * n is the least common multiple of their sizes. For example if dithers
     * is {0 nm, 5 nm) and offsets is (0", 15", -15") then there will be six
     * adjustments, lcm(2, 3): {(0 nm, 0"), (5 nm, 15"), (0 nm, -15"), (5 nm, 0"),
     * (0 nm, 15"), (5 nm, -15")}.
     */
    def compute(
      wavelengthDithers: List[WavelengthDither],
      spatialOffsets:    List[Offset.Q]
    ): NonEmptyList[Adjustment] =
      @tailrec def gcd(a: Long, b: Long): Long = if (b === 0) a else gcd(b, a%b)
      def lcm(as: Long*): Long = as.reduce { (a, b) => a/gcd(a,b)*b }

      val Δλs  = wavelengthDithers match
        case Nil => Stream(WavelengthDither.Zero)
        case ws  => Stream.emits(ws)

      val qs   = spatialOffsets match
        case Nil => Stream(Offset.Q.Zero)
        case os  => Stream.emits(os)

      // We default the wavelength dither to { 0 nm } and the spatial offset to
      // { 0" } if none are supplied so the resulting list must be at least one
      // element long.
      NonEmptyList.fromListUnsafe(
        Δλs
          .repeat
          .zip(qs.repeat)
          .take(lcm(wavelengthDithers.size max 1, spatialOffsets.size max 1))
          .map(Adjustment(_, _))
          .toList
      )
  end Adjustment

  /**
   * Science exposure count goals for a given adjustment.
   *
   * @param adjustment the wavelength dither and spatial offset combination
   * @param perBlockExposures how many science datasets per full block of time
   *                          at dither/offset combination
   * @param totalExposures total number of science datasets at this dither/offset
   *                       combination
   */
  case class Goal(
    adjustment:        Adjustment,
    perBlockExposures: NonNegInt,
    totalExposures:    NonNegInt
  )

  object Goal:

    def compute(
      wavelengthDithers: List[WavelengthDither],
      spatialOffsets:    List[Offset.Q],
      integration:       IntegrationTime
    ): NonEmptyList[Goal] =
      val adjs = Adjustment.compute(wavelengthDithers, spatialOffsets)
      val size = adjs.size

      val sci  = SciencePeriod.toMicroseconds
      val time = sci min integration.exposureTime.toNonNegMicroseconds.value

      val maxExpPerBlock = (sci / time).toInt
      val exposureCount  = integration.exposureCount.value

      // The calculation of the number of exposures per block differs depending
      // upon whether we have enough to fill a nominal block of exposures at
      // every adjustment.  If not, we spread the exposures we do have as much
      // as possible over the adjustments.  If so, we don't try to make it even
      // but instead try to fill as many blocks as possible.

      if exposureCount <= (size * maxExpPerBlock) then
        val perBlock = exposureCount / size
        val extra    = exposureCount % size
        adjs.zipWithIndex.map: (adj, idx) =>
          val expCount = NonNegInt.unsafeFrom(perBlock + (if idx < extra then 1 else 0))
          Goal(adj, expCount, expCount)
      else
        val fullBlocks = exposureCount / maxExpPerBlock
        val base       = fullBlocks / size * maxExpPerBlock
        adjs.zipWithIndex.map: (adj, idx) =>
          val extra = idx.toLong.comparison(fullBlocks % size) match
            case GreaterThan => 0
            case EqualTo     => exposureCount % maxExpPerBlock // left over
            case LessThan    => maxExpPerBlock
          Goal(adj, NonNegInt.unsafeFrom(maxExpPerBlock), NonNegInt.unsafeFrom(base + extra))
      end if

  end Goal

  /**
   * A description of the steps (arcs, flats, and science) that are associated
   * with an excution Goal.  The collections of steps is executed together in a
   * block, or "atom".  Nominally, each block will contain an arc, a flat, and
   * as many science datasets as we can fit in a single hour.
   *
   * @param goal wavelength dither, spatial offset and count of science datasets
   *             we seek per block and for the observation as a whole
   * @param arcs typically a single arc calibration, but smart gcal sometimes
   *             prescribes multiple concrete arcs per smart arc
   * @param flats typically a single flat calibration, but smart gcal sometimes
   *              prescribes multiple concrete flats per smart flat
   * @param science an instance of the science step that should be repeated
   */
  case class BlockDefinition[D](
    goal:    Goal,
    arcs:    List[ProtoStep[D]],
    flats:   List[ProtoStep[D]],
    science: ProtoStep[D]
  ):
    val arcCounts:  Map[ProtoStep[D], NonNegInt] = BlockDefinition.calCounts(arcs)
    val flatCounts: Map[ProtoStep[D], NonNegInt] = BlockDefinition.calCounts(flats)
    val allCals: List[ProtoStep[D]] = arcs ++ flats
    val allCalsCounts: Map[ProtoStep[D], NonNegInt]  = arcCounts ++ flatCounts

    def matches(step: StepRecord[D])(using Eq[D]): Boolean =
      step.isScienceSequence && (
        step.stepConfig.stepType match
          case StepType.Bias |
               StepType.Dark |
               StepType.SmartGcal => false
          case StepType.Gcal      => arcs.exists(_.matches(step)) || flats.exists(_.matches(step))
          case StepType.Science   => science.matches(step)
      )

  end BlockDefinition

  object BlockDefinition:

    private def calCounts[D](cal: List[ProtoStep[D]]): Map[ProtoStep[D], NonNegInt] =
      cal.groupMapReduce(identity)(_ => 1)(_ + _)
        .view
        .mapValues(NonNegInt.unsafeFrom)
        .toMap

    sealed trait Computer[D, G, L, U] extends GmosSequenceState[D, G, L, U]:

      def setup(config: Config[G, L, U], time: IntegrationTime): State[D, Unit] =
        for {
          _ <- optics.exposure    := time.exposureTime
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
       * @param expander smart gcal expander for GMOS (north or south as
       *                 appropriate)
       * @param config observation configuration
       * @param time integration time for science datasets as prescribed by ITC
       * @param includeArcs whether to include arcs at all (spec phot
       *                    calibrations do not have arcs)
       */
      def compute[F[_]: Monad](
        oid:      Observation.Id,
        expander: SmartGcalExpander[F, D],
        config:   Config[G, L, U],
        time:     IntegrationTime,
        calRole:  Option[CalibrationRole]
      ): F[Either[OdbError, NonEmptyList[BlockDefinition[D]]]] =
        Goal.compute(config.wavelengthDithers, config.spatialOffsets, time).traverse { g =>
          val isTwilight = calRole.contains(CalibrationRole.Twilight)

          val λ = config.centralWavelength
          val (smartArc, smartFlat, science) = eval {
            for {
              _ <- setup(config, time)
              _ <- optics.wavelength := λ.offset(g.adjustment.Δλ).getOrElse(λ)
              o  = Offset(Offset.P.Zero, g.adjustment.q)
              a <- arcStep(TelescopeConfig(o, StepGuideState.Disabled), if isTwilight then ObserveClass.DayCal else ObserveClass.NightCal)
              f <- flatStep(TelescopeConfig(o, StepGuideState.Disabled), if isTwilight then ObserveClass.DayCal else ObserveClass.NightCal)
              s <- scienceStep(TelescopeConfig(o, StepGuideState.Enabled), if isTwilight then ObserveClass.DayCal else ObserveClass.Science)
            } yield (a, f, s)
          }

          val includeFlats = !isTwilight
          val includeArcs  = calRole.isEmpty

          (for {
            fs <- if includeFlats then EitherT(expander.expandStep(smartFlat)).map(_.toList) else EitherT.pure(List.empty)
            as <- if includeArcs then EitherT(expander.expandStep(smartArc)).map(_.toList) else EitherT.pure(List.empty)
          } yield BlockDefinition(g, as.toList, fs, science)).value
        }
        .map(_.traverse(_.leftMap(s => OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $s".some))))

      end compute
    end Computer

    object North extends GmosNorthSequenceState
                    with Computer[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]

    object South extends GmosSouthSequenceState
                    with Computer[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]
  end BlockDefinition

  /**
   * BlockCompletion is used to track the count of completed science datasets
   * per block.
   */
  case class BlockCompletion[D](
    definition: BlockDefinition[D],
    completed:  NonNegInt
  ):
    /** The description of this block (as taken from the goal / adjustment). */
    def desc: NonEmptyString =
      definition.goal.adjustment.description

    /** Increases the completed count by the given amount. */
    def add(n: NonNegInt): BlockCompletion[D] =
      copy(completed = NonNegInt.unsafeFrom(completed.value + n.value))

    /** How many science datasets are remaining in the observation as a whole. */
    def remainingInObs: NonNegInt =
      NonNegInt.unsafeFrom((definition.goal.totalExposures.value - completed.value) max 0)

    /**
     * How many science datasets can we do in the current block if it has the
     * given number of pending science steps executed with valid calibrations
     * but not yet added to the completed count.  This value will never be more
     * than the total remainingInObs.
     */
    def remainingInBlock(pending: Int): NonNegInt =
      NonNegInt.unsafeFrom(
        ((remainingInObs.value min definition.goal.perBlockExposures.value) - pending) max 0
      )

    /**
     * Generates a full wavelength block.
     */
    def generate: (NonNegInt, List[ProtoStep[D]]) =
      val cnt = remainingInBlock(0)
      (cnt,
       cnt.value match
         case 0 => List.empty[ProtoStep[D]]
         case n => definition.allCals ++ List.fill(n)(definition.science)
      )

  end BlockCompletion

  object BlockCompletion:
    def init[D](definition: BlockDefinition[D]): BlockCompletion[D] =
      BlockCompletion(definition, Zero)

  /**
   * The BlockWindow represents a subset (typically equal to the entire set) of
   * recorded steps for a block.  The time range covered by the steps is always
   * less than or equal to the calibration validity period.  We break the entire
   * collection of steps for a block into windows and use them to find datasets
   * which have all their required calibrations and to compute which
   * calibrations are missing otherwise.
   *
   * Usually there will be one window per block, but we should be able to handle
   * the case where datasets are added after long delays.
   */
  trait BlockWindow[D]:
    def missingCalCounts: Map[ProtoStep[D], NonNegInt]
    def missingCals: List[ProtoStep[D]]
    def pendingScience: Set[Step.Id]
    def calibratedScience: Set[Step.Id]
    def calibratedScienceCount: NonNegInt =
      NonNegInt.unsafeFrom(calibratedScience.size)

  object BlockWindow:
    def apply[D](
      block: BlockDefinition[D],
      steps: SortedMap[Timestamp, StepRecord[D]]
    ): BlockWindow[D] =
      new BlockWindow[D]:
        // How many of each calibration are missing from the window
        lazy val missingCalCounts: Map[ProtoStep[D], NonNegInt] =
          steps
            .values
            .filter(s => s.successfullyCompleted && s.isGcal)
            .foldLeft(block.allCalsCounts) { (cals, step) =>
              cals.updatedWith(step.protoStep)(_.flatMap(n => NonNegInt.unapply(n.value - 1)))
            }

        // List, in order, of missing calibrations for this window.
        lazy val missingCals: List[ProtoStep[D]] =
          // Filter the ordered list of arcs + flats, removing still valid ones
          block.allCals.foldLeft((List.empty[ProtoStep[D]], missingCalCounts)) {
            case ((res, remaining), calStep) =>
              NonNegInt
                .unapply(remaining.getOrElse(calStep, Zero).value - 1)
                .fold((res, remaining)) { n =>
                  (calStep :: res, remaining.updated(calStep, n))
                }
          }._1.reverse

        // The step ids of all science datasets that are completed.
        lazy val pendingScience: Set[Step.Id] =
          steps.values.collect {
            case s if s.successfullyCompleted && s.isScience => s.id
          }.toSet

        // The step ids for science datasets that have calibrations
        lazy val calibratedScience: Set[Step.Id] =
          if missingCalCounts.values.exists(_.value > 0) then Set.empty
          else pendingScience

  end BlockWindow

  /**
   * BlockRecord tracks block definition, completion data, and all the
   * associated steps that have been executed.
   */
  case class BlockRecord[D](
    block: BlockCompletion[D],
    steps: SortedMap[Timestamp, StepRecord[D]],
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
    def windows: Stream[Pure, BlockWindow[D]] =
      endTime.fold(Stream.empty) { last =>
        Stream
          .emits[Pure, Timestamp](steps.keys.toList)
          .fproduct(_.plusCalValidityPeriod min last)
          .takeThrough((_, e) => e < last)
          .map { case (s, e) => BlockWindow(block.definition, steps.rangeFrom(s).rangeTo(e)) }
      }

    /**
     * A window which ends at the specified time but begins one cal validity
     * period earlier.
     */
    def windowEndingAt(timestamp: Timestamp): BlockWindow[D] =
      BlockWindow(
        block.definition,
        steps.rangeFrom(timestamp.minusCalValidityPeriod).rangeTo(timestamp)
      )

    /**
     * All the calibrated science steps in this record.  We compute this window
     * by window.  In one window the science step may be missing cals but in
     * another it may have them.
     */
    lazy val calibratedScience: Set[Step.Id] =
      windows
        .fold(Set.empty[Step.Id])((s, w) => s ++ w.calibratedScience)
        .compile
        .toList
        .head

    /**
     * Count of science datasets which have their calibrations.  These will
     * count towards completion.
     */
    lazy val calibratedScienceCount: NonNegInt =
      NonNegInt.unsafeFrom(calibratedScience.size)

    /**
     * Marks the end of the record lifetime, books the calibrated science steps
     * and resets.
     */
    def settle: BlockRecord[D] =
      if steps.isEmpty then this else copy(
        block = block.add(calibratedScienceCount),
        steps = SortedMap.empty
      )

    /** Records the next step. */
    def record(step: StepRecord[D])(using Eq[D]): BlockRecord[D] =
      if block.definition.matches(step) then copy(steps = steps + (step.created -> step))
      else this

    /**
     * How much time is left in the current iteration of this block at time
     * `timestamp`.
     */
    def remainingTimeAt(timestamp: Timestamp): TimeSpan =
      val limit = startTime.map(_.plusCalValidityPeriod).getOrElse(Timestamp.Min)
      val start = endTime.map(_ max timestamp).getOrElse(Timestamp.Max)
      Option.when(start <= limit)(TimeSpan.between(start, limit)).flatten.getOrElse(TimeSpan.Zero)

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
     * @tparam S static configration type
     * @return number of science datasets that will be contributed to completion
     *         by this block tupled with any missing steps to fill out the block
     */
    def generate[S](
      timestamp: Timestamp,
      static:    S,
      estimator: TimeEstimateCalculator[S, D],
      lastSteps: TimeEstimateCalculator.Last[D]
    ): (NonNegInt, List[ProtoStep[D]]) =

      // What calibrations are missing in the last window?
      val window      = windowEndingAt(timestamp)
      val missingCals = window.missingCals

      // Which pending science in the last window can be completed by adding
      // missing calibrations? Previous windows may have uncalibrated science,
      // but those would be lost since we won't get the calibration in time.
      val uncalibratedScience =
        if missingCals.isEmpty then Set.empty[Step.Id]
        else window.pendingScience -- calibratedScience

      // How many, at a maximum, ignoring time, science steps could you do to
      // finish out the block?
      val currentCount = (calibratedScience ++ uncalibratedScience).size
      val maxRemaining = block.remainingInBlock(currentCount)

      // How long do we have left to fill with science?  Adjust lastSteps as
      // though one or more science steps have just happened.
      val lastStepsʹ    = lastSteps.next(block.definition.science)
      val calTime       = estimator.estimateTotal(static, missingCals).runA(lastStepsʹ).value
      val remainingTime = remainingTimeAt(timestamp) -| calTime

      // How long would the first science step take?  It may be different from
      // remaining steps if there is a science fold move to make.
      val firstStepTime = estimator.estimateOne(static, block.definition.science).runA(lastSteps).value.total

      if remainingTime < firstStepTime then
        // No time left for more science
        if uncalibratedScience.sizeIs > 0 then
          // There are some calibrations we could add to save the last science though
          (NonNegInt.unsafeFrom(currentCount), missingCals)
        else
          // No pending science or cals, just count the science for which we have valid cals
          (NonNegInt.unsafeFrom(calibratedScience.size), Nil)
        end if
      else
        // Okay, there's time for at least one more step.
        val remainingTimeʹ = remainingTime -| firstStepTime

        // How long would each subsequent science step take? This should be less
        // than the first step time because there's no need to move the science
        // fold.
        val otherStepTime  = estimator.estimateStep(static, lastStepsʹ, block.definition.science).total

        // How many new science steps should we add then?  Do not go over the
        // max for the block or for the observation as a whole.
        val otherStepCount = (remainingTimeʹ.toMicroseconds / otherStepTime.toMicroseconds).toInt
        val newCount       = maxRemaining.value min (1 + otherStepCount)
        val scienceSteps   = List.fill(newCount)(block.definition.science)

        // Order the steps according to whether we've just done any science.
        // We want to avoid switching the science fold more than necessary.
        val steps = if currentCount == 0 then missingCals ++ scienceSteps
                    else scienceSteps ++ missingCals

        // This block will be contributing currentCount + newCount science to
        // the total, assuming 'steps` are executed.
        (NonNegInt.unsafeFrom(currentCount + newCount), steps)
      end if

    end generate

  end BlockRecord

  object BlockRecord:
    def init[D](definition: BlockDefinition[D]): BlockRecord[D] =
      BlockRecord(BlockCompletion.init(definition), SortedMap.empty)

  end BlockRecord

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
    time:        IntegrationTime,
    records:     NonEmptyVector[BlockRecord[D]],
    tracker:     IndexTracker,
    pos:         Int
  ) extends SequenceGenerator.Base[D]:

    val length: Int = records.length

    override def generate(timestamp: Timestamp): Stream[Pure, Atom[D]] =
      val (aix, six) = tracker.toTuple

      // The first atom will have any unfinished steps from the current atom, so
      // it is handled separately.
      val rec         = records.getUnsafe(pos)
      val block       = rec.block
      val (n, steps)  = if rec.steps.isEmpty then block.generate
                        else rec.generate(timestamp, static, estimator, lastSteps)
      val (cs, atom0) = atomBuilder.buildOption(block.desc.some, aix, six, steps).run(lastSteps).value
      val blocks      = records.map(_.block).updatedUnsafe(pos, block.add(n))

      Stream
        .iterate((pos + 1) % length)(pos => (pos + 1) % length)
        .mapAccumulate((blocks, aix + 1, cs)) { case ((blocks, aix, cs), pos) =>
          val block       = blocks.getUnsafe(pos)
          val (n, steps)  = block.generate
          val (csʹ, atom) = atomBuilder.buildOption(block.desc.some, aix, 0, steps).run(cs).value
          val blocksʹ     = blocks.updatedUnsafe(pos, block.add(n))
          ((blocksʹ, aix + 1, csʹ), atom)
        }
        .cons1((blocks, 0, TimeEstimateCalculator.Last.empty), atom0) // put the first atom back
        .takeThrough { case ((bs, _, _), _) => bs.foldMap(_.completed.value) < time.exposureCount.value }
        .collect { case (_, Some(atom)) => atom }
    end generate

    // Advances the block index we're working on, setting it to the first block
    // from 'start' that matches the step.
    private def advancePos(start: Int, step: StepRecord[D])(using Eq[D]): Int =
      Stream
        .iterate(start%length)(p => (p + 1) % length)
        .take(length)
        .dropWhile(p => !records.getUnsafe(p).block.definition.matches(step))
        .head
        .compile
        .toList
        .headOption
        .getOrElse(pos)

    override def recordStep(step: StepRecord[D])(using Eq[D]): SequenceGenerator[D] =
      if step.isAcquisitionSequence then
        this
      else
        val trackerʹ = tracker.record(step)

        // Advance the block we're focusing upon, if necessary.  This will happen
        // (potentially) for the first step recorded, or when a new atom is started
        val (recordsʹ, posʹ) =
          tracker match
            case IndexTracker.Zero =>
              (records, advancePos(0, step))
            case _                 =>
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
      blockDef:  BlockDefinition.Computer[D, G, L, U],
      config:    Config[G, L, U],
      time:      Either[OdbError, IntegrationTime],
      calRole:   Option[CalibrationRole]
    ): F[Either[OdbError, SequenceGenerator[D]]] =

      def sequenceUnavailable(m: String): OdbError =
        OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $m".some)

      def extractTime: Either[OdbError, IntegrationTime] =
        time.filterOrElse(
          _.exposureTime.toNonNegMicroseconds.value > 0,
          sequenceUnavailable(s"GMOS Long Slit science requires a positive exposure time.")
        )

      // Adjust the config and integration time according to the calibration role.
      val configAndTime = calRole match
        case None                                     =>
          extractTime.tupleLeft(config)

        case Some(CalibrationRole.SpectroPhotometric) =>
          val configʹ = calibrationObservationConfig(config)
          extractTime
            .map(_.copy(exposureCount = PosInt.unsafeFrom(configʹ.wavelengthDithers.length)))
            .tupleLeft(configʹ)

        case Some(CalibrationRole.Twilight)           =>
          val configʹ = calibrationObservationConfig(config)
          val timeʹ   = IntegrationTime(TwilightExposureTime, PosInt.unsafeFrom(configʹ.wavelengthDithers.length))
          (configʹ, timeʹ).asRight[OdbError]

        case Some(c)                                  =>
          sequenceUnavailable(s"GMOS Long Slit ${c.tag} not implemented").asLeft

      // If exposure time is longer than the science period, there will never be
      // time enough to do any science steps.
      val configAndTimeʹ = configAndTime.filterOrElse(
        _._2.exposureTime <= SciencePeriod,
        sequenceUnavailable(s"Exposure times over ${SciencePeriod.toMinutes} minutes are not supported.")
      )

      // Compute the generator
      val result = for
        (configʹ, timeʹ) <- EitherT.fromEither[F](configAndTimeʹ)
        defs             <- EitherT(blockDef.compute(oid, expander, configʹ, timeʹ, calRole))
      yield ScienceGenerator(
        estimator,
        static,
        TimeEstimateCalculator.Last.empty[D],
        AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science),
        timeʹ,
        defs.map(BlockRecord.init).toNev,
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
    ScienceGenerator.instantiate(observationId, estimator, static, namespace, expander, BlockDefinition.North, config, time, calRole)

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
    ScienceGenerator.instantiate(observationId, estimator, static, namespace, expander, BlockDefinition.South, config, time, calRole)

end Science
