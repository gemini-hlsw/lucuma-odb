// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit

import cats.Comparison.*
import cats.Eq
import cats.Monad
import cats.Order.catsKernelOrderingForOrder
import cats.data.EitherT
import cats.data.State
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.order.*
import cats.syntax.traverse.*
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
import lucuma.core.enums.StepType
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
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
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StepRecord
import lucuma.odb.sequence.data.VisitRecord
import lucuma.odb.sequence.util.AtomBuilder
import lucuma.odb.sequence.util.IndexTracker

import java.util.UUID
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

object Science:

  /**
   * Defines how long does a calibration remains valid.  After this amount of
   * time, the calibration must be taken again.
   */
  val CalValidityPeriod: TimeSpan =
    90.minuteTimeSpan

  extension (t: Timestamp)
    def plusCalValidityPeriod: Timestamp =
      t.plusMicrosOption(CalValidityPeriod.toMicroseconds).getOrElse(Timestamp.Max)

    def minusCalValidityPeriod: Timestamp =
      t.plusMicrosOption(- CalValidityPeriod.toMicroseconds).getOrElse(Timestamp.Min)

  private val Zero: NonNegInt = NonNegInt.unsafeFrom(0)

  /**
   * The nominal amount of time to spend at one (wavelength dither, spatial
   * offset) pair.  This will help determine how many exposures we hope to
   * obtain while processing a single "wavelength block".
   */
  val SciencePeriod: TimeSpan =
    1.hourTimeSpan

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

    def compute(
      wavelengthDithers: List[WavelengthDither],
      spatialOffsets:    List[Offset.Q]
    ): List[Adjustment] =
      @tailrec def gcd(a: Long, b: Long): Long = if (b === 0) a else gcd(b, a%b)
      def lcm(as: Long*): Long = as.reduce { (a, b) => a/gcd(a,b)*b }

      val Δλs  = wavelengthDithers match
        case Nil => Stream(WavelengthDither.Zero)
        case ws  => Stream.emits(ws)

      val qs   = spatialOffsets match
        case Nil => Stream(Offset.Q.Zero)
        case os  => Stream.emits(os)

      Δλs
        .repeat
        .zip(qs.repeat)
        .take(lcm(wavelengthDithers.size max 1, spatialOffsets.size max 1))
        .map(Adjustment(_, _))
        .toList
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
    perBlockExposures: Int,
    totalExposures:    Int
  )

  object Goal:

    def compute(
      wavelengthDithers: List[WavelengthDither],
      spatialOffsets:    List[Offset.Q],
      integration:       IntegrationTime
    ): List[Goal] =
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
          val expCount = perBlock + (if idx < extra then 1 else 0)
          Goal(adj, expCount, expCount)
      else
        val fullBlocks = exposureCount / maxExpPerBlock
        val base       = fullBlocks / size * maxExpPerBlock
        adjs.zipWithIndex.map: (adj, idx) =>
          val extra = idx.toLong.comparison(fullBlocks % size) match
            case GreaterThan => 0
            case EqualTo     => exposureCount % maxExpPerBlock // left over
            case LessThan    => maxExpPerBlock
          Goal(adj, maxExpPerBlock, base + extra)
      end if

  end Goal

  /**
   * A description of the steps (arcs, flats, and science) that are associated
   * with an excution Goal.  The collections of steps is executed together in a
   * block, or "atom".
   */
  case class BlockDefinition[D](
    goal:    Goal,
    flats:   List[ProtoStep[D]],
    arcs:    List[ProtoStep[D]],
    science: ProtoStep[D]
  ):
    val flatCounts: Map[ProtoStep[D], NonNegInt] = BlockDefinition.calCounts(flats)
    val arcCounts:  Map[ProtoStep[D], NonNegInt] = BlockDefinition.calCounts(arcs)
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

      def compute[F[_]: Monad](
        expander:    SmartGcalExpander[F, D],
        config:      Config[G, L, U],
        time:        IntegrationTime,
        includeArcs: Boolean
      ): F[Either[String, List[BlockDefinition[D]]]] =
        Goal.compute(config.wavelengthDithers, config.spatialOffsets, time).traverse { g =>
          val λ = config.centralWavelength
          val (smartArc, smartFlat, science) = eval {
            for {
              _ <- setup(config, time)
              _ <- optics.wavelength := λ.offset(g.adjustment.Δλ).getOrElse(λ)
              a <- arcStep(ObserveClass.PartnerCal)
              f <- flatStep(ObserveClass.PartnerCal)
              s <- scienceStep(Offset(Offset.P.Zero, g.adjustment.q), ObserveClass.Science)
            } yield (a, f, s)
          }

          (for {
            fs <- EitherT(expander.expandStep(smartFlat)).map(_.toList)
            as <- if includeArcs then EitherT(expander.expandStep(smartArc)).map(_.toList) else EitherT.pure(List.empty)
          } yield BlockDefinition(g, fs, as.toList, science)).value
        }.map(_.sequence)

      end compute
    end Computer

    object North extends GmosNorthSequenceState
                    with Computer[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]

    object South extends GmosSouthSequenceState
                    with Computer[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]
  end BlockDefinition

  case class BlockCompletion[D](
    definition: BlockDefinition[D],
    completed:  NonNegInt
  ):
    def desc: NonEmptyString =
      definition.goal.adjustment.description

    def add(n: NonNegInt): BlockCompletion[D] =
      copy(completed = NonNegInt.unsafeFrom(completed.value + n.value))

    def remainingInObs: NonNegInt =
      NonNegInt.unsafeFrom((definition.goal.totalExposures - completed.value) max 0)

    def remainingInBlock(pending: Int): NonNegInt =
      NonNegInt.unsafeFrom(
        ((remainingInObs.value min definition.goal.perBlockExposures) - pending) max 0
      )

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

  trait BlockWindow[D]:
    def missingCalCounts: Map[ProtoStep[D], NonNegInt]
    def missingCals: List[ProtoStep[D]]
    def pendingScience: Set[Step.Id]
    def pendingScienceCount: NonNegInt
    def calibratedScience: Set[Step.Id]
    def calibratedScienceCount: NonNegInt =
      NonNegInt.unsafeFrom(calibratedScience.size)

  object BlockWindow:
    def apply[D](
      block: BlockDefinition[D],
      steps: SortedMap[Timestamp, StepRecord[D]]
    ): BlockWindow[D] =
      new BlockWindow[D]:
        lazy val missingCalCounts: Map[ProtoStep[D], NonNegInt] =
          steps
            .values
            .filter(s => s.successfullyCompleted && s.isGcal)
            .foldLeft(block.allCalsCounts) { (cals, step) =>
              cals.updatedWith(step.protoStep)(_.flatMap(n => NonNegInt.unapply(n.value - 1)))
            }

        lazy val missingCals: List[ProtoStep[D]] =
          // Filter the ordered list of arcs + flats, removing still valid ones from
          // the beginning.
          block.allCals.foldLeft((List.empty[ProtoStep[D]], missingCalCounts)) {
            case ((res, remaining), calStep) =>
              NonNegInt
                .unapply(remaining.getOrElse(calStep, Zero).value - 1)
                .fold((res, remaining)) { n =>
                  (calStep :: res, remaining.updated(calStep, n))
                }
          }._1.reverse

        lazy val pendingScience: Set[Step.Id] =
          steps.values.collect {
            case s if s.successfullyCompleted && s.isScience => s.id
          }.toSet

        lazy val pendingScienceCount: NonNegInt =
          NonNegInt.unsafeFrom(pendingScience.size)

        lazy val calibratedScience: Set[Step.Id] =
          if missingCalCounts.values.exists(_.value > 0) then Set.empty
          else pendingScience

  end BlockWindow

  case class BlockRecord[D](
    block: BlockCompletion[D],
    steps: SortedMap[Timestamp, StepRecord[D]],
  ):
    val startTime: Option[Timestamp] =
      steps.headOption.map(_._2.created)

    val endTime: Option[Timestamp] =
      steps.lastOption.map(_._2.created)

    val interval: Option[TimestampInterval] =
      (startTime, endTime).mapN(TimestampInterval.between)

    def windows: Stream[Pure, BlockWindow[D]] =
      endTime.fold(Stream.empty) { last =>
        Stream
          .emits[Pure, Timestamp](steps.keys.toList)
          .fproduct(_.plusCalValidityPeriod min last)
          .takeThrough((_, e) => e < last)
          .map { case (s, e) => BlockWindow(block.definition, steps.rangeFrom(s).rangeTo(e)) }
      }

    def windowEndingAt(timestamp: Timestamp): BlockWindow[D] =
      BlockWindow(
        block.definition,
        steps.rangeFrom(timestamp.minusCalValidityPeriod).rangeTo(timestamp)
      )

    lazy val calibratedScience: Set[Step.Id] =
      windows
        .fold(Set.empty[Step.Id])((s, w) => s ++ w.calibratedScience)
        .compile
        .toList
        .head

    lazy val calibratedScienceCount: NonNegInt =
      NonNegInt.unsafeFrom(calibratedScience.size)

    def settle: BlockRecord[D] =
      if steps.isEmpty then this else copy(
        block = block.add(calibratedScienceCount),
        steps = SortedMap.empty
      )

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

    def generate[S](
      timestamp: Timestamp,
      static:    S,
      estimator: TimeEstimateCalculator[S, D],
      calcState: TimeEstimateCalculator.Last[D]
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

      // Time estimate calc state up to now.
      val calcStateʹ = steps.foldLeft(calcState) { case (cur, (_, step)) =>
         cur.next(step.protoStep)
      }

      // How long do we have left to fill with science?
      val calTime       = TimeEstimateCalculator.estimateTimeSpan(estimator, static, calcStateʹ, missingCals)
      val remainingTime = remainingTimeAt(timestamp) -| calTime

      // How long would the first science step take?  It may be different from
      // remaining steps if there is a science fold move to make.
      val firstStepTime = estimator.estimateStep(static, calcState, block.definition.science).total

      // How long would each subsequent science step take?
      val otherStepTime = estimator.estimateStep(static, calcStateʹ, block.definition.science).total

      if remainingTime < firstStepTime then
        if uncalibratedScience.sizeIs > 0 then
          (NonNegInt.unsafeFrom(currentCount), missingCals)
        else
          (NonNegInt.unsafeFrom(calibratedScience.size), Nil)
      else
        val remainingTimeʹ = remainingTime -| firstStepTime
        val otherStepCount = (remainingTimeʹ.toMicroseconds / otherStepTime.toMicroseconds).toInt
        val newCount       = maxRemaining.value min (1 + otherStepCount)
        val scienceSteps   = List.fill(newCount)(block.definition.science)
        val steps = if currentCount == 0 then missingCals ++ scienceSteps
                    else scienceSteps ++ missingCals
        (NonNegInt.unsafeFrom(currentCount + newCount), steps)
      end if

    end generate

  end BlockRecord

  object BlockRecord:
    def init[D](definition: BlockDefinition[D]): BlockRecord[D] =
      BlockRecord(BlockCompletion.init(definition), SortedMap.empty)

  end BlockRecord

  private case class ScienceGenerator[S, D](
    estimator:   TimeEstimateCalculator[S, D],
    static:      S,
    atomBuilder: AtomBuilder[D],
    time:        IntegrationTime,
    records:     List[BlockRecord[D]],
    tracker:     IndexTracker,
    pos:         Int
  ) extends SequenceGenerator[D]:

    val length: Int = records.length

    override def generate(timestamp: Timestamp): Stream[Pure, Atom[D]] =
      val (aix, six) = tracker.toTuple
      val startState = TimeEstimateCalculator.Last.empty[D]

      // The first atom will have any unfinished steps from the current atom, so
      // it is handled separately.
      val rec         = records(pos)
      val block       = rec.block
      val (n, steps)  = if rec.steps.isEmpty then block.generate
                        else rec.generate(timestamp, static, estimator, startState)
      val (cs, atom0) = atomBuilder.buildOption(block.desc.some, aix, six, steps).run(startState).value
      val blocks      = records.map(_.block).updated(pos, block.add(n)).toVector

      Stream
        .iterate((pos + 1) % length)(pos => (pos + 1) % length)
        .mapAccumulate((blocks, aix + 1, cs)) { case ((blocks, aix, cs), pos) =>
          val block       = blocks(pos)
          val (n, steps)  = block.generate
          val (csʹ, atom) = atomBuilder.buildOption(block.desc.some, aix, 0, steps).run(cs).value
          val blocksʹ     = blocks.updated(pos, block.add(n))
          ((blocksʹ, aix + 1, csʹ), atom)
        }
        .cons1((blocks, 0, 0), atom0) // put the first atom back
        .takeThrough { case ((bs, _, _), _) => bs.foldMap(_.completed.value) < time.exposureCount.value }
        .collect { case (_, Some(atom)) => atom }
    end generate

    private def advancePos(start: Int, step: StepRecord[D])(using Eq[D]): Int =
      Stream
        .iterate(start)(p => (p + 1) % length)
        .take(length)
        .dropWhile(p => !records(p).block.definition.matches(step))
        .head
        .compile
        .toList
        .headOption
        .getOrElse(pos)

    override def recordStep(step: StepRecord[D])(using Eq[D]): SequenceGenerator[D] =
      val trackerʹ = tracker.record(step)

      val (recordsʹ, posʹ) =
        if tracker.atomCount === trackerʹ.atomCount then (records, pos)
        else (records.map(_.settle), advancePos(pos+1, step))

      val recordsʹʹ =
        step.stepConfig.stepType match
          case StepType.Bias | StepType.Dark | StepType.SmartGcal =>
            // GMOS Longslit doesn't use biases or darks, and smart gcal has
            // been expanded so ignore these.
            recordsʹ
          case StepType.Gcal                                      =>
            // We don't know which spatial offset this is associated with so
            // record it everywhere
            recordsʹ.map(_.record(step))
          case StepType.Science                                   =>
            // Record the step at the current index, settle all others
            recordsʹ.zipWithIndex.map: (rec, idx) =>
              if posʹ === idx then rec.record(step) else rec.settle

      copy(
        records = recordsʹʹ,
        tracker = trackerʹ,
        pos     = posʹ
      )

    end recordStep

    override def recordVisit(visit: VisitRecord): SequenceGenerator[D] =
      this

  end ScienceGenerator

  private object ScienceGenerator:

    private def specPhotDithers[G, L, U](c: Config[G, L, U]): List[WavelengthDither] =
      val limit = c.coverage.toPicometers.value.value / 10.0
      if c.wavelengthDithers.exists(_.toPicometers.value.abs > limit) then c.wavelengthDithers
      else List(WavelengthDither.Zero)

    def instantiate[F[_]: Monad, S, D, G, L, U](
      estimator: TimeEstimateCalculator[S, D],
      static:    S,
      namespace: UUID,
      expander:  SmartGcalExpander[F, D],
      blockDef:  BlockDefinition.Computer[D, G, L, U],
      config:    Config[G, L, U],
      time:      IntegrationTime,
      calRole:   Option[CalibrationRole]
    ): F[Either[String, SequenceGenerator[D]]] =
      val e = calRole match
        case None                                     =>
          (config, time).asRight[String]

        case Some(CalibrationRole.SpectroPhotometric) =>
          val dithers = specPhotDithers(config)
          val configʹ = (for {
            _ <- Config.explicitWavelengthDithers := dithers.some
            _ <- Config.explicitSpatialOffsets    := List(Offset.Q.Zero).some
          } yield ()).runS(config).value
          val timeʹ   = time.copy(exposureCount = PosInt.unsafeFrom(dithers.length))
          (configʹ, timeʹ).asRight[String]

        case Some(c)                                  =>
          s"GMOS Long Slit ${c.tag} not implemented".asLeft

      val atomBuilder = AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science)
      (for {
        (configʹ, timeʹ) <- EitherT.fromEither[F](e)
        defs             <- EitherT(blockDef.compute(expander, configʹ, timeʹ, includeArcs = calRole.isEmpty))
      } yield ScienceGenerator(estimator, static, atomBuilder, timeʹ, defs.map(BlockRecord.init), IndexTracker.Zero, 0)).value

  end ScienceGenerator

  def gmosNorth[F[_]: Monad](
    estimator: TimeEstimateCalculator[StaticConfig.GmosNorth, GmosNorth],
    static:    StaticConfig.GmosNorth,
    namespace: UUID,
    expander:  SmartGcalExpander[F, GmosNorth],
    config:    Config.GmosNorth,
    time:      IntegrationTime,
    calRole:   Option[CalibrationRole]
  ): F[Either[String, SequenceGenerator[GmosNorth]]] =
    ScienceGenerator.instantiate(estimator, static, namespace, expander, BlockDefinition.North, config, time, calRole)

  def gmosSouth[F[_]: Monad](
    estimator: TimeEstimateCalculator[StaticConfig.GmosSouth, GmosSouth],
    static:    StaticConfig.GmosSouth,
    namespace: UUID,
    expander:  SmartGcalExpander[F, GmosSouth],
    config:    Config.GmosSouth,
    time:      IntegrationTime,
    calRole:   Option[CalibrationRole]
  ): F[Either[String, SequenceGenerator[GmosSouth]]] =
    ScienceGenerator.instantiate(estimator, static, namespace, expander, BlockDefinition.South, config, time, calRole)

end Science