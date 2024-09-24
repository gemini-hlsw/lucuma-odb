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
import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.order.*
import cats.syntax.traverse.*
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
import lucuma.core.enums.StepType
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.optics.syntax.lens.*
import lucuma.core.optics.syntax.optional.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StepRecord
import lucuma.odb.sequence.data.VisitRecord
import lucuma.odb.sequence.util.IndexTracker
import monocle.Focus
import monocle.Lens

import scala.annotation.tailrec

object Science:

  /**
   * Defines how long does a calibration remains valid.  After this amount of
   * time, the calibration must be taken again.
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
   * Captures an adjustment to the fixed instrument and telescope position that
   * otherwise remains constant throughout a GMOS longslit observation.
   * (dither) and a small spatial offset (in q).
   *
   * @param Δλ small wavelength adjustment (dither)
   * @param q small spatial offset in q
   */
  case class Adjustment(Δλ: WavelengthDither, q: Offset.Q):
    def description: String =
      s"${Δλ.toNanometers.value} nm, ${Angle.signedDecimalArcseconds.get(q.toAngle)}″"

  object Adjustment:

    def compute(
      wavelengthDithers: List[WavelengthDither],
      spatialOffsets:    List[Offset.Q]
    ): List[Adjustment] =
      @tailrec def gcd(a: Long, b: Long): Long = if (b === 0) a else gcd(b, a%b)
      def lcm(as: Long*): Long = as.reduce { (a, b) => a/gcd(a,b)*b }

      val Δλs  = wavelengthDithers match {
        case Nil => Stream(WavelengthDither.Zero)
        case ws  => Stream.emits(ws)
      }

      val qs   = spatialOffsets match {
        case Nil => Stream(Offset.Q.Zero)
        case os  => Stream.emits(os)
      }

      Δλs
        .repeat
        .zip(qs.repeat)
        .take(lcm(wavelengthDithers.size max 1, spatialOffsets.size max 1))
        .map(Adjustment(_, _))
        .toList
  end Adjustment

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
   * A description of the Steps associated with each Goal.
   */
  case class Steps[D](
    goal:    Goal,
    flats:   List[ProtoStep[D]],
    arcs:    List[ProtoStep[D]],
    science: ProtoStep[D]
  ):
    val flatCounts: Map[ProtoStep[D], Int] = Steps.calCounts(flats)
    val arcCounts:  Map[ProtoStep[D], Int] = Steps.calCounts(arcs)
    def allCals: List[ProtoStep[D]] = arcs ++ flats

  object Steps:
    private def calCounts[D](cal: List[ProtoStep[D]]): Map[ProtoStep[D], Int] =
      cal.groupMapReduce(identity)(_ => 1)(_ + _)

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
      ): F[Either[String, List[Steps[D]]]] =
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
          } yield Steps(g, fs, as.toList, science)).value
        }.map(_.sequence)

      end compute
    end Computer

    object North extends GmosNorthSequenceState
                    with Computer[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]

    object South extends GmosSouthSequenceState
                    with Computer[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]
  end Steps

  case class WavelengthBlock[D](
    steps:      Steps[D],
    science:    List[Timestamp],
    cal:        Map[ProtoStep[D], List[Timestamp]],
    completed:  Int
  ):

    def isEmpty: Boolean =
      science.isEmpty && cal.isEmpty

    // when does the earliest calibration in this block expire?
    lazy val calibrationExpiration: Option[Timestamp] =
      cal
      .values
      .toList
      .flatten
      .minOption
      .flatMap(_.plusMicrosOption(CalValidityPeriod.toMicroseconds))

    def missingCalsAt(t: Timestamp): List[ProtoStep[D]] =
      // A map, keyed by calibration step, containing how many instances are
      // still needed.
      val remaining = (steps.arcCounts ++ steps.flatCounts).map { (step, n) =>
        val valid = cal.get(step).fold(0)(_.count(ct => TimeSpan.between(ct, t).exists(_ <= CalValidityPeriod)))
        (step, (n - valid) max 0)
      }
      // Filter the ordered list of arcs + flats, removing still valid ones from
      // the beginning.
      (steps.arcs ++ steps.flats).foldRight((List.empty[ProtoStep[D]], remaining)) { case (step, (res, remainingʹ)) =>
        val r = remainingʹ(step)
        if r > 0 then (step :: res, remainingʹ.updated(step, r - 1))
        else (res, remainingʹ)
      }._1

    def hasValidCalibrations(t: Timestamp): Boolean =
      missingCalsAt(t).isEmpty

    lazy val scienceCount: Int =
      if cal.isEmpty then 0 else science.count(hasValidCalibrations)

    def remainingScienceTimeFrom(t: Timestamp): TimeSpan =
      calibrationExpiration
        .filter(_ > t)
        .flatMap(TimeSpan.between(t, _))
        .getOrElse(TimeSpan.Zero)

    def unexecutedRemainingScienceExposures: Int =
      val forCurrentBlock  = (steps.goal.perBlockExposures - scienceCount)            max 0
      val forWholeObs      = (steps.goal.totalExposures - (completed + scienceCount)) max 0
      forCurrentBlock min forWholeObs

    // remaining science exposures in this block (there could be more for future
    // blocks)
    def partiallyExecutedRemainingScienceExposures(t: Timestamp, exposureTime: TimeSpan): Int =
      unexecutedRemainingScienceExposures min
        (remainingScienceTimeFrom(t).toMicroseconds / exposureTime.toMicroseconds).toInt

    // reset the science and cal step trackers and increment the completed count
    def settle: WavelengthBlock[D] =
      copy(science = List.empty, cal = Map.empty, completed = completed + scienceCount)

    def matches(step: StepRecord[D])(using Eq[D]): Boolean =
      step.isScienceSequence && (
        step.stepConfig.stepType match
          case StepType.Bias |
               StepType.Dark |
               StepType.SmartGcal => false
          case StepType.Gcal      => steps.arcs.exists(_.matches(step)) || steps.flats.exists(_.matches(step))
          case StepType.Science   => steps.science.matches(step)
      )

    private def addStep(step: StepRecord[D]): WavelengthBlock[D] =
      step.stepConfig match
        case StepConfig.Bias | StepConfig.Dark | StepConfig.SmartGcal(_) =>
          this

        case StepConfig.Gcal(_, _, _, _) =>
          copy(cal = cal.updatedWith(step.protoStep) { ots =>
            // TODO: created?
            (step.created :: ots.toList.flatten).some
          })

        case StepConfig.Science(_, _) =>
          copy(science = step.created :: science)

    def record(step: StepRecord[D])(using Eq[D]): WavelengthBlock[D] =
      if step.successfullyCompleted && matches(step) then addStep(step)
      else this

    def unexecutedRemainder: (WavelengthBlock[D], List[ProtoStep[D]]) =
      val n = unexecutedRemainingScienceExposures
      if n === 0 then (this, Nil)
      else (
        WavelengthBlock.completed.modify(_ + n)(settle),
        steps.allCals ++ List.fill(n)(steps.science)
      )

    def partiallyExecutedRemainder(
      atTime: Timestamp,
      exposureTime: TimeSpan
    ): (WavelengthBlock[D], List[ProtoStep[D]]) =
      if completed >= steps.goal.totalExposures then
        (this, Nil)
      else
        val cs = missingCalsAt(atTime)

        // Add the missing cals as if they just happened.
        val wb = copy(cal = cs.foldLeft(cal) { (m, calStep) =>
          m.updatedWith(calStep)(o => (atTime :: o.toList.flatten).some)
        })

        val n   = wb.partiallyExecutedRemainingScienceExposures(atTime, exposureTime)
        val wbʹ = WavelengthBlock.completed.modify(_ + n)(wb.settle)
        val ss  = List.fill(n)(steps.science)

        // If there are no pending science datasets already put the cals at the
        // beginning.  Otherwise the missing cals should finish the block.
        if science.isEmpty then (wbʹ, cs ++ ss) else (wbʹ, ss ++ cs)
    end partiallyExecutedRemainder

    def remainder(
      atTime:       Timestamp,
      exposureTime: TimeSpan
    ): (WavelengthBlock[D], List[ProtoStep[D]]) =
      if isEmpty then unexecutedRemainder
      else partiallyExecutedRemainder(atTime, exposureTime)

  end WavelengthBlock

  object WavelengthBlock:
    def init[D](steps: Steps[D]): WavelengthBlock[D] =
      WavelengthBlock(steps, List.empty, Map.empty, 0)

    def completed[D]: Lens[WavelengthBlock[D], Int] =
      Focus[WavelengthBlock[D]](_.completed)

  private case class ScienceGenerator[D](
    time:    IntegrationTime,
    blocks:  List[WavelengthBlock[D]],
    tracker: IndexTracker,
    pos:     Int
  ) extends SequenceGenerator[D]:

    val length: Int = blocks.length

    override def generate(t: Timestamp): Stream[Pure, (ProtoAtom[(ProtoStep[D], Int)], Int)] =
      val (aix, six) = tracker.toTuple
      Stream
        .iterate(pos)(idx => (idx + 1) % length)
        .mapAccumulate((blocks.toVector, aix)) { case ((bs, aixʹ), idx) =>
          val wb   = bs(idx)
          val (wbʹ, steps) = wb.remainder(t, time.exposureTime)
          val bsʹ  = bs.updated(idx, wbʹ)
          val desc = NonEmptyString.unapply(wb.steps.goal.adjustment.description)
          val sixʹ = if wb.isEmpty then 0 else six
          val atom = NonEmptyList.fromList(steps).map { nel =>
            (ProtoAtom(desc, nel.zipWithIndex.map(_.map(_ + sixʹ))), aixʹ)
          }
          ((bsʹ, aixʹ + 1), atom)
        }
        .takeThrough { case ((bs, _), _) => bs.foldMap(_.completed) < time.exposureCount.value }
        .collect { case (_, Some(atom)) => atom }

    private def advancePos(start: Int, step: StepRecord[D])(using Eq[D]): Int =
      Stream
        .iterate(start)(p => (p + 1) % length)
        .take(length)
        .dropWhile(p => !blocks(p).matches(step))
        .head
        .compile
        .toList
        .headOption
        .getOrElse(pos)

    override def recordStep(step: StepRecord[D])(using Eq[D]): SequenceGenerator[D] =
      val trackerʹ = tracker.record(step)

      val (blocksʹ, posʹ) =
        if tracker.atomCount === trackerʹ.atomCount then (blocks, pos)
        else (blocks.map(_.settle), advancePos(pos+1, step))

      val blocksʹʹ =
        step.stepConfig.stepType match
          case StepType.Bias | StepType.Dark | StepType.SmartGcal =>
            // GMOS Longslit doesn't use biases or darks, and smart gcal has
            // been expanded so ignore these.
            blocksʹ
          case StepType.Gcal                                      =>
            // We don't know which spatial offset this is associated with so
            // record it everywhere
            blocksʹ.map(_.record(step))
          case StepType.Science                                   =>
            // Record the step at the current index, settle all others
            blocksʹ.zipWithIndex.map { (b, idx) =>
              if posʹ === idx then b.record(step) else b.settle
            }

      ScienceGenerator(time, blocksʹʹ, trackerʹ, posʹ)

    end recordStep

    override def recordVisit(visit: VisitRecord): SequenceGenerator[D] =
      this

  end ScienceGenerator

  private object ScienceGenerator:

    private def specPhotDithers[G, L, U](c: Config[G, L, U]): List[WavelengthDither] =
      val limit = c.coverage.toPicometers.value.value / 10.0
      if c.wavelengthDithers.exists(_.toPicometers.value.abs > limit) then c.wavelengthDithers
      else List(WavelengthDither.Zero)

    def instantiate[F[_]: Monad, D, G, L, U](
      sc:          Steps.Computer[D, G, L, U],
      expander:    SmartGcalExpander[F, D],
      config:      Config[G, L, U],
      time:        IntegrationTime,
      calRole:     Option[CalibrationRole]
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

      (for {
        (configʹ, timeʹ) <- EitherT.fromEither[F](e)
        steps            <- EitherT(sc.compute(expander, configʹ, timeʹ, includeArcs = calRole.isEmpty))
      } yield ScienceGenerator(timeʹ, steps.map(WavelengthBlock.init), IndexTracker.Zero, 0)).value

  end ScienceGenerator

  def gmosNorth[F[_]: Monad](
    expander: SmartGcalExpander[F, GmosNorth],
    config:   Config.GmosNorth,
    time:     IntegrationTime,
    calRole:  Option[CalibrationRole]
  ): F[Either[String, SequenceGenerator[GmosNorth]]] =
    ScienceGenerator.instantiate(Steps.North, expander, config, time, calRole)

  def gmosSouth[F[_]: Monad](
    expander: SmartGcalExpander[F, GmosSouth],
    config:   Config.GmosSouth,
    time:     IntegrationTime,
    calRole:  Option[CalibrationRole]
  ): F[Either[String, SequenceGenerator[GmosSouth]]] =
    ScienceGenerator.instantiate(Steps.South, expander, config, time, calRole)

end Science