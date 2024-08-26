// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit

import cats.Eq
import cats.Comparison.*
import cats.Monad
import cats.Order.catsKernelOrderingForOrder
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.functor.*
import cats.syntax.order.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import fs2.Pure
import fs2.Stream
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
import lucuma.core.syntax.timespan.*
import lucuma.core.optics.syntax.lens.*
import lucuma.core.optics.syntax.optional.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StepRecord
import monocle.Focus
import monocle.Lens
import scala.annotation.tailrec

sealed trait Science2[D]:

  def generate: Stream[Pure, ProtoAtom[ProtoStep[D]]]

  def record(step: StepRecord[D])(using Eq[D]): Science2[D]

end Science2

object Science2:

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
      s"${Δλ.toNanometers.value} nm, ${Angle.signedDecimalArcseconds.get(q.toAngle)}\""

  object Adjustment:

    def adjustmentsFor(
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
    toalExposures:     Int
  )

  object Goal:

    def goalsFor(
      wavelengthDithers: List[WavelengthDither],
      spatialOffsets:    List[Offset.Q],
      integration:       IntegrationTime
    ): List[Goal] =
      val adjs = Adjustment.adjustmentsFor(wavelengthDithers, spatialOffsets)
      val size = adjs.size

      val sci  = SciencePeriod.toMicroseconds
      val time = sci min integration.exposureTime.toNonNegMicroseconds.value

      val maxExpPerBlock = (sci / time).toInt
      val exposureCount  = integration.exposures.value

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
    flats:   NonEmptyList[ProtoStep[D]],
    arcs:    NonEmptyList[ProtoStep[D]],
    science: ProtoStep[D]
  ):
    val flatCounts: Map[ProtoStep[D], Int] = Steps.calCounts(flats)
    val arcCounts:  Map[ProtoStep[D], Int] = Steps.calCounts(arcs)

  object Steps:
    private def calCounts[D](cal: NonEmptyList[ProtoStep[D]]): Map[ProtoStep[D], Int] =
      cal.toList.groupMapReduce(identity)(_ => 1)(_ + _)

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
        expander: SmartGcalExpander[F, D],
        config:   Config[G, L, U],
        time:     IntegrationTime
      ): F[Either[String, List[Steps[D]]]] =
        Goal.goalsFor(config.wavelengthDithers, config.spatialOffsets, time).traverse { g =>
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
            fs <- EitherT(expander.expandStep(smartFlat))
            as <- EitherT(expander.expandStep(smartArc))
          } yield Steps(g, fs, as, science)).value
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

    // when does the earliest calibration in this block expire?
    lazy val calibrationExpiration: Option[Timestamp] =
      cal
      .values
      .toList
      .flatten
      .minOption
      .flatMap(_.plusMicrosOption(CalValidityPeriod.toMicroseconds))

    def missingCalsAt(t: Timestamp): List[ProtoStep[D]] =
      (steps.arcCounts.toList ++ steps.flatCounts.toList).foldLeft(List.empty[ProtoStep[D]]) { case (res, (step, cnt)) =>
        val missing = cal.get(step).fold(cnt) { instances =>
          cnt - instances.count(ct => TimeSpan.between(ct, t).exists(_ <= CalValidityPeriod))
        }
        if missing <= 0 then res else res ++ List.fill(missing)(step)
      }

    def hasValidCalibrations(t: Timestamp): Boolean =
      missingCalsAt(t).isEmpty

    lazy val scienceCount: Int =
      science.count(hasValidCalibrations)

    def remainingScienceTimeFrom(t: Timestamp): TimeSpan =
      calibrationExpiration
        .filter(_ > t)
        .flatMap(TimeSpan.between(t, _))
        .getOrElse(TimeSpan.Zero)

    // remaining science exposures in this block (there could be more for future
    // blocks)
    def remainingScienceExposuresAt(t: Timestamp, exposureTime: TimeSpan): Int =
      val forCurrentBlock  = (steps.goal.perBlockExposures - scienceCount)           max 0
      val forWholeObs      = (steps.goal.toalExposures - (completed + scienceCount)) max 0
      val forRemainingTime = (remainingScienceTimeFrom(t).toMicroseconds / exposureTime.toMicroseconds).toInt
      forCurrentBlock min forWholeObs min forRemainingTime

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
      if !matches(step) then settle
      else if step.successfullyCompleted then addStep(step)
      else this

    def remainderAt(t: Timestamp, exposureTime: TimeSpan): (List[ProtoStep[D]], WavelengthBlock[D]) =
      val cs = missingCalsAt(t)

      // Add the missing cals as if they just happened.
      val wb = copy(cal = cs.foldLeft(cal) { (m, calStep) =>
        m.updatedWith(calStep)(o => (t :: o.toList.flatten).some)
      })

      val n  = wb.remainingScienceExposuresAt(t, exposureTime)
      val ss = List.fill(n)(steps.science)

      val wbʹ = WavelengthBlock.completed.modify(_ + n)(wb.settle)
      // If there are no pending science datasets already but the cals at the
      // beginning.  Otherwise the missing cals should finish the block.
      if science.isEmpty then (cs ++ ss, wbʹ) else (ss ++ cs, wbʹ)

  end WavelengthBlock

  object WavelengthBlock:
    def init[D](steps: Steps[D]): WavelengthBlock[D] =
      WavelengthBlock(steps, List.empty, Map.empty, 0)

    def completed[D]: Lens[WavelengthBlock[D], Int] =
      Focus[WavelengthBlock[D]](_.completed)

  private case class ScienceState[D](
    blocks: List[WavelengthBlock[D]]
  ) extends Science2[D]:

    override def generate: Stream[Pure, ProtoAtom[ProtoStep[D]]] =
      Stream.empty

    override def record(step: StepRecord[D])(using Eq[D]): Science2[D] =
      copy(blocks = blocks.map(_.record(step)))

  end ScienceState

  private object ScienceState:
    def instantiate[F[_]: Monad, D, G, L, U](
      sc:       Steps.Computer[D, G, L, U],
      expander: SmartGcalExpander[F, D],
      config:   Config[G, L, U],
      time:     IntegrationTime
    ): F[Either[String, Science2[D]]] =
      EitherT(sc.compute(expander, config, time))
        .map(_.map(WavelengthBlock.init))
        .map(ScienceState(_))
        .value
  end ScienceState

  def gmosNorth[F[_]: Monad](
    expander: SmartGcalExpander[F, GmosNorth],
    config:   Config.GmosNorth,
    time:     IntegrationTime
  ): F[Either[String, Science2[GmosNorth]]] =
    ScienceState.instantiate(Steps.North, expander, config, time)

  def gmosSouth[F[_]: Monad](
    expander: SmartGcalExpander[F, GmosSouth],
    config:   Config.GmosSouth,
    time:     IntegrationTime
  ): F[Either[String, Science2[GmosSouth]]] =
    ScienceState.instantiate(Steps.South, expander, config, time)

end Science2