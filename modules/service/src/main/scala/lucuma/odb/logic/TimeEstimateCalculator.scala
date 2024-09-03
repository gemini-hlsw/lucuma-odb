// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.MonadError
import cats.syntax.functor.*
import cats.syntax.traverse.*
import fs2.Pipe
import fs2.Pure
import lucuma.core.model.sequence.SetupTime
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import skunk.Session

trait TimeEstimateCalculator[S, D] {
  def estimateSetup: SetupTime

  def estimateSequence(static: S): Pipe[Pure, (ProtoAtom[(ProtoStep[D], Int)], Int), (ProtoAtom[(ProtoStep[D], Int, StepEstimate)], Int)]

  def estimateStep(static: S, state: EstimatorState[D], next: ProtoStep[D]): StepEstimate

}

object TimeEstimateCalculator {

  def fromSession[F[_]](s: Session[F], enums: Enums)(using MonadError[F, Throwable]): F[TimeEstimateCalculator.ForInstrumentMode] =
    TimeEstimateContext.select(s, enums).map(fromContext)

  private def fromContext(ctx: TimeEstimateContext): ForInstrumentMode =
    new ForInstrumentMode(ctx)

  private def fromEstimators[S, D](
    setup:             SetupTime,
    configChange:      ConfigChangeEstimator[D],
    detectorEstimator: DetectorEstimator[S, D]
  ): TimeEstimateCalculator[S, D] =
    new TimeEstimateCalculator[S, D] {
      def estimateSetup: SetupTime =
        setup

      def estimateStep(static: S, past: EstimatorState[D], next: ProtoStep[D]): StepEstimate = {
        val c = configChange.estimate(past, next)
        val d = detectorEstimator.estimate(static, next)
        StepEstimate.fromMax(c, d)
      }

      def estimateSequence(static: S): Pipe[Pure, (ProtoAtom[(ProtoStep[D], Int)], Int), (ProtoAtom[(ProtoStep[D], Int, StepEstimate)], Int)] =
        _.mapAccumulate(EstimatorState.empty[D]) { case (s, (atom, aix)) =>
           val sa = atom.mapAccumulate(s) { case (sʹ, (step, six)) =>
             (sʹ.next(step), (step, six, estimateStep(static, sʹ, step)))
           }
           sa.tupleRight(aix)
        }.map(_._2)  // discard the state
    }

  class ForInstrumentMode private[TimeEstimateCalculator] (private val ctx: TimeEstimateContext) {
    private val cce = ConfigChangeEstimator.using(ctx.enums)
    private val de  = DetectorEstimator.using(ctx)

    lazy val gmosNorth: TimeEstimateCalculator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth] =
      fromEstimators(
        SetupTime(
          ctx.enums.TimeEstimate.GmosNorthLongslitSetup.time,
          ctx.enums.TimeEstimate.GmosNorthReacquisition.time
        ),
        cce.gmosNorth,
        de.gmosNorth
      )

    lazy val gmosSouth: TimeEstimateCalculator[StaticConfig.GmosSouth, DynamicConfig.GmosSouth] =
      fromEstimators(
        SetupTime(
          ctx.enums.TimeEstimate.GmosSouthLongslitSetup.time,
          ctx.enums.TimeEstimate.GmosSouthReacquisition.time
        ),
        cce.gmosSouth,
        de.gmosSouth
      )
  }

}
