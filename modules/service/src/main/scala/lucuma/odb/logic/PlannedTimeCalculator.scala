// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.MonadError
import cats.data.NonEmptyList
import cats.syntax.functor.*
import cats.syntax.traverse.*
import fs2.Pipe
import fs2.Stream
import lucuma.core.data.Zipper
import lucuma.core.model.sequence.SetupTime
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import skunk.Session

trait PlannedTimeCalculator[S, D] {
  def estimateSetup: SetupTime

  def estimateSequence[F[_]](static: S): Pipe[F, ProtoAtom[ProtoStep[D]], ProtoAtom[ProtoStep[(D, StepEstimate)]]]
}

object PlannedTimeCalculator {

  def fromSession[F[_]](s: Session[F], enums: Enums)(using MonadError[F, Throwable]): F[PlannedTimeCalculator.ForInstrumentMode] =
    PlannedTimeContext.select(s, enums).map(fromContext)

  def fromContext(ctx: PlannedTimeContext): ForInstrumentMode =
    new ForInstrumentMode(ctx)

  def fromEstimators[S, D](
    setup:             SetupTime,
    configChange:      ConfigChangeEstimator[D],
    detectorEstimator: DetectorEstimator[S, D]
  ): PlannedTimeCalculator[S, D] =
    new PlannedTimeCalculator[S, D] {
      def estimateSetup: SetupTime =
        setup

      def estimateSequence[F[_]](static: S): Pipe[F, ProtoAtom[ProtoStep[D]], ProtoAtom[ProtoStep[(D, StepEstimate)]]] =
        _.mapAccumulate(EstimatorState.empty[D]) { (s, a) =>
          a.mapAccumulate(s) { (sʹ, step) =>
            val c = configChange.estimate(sʹ, step)
            val d = detectorEstimator.estimate(static, step)
            (sʹ.next(step), step.tupleRight(StepEstimate.fromMax(c, d)))
          }
        }.map(_._2)
    }

  class ForInstrumentMode private[PlannedTimeCalculator] (private val ctx: PlannedTimeContext) {
    private val cce = ConfigChangeEstimator.using(ctx.enums)
    private val de  = DetectorEstimator.using(ctx)

    lazy val gmosNorth: PlannedTimeCalculator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth] =
      fromEstimators(
        SetupTime(
          ctx.enums.TimeEstimate.GmosNorthLongslitSetup.time,
          ctx.enums.TimeEstimate.GmosNorthReacquisition.time
        ),
        cce.gmosNorth,
        de.gmosNorth
      )

    lazy val gmosSouth: PlannedTimeCalculator[StaticConfig.GmosSouth, DynamicConfig.GmosSouth] =
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
