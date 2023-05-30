// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.Eq
import cats.syntax.eq.*
import cats.syntax.functorFilter.*
import cats.syntax.option.*
import lucuma.core.enums.StepType
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.sequence.ConfigChangeEstimate
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.sequence.data.ProtoStep

/**
 * Estimates the configuration change cost from the previous step to the
 * `present` one.
 *
 * @tparam D instrument dynamic configuration type
 */
trait ConfigChangeEstimator[D] {

  def estimate(past: EstimatorState[D], present: ProtoStep[D]): List[ConfigChangeEstimate]

}

object ConfigChangeEstimator {

  def using(e: Enums): Applied =
    new Applied(e)

  extension (tc: Enums#TimeEstimate) {
    def toConfigChange: ConfigChangeEstimate =
      ConfigChangeEstimate(tc.name, tc.description, tc.time)
  }

  class Applied private[ConfigChangeEstimator] (private val enums: Enums) {

    private abstract class ForInstrument[D] extends ConfigChangeEstimator[D] {

      def check[A: Eq](estimate: Enums#TimeEstimate, past: EstimatorState[D], present: ProtoStep[D])(f: D => A): Option[ConfigChangeEstimate] =
        Option.when(past.step.map(s => f(s.value)).exists(_ =!= f(present.value)))(
          estimate.toConfigChange
        )

      def instrumentChecks(past: EstimatorState[D], present: ProtoStep[D]): List[Option[ConfigChangeEstimate]]

      def estimate(past: EstimatorState[D], present: ProtoStep[D]): List[ConfigChangeEstimate] =
        instrumentChecks(past, present).flattenOption ++ gcal(past, present) ++ offset(past, present)

    }

    lazy val gmosNorth: ConfigChangeEstimator[DynamicConfig.GmosNorth] =
      new ForInstrument[DynamicConfig.GmosNorth] {
        override def instrumentChecks(past: EstimatorState[DynamicConfig.GmosNorth], present: ProtoStep[DynamicConfig.GmosNorth]): List[Option[ConfigChangeEstimate]] =
          List(
            check(enums.TimeEstimate.GmosNorthFilter, past, present)(_.filter),
            check(enums.TimeEstimate.GmosNorthFpu, past, present)(_.fpu),
            check(enums.TimeEstimate.GmosNorthDisperser, past, present)(_.gratingConfig.map(_.grating))
          )
      }


    lazy val gmosSouth: ConfigChangeEstimator[DynamicConfig.GmosSouth] =
      new ForInstrument[DynamicConfig.GmosSouth] {
        override def instrumentChecks(past: EstimatorState[DynamicConfig.GmosSouth], present: ProtoStep[DynamicConfig.GmosSouth]): List[Option[ConfigChangeEstimate]] =
          List(
            check(enums.TimeEstimate.GmosSouthFilter, past, present)(_.filter),
            check(enums.TimeEstimate.GmosSouthFpu, past, present)(_.fpu),
            check(enums.TimeEstimate.GmosSouthDisperser, past, present)(_.gratingConfig.map(_.grating))
          )
      }


    private def gcal[D](past: EstimatorState[D], present: ProtoStep[D]): List[ConfigChangeEstimate] = {
      val scienceFold = Option.unless(past.step.exists(_.stepConfig.usesGcalUnit) === present.stepConfig.usesGcalUnit)(
        enums.TimeEstimate.ScienceFold.toConfigChange
      )

      val gcal = (past.gcal, present.stepConfig) match {
        case (Some(p), c@StepConfig.Gcal(_, _, _, _)) =>

          def est[A: Eq](estimate: enums.TimeEstimate)(f: StepConfig.Gcal => A): Option[ConfigChangeEstimate] =
            Option.unless(f(p) === f(c))(estimate.toConfigChange)

          List(
            est(enums.TimeEstimate.GcalDiffuser)(_.diffuser),
            est(enums.TimeEstimate.GcalFilter)(_.filter),
            est(enums.TimeEstimate.GcalShutter)(_.shutter)
          )

        case _                                        =>
          Nil
      }

      (scienceFold :: gcal).flattenOption
    }

    private def offset[D](past: EstimatorState[D], present: ProtoStep[D]): List[ConfigChangeEstimate] = {
      val prevOffset = past.science.fold(Offset.Zero)(_.offset)
      present.stepConfig match {
        case StepConfig.Science(curOffset) =>
          Option.when(prevOffset =!= curOffset) {
            val const  = enums.TimeEstimate.OffsetConstant.time
            val perArc = enums.TimeEstimate.OffsetDistance.time
            val dist   = perArc *| Angle.decimalArcseconds.get(prevOffset.distance(curOffset))
            ConfigChangeEstimate(
              "Offset",
              f"Offset cost, ${const.toSeconds}%.0f (constant) + ${dist.toSeconds}%.4f (distance)",
              const +| dist
            )
          }.toList
        case _                             =>
          Nil
      }
    }

  }

}
