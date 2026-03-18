// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.MonadError
import cats.syntax.functor.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.model.sequence.SetupTime
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.core.refined.numeric.NonZeroInt
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.sequence.TimeEstimateCalculator
import lucuma.odb.sequence.data.ProtoStep
import skunk.Session

object TimeEstimateCalculatorImplementation:

  def fromSession[F[_]](s: Session[F], enums: Enums)(using MonadError[F, Throwable]): F[TimeEstimateCalculatorImplementation.ForInstrumentMode] =
    TimeEstimateContext.select(s, enums).map(fromContext)

  private def fromContext(ctx: TimeEstimateContext): ForInstrumentMode =
    new ForInstrumentMode(ctx)

  private def fromEstimators[S, D](
    setup:             SetupTime,
    maxVisit:          TimeSpan,
    configChange:      ConfigChangeEstimator[D],
    detectorEstimator: DetectorEstimator[S, D]
  ): TimeEstimateCalculator[S, D] =
    new TimeEstimateCalculator[S, D]:
      override def estimateSetup: SetupTime =
        setup

      override def estimateSetupCount(scienceTime: TimeSpan): NonNegInt =
        if scienceTime.isZero then NonNegInt.MinValue
        else
          val OneQuarter: TimeSpan = maxVisit /| NonZeroInt.unsafeFrom(4)
          NonNegInt.unsafeFrom:
           (math.ceil((scienceTime -| OneQuarter).toMicroseconds.toDouble / maxVisit.toMicroseconds.toDouble) max 1.0).toInt

      override def estimateStep(static: S, last: TimeEstimateCalculator.Last[D], next: ProtoStep[D]): StepEstimate = {
        val c = configChange.estimate(last, next)
        val d = detectorEstimator.estimate(static, next)
        StepEstimate.fromMax(c, d)
      }

  class ForInstrumentMode private[TimeEstimateCalculatorImplementation] (private val ctx: TimeEstimateContext):
    private val cce = ConfigChangeEstimator.using(ctx.enums)
    private val de  = DetectorEstimator.using(ctx)

    lazy val igrins2: TimeEstimateCalculator[Igrins2StaticConfig, Igrins2DynamicConfig] =
      fromEstimators(
        SetupTime(
          ctx.enums.TimeEstimate.Igrins2LongslitSetup.time,
          ctx.enums.TimeEstimate.Igrins2Reacquisition.time
        ),
        ctx.enums.TimeEstimate.Igrins2LongslitMaxVisit.time,
        cce.igrins2,
        de.igrins2
      )

    lazy val flamingos2: TimeEstimateCalculator[Flamingos2StaticConfig, Flamingos2DynamicConfig] =
      fromEstimators(
        SetupTime(
          ctx.enums.TimeEstimate.Flamingos2LongslitSetup.time,
          ctx.enums.TimeEstimate.Flamingos2Reacquisition.time
        ),
        ctx.enums.TimeEstimate.Flamingos2LongslitMaxVisit.time,
        cce.flamingos2,
        de.flamingos2
      )

    lazy val gmosNorthImaging: TimeEstimateCalculator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth] =
      fromEstimators(
        SetupTime(
          ctx.enums.TimeEstimate.GmosNorthImagingSetup.time,
          ctx.enums.TimeEstimate.GmosNorthReacquisition.time
        ),
        ctx.enums.TimeEstimate.GmosNorthImagingMaxVisit.time,
        cce.gmosNorth,
        de.gmosNorth
      )

    lazy val gmosNorthLongSlit: TimeEstimateCalculator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth] =
      fromEstimators(
        SetupTime(
          ctx.enums.TimeEstimate.GmosNorthLongslitSetup.time,
          ctx.enums.TimeEstimate.GmosNorthReacquisition.time
        ),
        ctx.enums.TimeEstimate.GmosNorthLongslitMaxVisit.time,
        cce.gmosNorth,
        de.gmosNorth
      )

    lazy val gmosSouthImaging: TimeEstimateCalculator[StaticConfig.GmosSouth, DynamicConfig.GmosSouth] =
      fromEstimators(
        SetupTime(
          ctx.enums.TimeEstimate.GmosSouthImagingSetup.time,
          ctx.enums.TimeEstimate.GmosSouthReacquisition.time
        ),
        ctx.enums.TimeEstimate.GmosSouthImagingMaxVisit.time,
        cce.gmosSouth,
        de.gmosSouth
      )

    lazy val gmosSouthLongSlit: TimeEstimateCalculator[StaticConfig.GmosSouth, DynamicConfig.GmosSouth] =
      fromEstimators(
        SetupTime(
          ctx.enums.TimeEstimate.GmosSouthLongslitSetup.time,
          ctx.enums.TimeEstimate.GmosSouthReacquisition.time
        ),
        ctx.enums.TimeEstimate.GmosSouthLongslitMaxVisit.time,
        cce.gmosSouth,
        de.gmosSouth
      )
