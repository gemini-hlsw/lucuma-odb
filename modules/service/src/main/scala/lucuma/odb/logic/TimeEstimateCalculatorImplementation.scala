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
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.core.refined.numeric.NonZeroInt
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.sequence.SetupTimeEstimateCalculator
import lucuma.odb.sequence.StepTimeEstimateCalculator
import lucuma.odb.sequence.data.ProtoStep
import skunk.Session

object TimeEstimateCalculatorImplementation:

  def fromSession[F[_]](s: Session[F], enums: Enums)(using MonadError[F, Throwable]): F[TimeEstimateCalculatorImplementation.ForInstrumentMode] =
    TimeEstimateContext.select(s, enums).map(fromContext)

  private def fromContext(ctx: TimeEstimateContext): ForInstrumentMode =
    new ForInstrumentMode(ctx)

  private def setupCalculatorfromEstimation(
    setup:             SetupTime,
    maxVisit:          TimeSpan,
  ): SetupTimeEstimateCalculator =
    new SetupTimeEstimateCalculator:
      override def estimateSetupTime: SetupTime =
        setup

      override def estimateSetupCount(scienceTime: TimeSpan): NonNegInt =
        if scienceTime.isZero then NonNegInt.MinValue
        else
          val OneQuarter: TimeSpan = maxVisit /| NonZeroInt.unsafeFrom(4)
          NonNegInt.unsafeFrom:
           (math.ceil((scienceTime -| OneQuarter).toMicroseconds.toDouble / maxVisit.toMicroseconds.toDouble) max 1.0).toInt

  private def stepCalculatorfromEstimators[S, D](
    configChange:      ConfigChangeEstimator[D],
    detectorEstimator: DetectorEstimator[S, D]
  ): StepTimeEstimateCalculator[S, D] =
    new StepTimeEstimateCalculator[S, D]:
      override def estimateStep(static: S, last: StepTimeEstimateCalculator.Last[D], next: ProtoStep[D]): StepEstimate =
        val c = configChange.estimate(last, next)
        val d = detectorEstimator.estimate(static, next)
        StepEstimate.fromMax(c, d)

  class ForInstrumentMode private[TimeEstimateCalculatorImplementation] (private val ctx: TimeEstimateContext):
    private val cce = ConfigChangeEstimator.using(ctx.enums)
    private val de  = DetectorEstimator.using(ctx)

    lazy val flamingos2LongSlitSetup: SetupTimeEstimateCalculator =
      setupCalculatorfromEstimation(
        SetupTime(
          ctx.enums.TimeEstimate.Flamingos2LongslitSetup.time,
          ctx.enums.TimeEstimate.Flamingos2Reacquisition.time
        ),
        ctx.enums.TimeEstimate.Flamingos2LongslitMaxVisit.time
      )

    lazy val gmosNorthImagingSetup: SetupTimeEstimateCalculator =
      setupCalculatorfromEstimation(
        SetupTime(
          ctx.enums.TimeEstimate.GmosNorthImagingSetup.time,
          ctx.enums.TimeEstimate.GmosNorthReacquisition.time
        ),
        ctx.enums.TimeEstimate.GmosNorthImagingMaxVisit.time
      )

    lazy val gmosNorthLongSlitSetup: SetupTimeEstimateCalculator =
      setupCalculatorfromEstimation(
        SetupTime(
          ctx.enums.TimeEstimate.GmosNorthLongslitSetup.time,
          ctx.enums.TimeEstimate.GmosNorthReacquisition.time
        ),
        ctx.enums.TimeEstimate.GmosNorthLongslitMaxVisit.time
      )

    lazy val gmosSouthImagingSetup: SetupTimeEstimateCalculator =
      setupCalculatorfromEstimation(
        SetupTime(
          ctx.enums.TimeEstimate.GmosSouthImagingSetup.time,
          ctx.enums.TimeEstimate.GmosSouthReacquisition.time
        ),
        ctx.enums.TimeEstimate.GmosSouthImagingMaxVisit.time
      )

    lazy val gmosSouthLongSlitSetup: SetupTimeEstimateCalculator =
      setupCalculatorfromEstimation(
        SetupTime(
          ctx.enums.TimeEstimate.GmosSouthLongslitSetup.time,
          ctx.enums.TimeEstimate.GmosSouthReacquisition.time
        ),
        ctx.enums.TimeEstimate.GmosSouthLongslitMaxVisit.time
      )

    lazy val igrins2LongSlitSetup: SetupTimeEstimateCalculator =
      setupCalculatorfromEstimation(
        SetupTime(
          ctx.enums.TimeEstimate.Igrins2LongslitSetup.time,
          ctx.enums.TimeEstimate.Igrins2Reacquisition.time
        ),
        ctx.enums.TimeEstimate.Igrins2LongslitMaxVisit.time
      )


    lazy val flamingos2Step: StepTimeEstimateCalculator[Flamingos2StaticConfig, Flamingos2DynamicConfig] =
      stepCalculatorfromEstimators(cce.flamingos2, de.flamingos2)

    lazy val ghostStep: StepTimeEstimateCalculator[GhostStaticConfig, GhostDynamicConfig] =
      stepCalculatorfromEstimators(cce.ghost, de.ghost)

    lazy val gmosNorthStep: StepTimeEstimateCalculator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth] =
      stepCalculatorfromEstimators(cce.gmosNorth, de.gmosNorth)

    lazy val gmosSouthStep: StepTimeEstimateCalculator[StaticConfig.GmosSouth, DynamicConfig.GmosSouth] =
      stepCalculatorfromEstimators(cce.gmosSouth, de.gmosSouth)

    lazy val igrins2Step: StepTimeEstimateCalculator[Igrins2StaticConfig, Igrins2DynamicConfig] =
      stepCalculatorfromEstimators(cce.igrins2, de.igrins2)