// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.syntax.either.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.model.sequence.DatasetEstimate
import lucuma.core.model.sequence.DetectorEstimate
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.odb.sequence.data.ProtoStep

/**
 * Estimates the detector exposure, readout, and write times for the current
 * `step`.
 *
 * @tparam S instrument static configuration type
 * @tparam D instrument dyanmic configuration type
 */
trait DetectorEstimator[S, D] {

  def estimate(static: S, step: ProtoStep[D]): List[DetectorEstimate]

}

object DetectorEstimator {

  def using(ctx: TimeEstimateContext): Applied =
    new Applied(ctx)

  class Applied private[DetectorEstimator] (private val ctx: TimeEstimateContext) {

    extension (f2: Flamingos2DynamicConfig) {
      def datasetEstimate: DatasetEstimate =
        DatasetEstimate(
          f2.exposure,
          f2.readMode match
            case Flamingos2ReadMode.Bright => ctx.enums.TimeEstimate.Flamingos2BrightReadout.time
            case Flamingos2ReadMode.Medium => ctx.enums.TimeEstimate.Flamingos2MediumReadout.time
            case Flamingos2ReadMode.Faint  => ctx.enums.TimeEstimate.Flamingos2FaintReadout.time,
          ctx.enums.TimeEstimate.Flamingos2Write.time
        )
    }

    extension (gn: DynamicConfig.GmosNorth) {
      def readoutKey(detector: GmosNorthDetector): GmosReadoutTime.Key =
        GmosReadoutTime.Key(detector.asLeft, gn.readout, gn.roi)

      def datasetEstimate(detector: GmosNorthDetector): DatasetEstimate =
        DatasetEstimate(
          gn.exposure,
          ctx.gmosReadout(readoutKey(detector)),
          ctx.enums.TimeEstimate.GmosNorthWrite.time
        )
    }

    extension (gs: DynamicConfig.GmosSouth) {
      def readoutKey(detector: GmosSouthDetector): GmosReadoutTime.Key =
        GmosReadoutTime.Key(detector.asRight, gs.readout, gs.roi)

      def datasetEstimate(detector: GmosSouthDetector): DatasetEstimate =
        DatasetEstimate(
          gs.exposure,
          ctx.gmosReadout(readoutKey(detector)),
          ctx.enums.TimeEstimate.GmosSouthWrite.time
        )
    }

    lazy val flamingos2: DetectorEstimator[Flamingos2StaticConfig, Flamingos2DynamicConfig] =
      (_: Flamingos2StaticConfig, step: ProtoStep[Flamingos2DynamicConfig]) => List(
        DetectorEstimate(
          "Flamingos2",
          s"Flamingos 2 Detector Array",
          step.value.datasetEstimate,
          NonNegInt.unsafeFrom(1)
        )
      )

    lazy val gmosNorth: DetectorEstimator[StaticConfig.GmosNorth, DynamicConfig.GmosNorth] =
      (static: StaticConfig.GmosNorth, step: ProtoStep[DynamicConfig.GmosNorth]) => List(
        DetectorEstimate(
          "GMOS North",
          s"GMOS North ${static.detector.shortName} Detector Array",
          step.value.datasetEstimate(static.detector),
          NonNegInt.unsafeFrom(1)
        )
      )

    lazy val gmosSouth: DetectorEstimator[StaticConfig.GmosSouth, DynamicConfig.GmosSouth] =
      (static: StaticConfig.GmosSouth, step: ProtoStep[DynamicConfig.GmosSouth]) => List(
        DetectorEstimate(
          "GMOS South",
          s"GMOS South ${static.detector.shortName} Detector Array",
          step.value.datasetEstimate(static.detector),
          NonNegInt.unsafeFrom(1)
        )
      )


  }

}
