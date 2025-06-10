// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Result
import grackle.skunk.SkunkMapping
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.odb.graphql.table.*
import lucuma.core.enums.GmosBinning

trait GmosImagingMapping[F[_]]
  extends GmosImagingView[F] with OptionalFieldMapping[F] { this: SkunkMapping[F] =>

  import GmosImagingMapping.*

  private class CommonImagingMapping(cc: CommonImagingColumns) {
      val defaultBin = SqlField("defaultBin", cc.BinDefault)
      val explicitBin = SqlField("explicitBin", cc.Bin)
      val bin = explicitOrElseDefault[GmosBinning]("bin", "explicitBin", "defaultBin")


      val roi: FieldMapping                 = explicitOrElseDefault[GmosRoi]("roi", "explicitRoi", "defaultRoi")
      val defaultRoi: FieldMapping          = CursorField[GmosRoi]("defaultRoi", _ => Result(DefaultRoi))
      val explicitRoi: FieldMapping         = SqlField("explicitRoi", cc.Roi)

      val ampReadMode: FieldMapping         = explicitOrElseDefault[GmosAmpReadMode]("ampReadMode", "explicitAmpReadMode", "defaultAmpReadMode")
      val defaultAmpReadMode: FieldMapping  = CursorField[GmosAmpReadMode]("defaultAmpReadMode", _ => Result(DefaultAmpReadMode))
      val explicitAmpReadMode: FieldMapping = SqlField("explicitAmpReadMode", cc.AmpReadMode)

      val ampGain: FieldMapping             = explicitOrElseDefault[GmosAmpGain]("ampGain", "explicitAmpGain", "defaultAmpGain")
      val defaultAmpGain: FieldMapping      = CursorField[GmosAmpGain]("defaultAmpGain", _ => Result(DefaultAmpGain))
      val explicitAmpGain: FieldMapping     = SqlField("explicitAmpGain", cc.AmpGain)
  }

  lazy val GmosNorthImagingMapping: ObjectMapping =
    val common = new CommonImagingMapping(GmosNorthImagingView.Common)

    ObjectMapping(GmosNorthImagingType)(

      SqlField("observationId", GmosNorthImagingView.ObservationId, key = true, hidden = true),

      common.bin,
      common.defaultBin,
      common.explicitBin,

      common.roi,
      common.defaultRoi,
      common.explicitRoi,

      common.ampReadMode,
      common.defaultAmpReadMode,
      common.explicitAmpReadMode,

      common.ampGain,
      common.defaultAmpGain,
      common.explicitAmpGain,

      SqlField("filters", GmosNorthImagingView.Filters),

    )

  lazy val GmosSouthImagingMapping: ObjectMapping =
    val common = new CommonImagingMapping(GmosNorthImagingView.Common)

    ObjectMapping(GmosSouthImagingType)(

      SqlField("observationId", GmosSouthImagingView.ObservationId, key = true, hidden = true),
      common.bin,
      common.defaultBin,
      common.explicitBin,

      common.roi,
      common.defaultRoi,
      common.explicitRoi,

      common.ampReadMode,
      common.defaultAmpReadMode,
      common.explicitAmpReadMode,

      common.ampGain,
      common.defaultAmpGain,
      common.explicitAmpGain,

      SqlField("filters", GmosSouthImagingView.Filters),
  //
  //     // Binning (explicit override or default)
  //     SqlField("defaultBin", cc.BinDefault),
  //     SqlField("explicitBin", cc.XBin),
  //     explicitOrElseDefault[GmosBinning]("bin", "explicitBin", "defaultBin")
  //
  //     explicitOrElseDefault[GmosYBinning]("yBin", "explicitYBin", "defaultYBin"),
  //     SqlField("explicitYBin", GmosSouthImagingView.ExplicitBin),
  //     CursorField[GmosYBinning]("defaultYBin", _ => Result(DefaultYBin)),
  //
  //     // AmpReadMode (explicit override or default)
  //     explicitOrElseDefault[GmosAmpReadMode]("ampReadMode", "explicitAmpReadMode", "defaultAmpReadMode"),
  //     SqlField("explicitAmpReadMode", GmosSouthImagingView.ExplicitAmpReadMode),
  //     CursorField[GmosAmpReadMode]("defaultAmpReadMode", _ => Result(DefaultAmpReadMode)),
  //
  //     // AmpGain (explicit override or default)
  //     explicitOrElseDefault[GmosAmpGain]("ampGain", "explicitAmpGain", "defaultAmpGain"),
  //     SqlField("explicitAmpGain", GmosSouthImagingView.ExplicitAmpGain),
  //     CursorField[GmosAmpGain]("defaultAmpGain", _ => Result(DefaultAmpGain)),
  //
  //     // ROI (explicit override or default)
  //     explicitOrElseDefault[GmosRoi]("roi", "explicitRoi", "defaultRoi"),
  //     SqlField("explicitRoi", GmosSouthImagingView.ExplicitRoi),
  //     CursorField[GmosRoi]("defaultRoi", _ => Result(DefaultRoi))
    )
}

object GmosImagingMapping {

  // Default values for GMOS Imaging modes
  private val DefaultXBin: GmosXBinning = GmosXBinning.One
  private val DefaultYBin: GmosYBinning = GmosYBinning.One
  private val DefaultAmpReadMode: GmosAmpReadMode = GmosAmpReadMode.Slow
  private val DefaultAmpGain: GmosAmpGain = GmosAmpGain.Low
  private val DefaultRoi: GmosRoi = GmosRoi.FullFrame

}
