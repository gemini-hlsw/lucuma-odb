// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Result
import grackle.skunk.SkunkMapping
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.GmosRoi
import lucuma.odb.graphql.table.*

trait GmosImagingMapping[F[_]]
  extends GmosImagingView[F] with OptionalFieldMapping[F] { this: SkunkMapping[F] =>

  import GmosImagingMapping.*

  lazy val GmosNorthImagingMapping: ObjectMapping =
    ObjectMapping(GmosNorthImagingType)(

      SqlField("observationId", GmosNorthImagingView.ObservationId, key = true, hidden = true),

      SqlField("filters", GmosNorthImagingView.Filters),

      // Binning (explicit override or default)
      explicitOrElseDefault[GmosBinning]("bin", "explicitBin", "defaultBin"),
      SqlField("explicitBin", GmosNorthImagingView.ExplicitBin),
      CursorField[GmosBinning]("defaultBin", _ => Result(DefaultBin)),

      // AmpReadMode (explicit override or default)
      explicitOrElseDefault[GmosAmpReadMode]("ampReadMode", "explicitAmpReadMode", "defaultAmpReadMode"),
      SqlField("explicitAmpReadMode", GmosNorthImagingView.ExplicitAmpReadMode),
      CursorField[GmosAmpReadMode]("defaultAmpReadMode", _ => Result(DefaultAmpReadMode)),

      // AmpGain (explicit override or default)
      explicitOrElseDefault[GmosAmpGain]("ampGain", "explicitAmpGain", "defaultAmpGain"),
      SqlField("explicitAmpGain", GmosNorthImagingView.ExplicitAmpGain),
      CursorField[GmosAmpGain]("defaultAmpGain", _ => Result(DefaultAmpGain)),

      // ROI (explicit override or default)
      explicitOrElseDefault[GmosRoi]("roi", "explicitRoi", "defaultRoi"),
      SqlField("explicitRoi", GmosNorthImagingView.ExplicitRoi),
      CursorField[GmosRoi]("defaultRoi", _ => Result(DefaultRoi))
    )

  lazy val GmosSouthImagingMapping: ObjectMapping =
    ObjectMapping(GmosSouthImagingType)(

      SqlField("observationId", GmosSouthImagingView.ObservationId, key = true, hidden = true),

      SqlField("filters", GmosSouthImagingView.Filters),

      // Binning (explicit override or default)
      explicitOrElseDefault[GmosBinning]("bin", "explicitBin", "defaultBin"),
      SqlField("explicitBin", GmosSouthImagingView.ExplicitBin),
      CursorField[GmosBinning]("defaultBin", _ => Result(DefaultBin)),

      // AmpReadMode (explicit override or default)
      explicitOrElseDefault[GmosAmpReadMode]("ampReadMode", "explicitAmpReadMode", "defaultAmpReadMode"),
      SqlField("explicitAmpReadMode", GmosSouthImagingView.ExplicitAmpReadMode),
      CursorField[GmosAmpReadMode]("defaultAmpReadMode", _ => Result(DefaultAmpReadMode)),

      // AmpGain (explicit override or default)
      explicitOrElseDefault[GmosAmpGain]("ampGain", "explicitAmpGain", "defaultAmpGain"),
      SqlField("explicitAmpGain", GmosSouthImagingView.ExplicitAmpGain),
      CursorField[GmosAmpGain]("defaultAmpGain", _ => Result(DefaultAmpGain)),

      // ROI (explicit override or default)
      explicitOrElseDefault[GmosRoi]("roi", "explicitRoi", "defaultRoi"),
      SqlField("explicitRoi", GmosSouthImagingView.ExplicitRoi),
      CursorField[GmosRoi]("defaultRoi", _ => Result(DefaultRoi))
    )
}

object GmosImagingMapping {

  // Default values for GMOS Imaging modes
  private val DefaultBin: GmosBinning = GmosBinning.One
  private val DefaultAmpReadMode: GmosAmpReadMode = GmosAmpReadMode.Slow
  private val DefaultAmpGain: GmosAmpGain = GmosAmpGain.Low
  private val DefaultRoi: GmosRoi = GmosRoi.FullFrame

}
