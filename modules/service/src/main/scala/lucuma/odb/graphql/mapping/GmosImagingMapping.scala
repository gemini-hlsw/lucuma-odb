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

  private object CommonImagingFields:

    val bin: FieldMapping        = explicitOrElseDefault[GmosBinning]("bin", "explicitBin", "defaultBin")
    val defaultBin: FieldMapping = CursorField[GmosBinning]("defaultBin", _ => Result(DefaultBin))


    val ampReadMode: FieldMapping        = explicitOrElseDefault[GmosAmpReadMode]("ampReadMode", "explicitAmpReadMode", "defaultAmpReadMode")
    val defaultAmpReadMode: FieldMapping = CursorField[GmosAmpReadMode]("defaultAmpReadMode", _ => Result(DefaultAmpReadMode))

    val ampGain: FieldMapping        = explicitOrElseDefault[GmosAmpGain]("ampGain", "explicitAmpGain", "defaultAmpGain")
    val defaultAmpGain: FieldMapping = CursorField[GmosAmpGain]("defaultAmpGain", _ => Result(DefaultAmpGain))

    val roi: FieldMapping        = explicitOrElseDefault[GmosRoi]("roi", "explicitRoi", "defaultRoi")
    val defaultRoi: FieldMapping = CursorField[GmosRoi]("defaultRoi", _ => Result(DefaultRoi))

  lazy val GmosNorthImagingMapping: ObjectMapping =
    ObjectMapping(GmosNorthImagingType)(

      SqlField("observationId", GmosNorthImagingView.ObservationId, key = true, hidden = true),
      SqlField("filters", GmosNorthImagingView.Filters),

      CommonImagingFields.bin,
      SqlField("explicitBin", GmosNorthImagingView.ExplicitBin),
      CommonImagingFields.defaultBin,

      CommonImagingFields.ampReadMode,
      SqlField("explicitAmpReadMode", GmosNorthImagingView.ExplicitAmpReadMode),
      CommonImagingFields.defaultAmpReadMode,

      CommonImagingFields.ampGain,
      SqlField("explicitAmpGain", GmosNorthImagingView.ExplicitAmpGain),
      CommonImagingFields.defaultAmpGain,

      CommonImagingFields.roi,
      SqlField("explicitRoi", GmosNorthImagingView.ExplicitRoi),
      CommonImagingFields.defaultRoi
    )

  lazy val GmosSouthImagingMapping: ObjectMapping =
    ObjectMapping(GmosSouthImagingType)(

      SqlField("observationId", GmosSouthImagingView.ObservationId, key = true, hidden = true),
      SqlField("filters", GmosSouthImagingView.Filters),

      CommonImagingFields.bin,
      SqlField("explicitBin", GmosSouthImagingView.ExplicitBin),
      CommonImagingFields.defaultBin,

      CommonImagingFields.ampReadMode,
      SqlField("explicitAmpReadMode", GmosSouthImagingView.ExplicitAmpReadMode),
      CommonImagingFields.defaultAmpReadMode,

      CommonImagingFields.ampGain,
      SqlField("explicitAmpGain", GmosSouthImagingView.ExplicitAmpGain),
      CommonImagingFields.defaultAmpGain,

      CommonImagingFields.roi,
      SqlField("explicitRoi", GmosSouthImagingView.ExplicitRoi),
      CommonImagingFields.defaultRoi
    )
}

object GmosImagingMapping:

  private val DefaultBin: GmosBinning = GmosBinning.One
  private val DefaultAmpReadMode: GmosAmpReadMode = GmosAmpReadMode.Slow
  private val DefaultAmpGain: GmosAmpGain = GmosAmpGain.Low
  private val DefaultRoi: GmosRoi = GmosRoi.FullFrame

