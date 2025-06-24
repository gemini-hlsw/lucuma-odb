// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Result
import grackle.skunk.SkunkMapping
import io.circe.*
import io.circe.syntax.*
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.GmosRoi
import lucuma.core.math.Offset
import lucuma.odb.format.spatialOffsets.*
import lucuma.odb.graphql.table.*
import lucuma.odb.json.offset.query.given

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

    val spatialOffsets: FieldMapping =
      CursorFieldJson("spatialOffsets",
        cursor =>
          cursor
            .field("spatialOffsetsString", None)
            .flatMap(_.as[Option[String]].map(_.map(decodeSpatialOffsets)))
            .map(_.getOrElse(defaultSpatialOffsetsJson)),
        List("explicitSpatialOffsets", "defaultSpatialOffsets")
      )

    val explicitSpatialOffsets: FieldMapping =
      CursorFieldJson("explicitSpatialOffsets",
        cursor =>
          cursor
            .field("spatialOffsetsString", None)
            .flatMap(_.as[Option[String]].map(_.fold(defaultSpatialOffsetsJson)(decodeSpatialOffsets))),
        List("spatialOffsetsString")
      )

    val defaultSpatialOffsets: FieldMapping =
      CursorFieldJson("defaultSpatialOffsets", _ => Result(defaultSpatialOffsetsJson), Nil)

  lazy val GmosNorthImagingMapping: ObjectMapping =
    ObjectMapping(GmosNorthImagingType)(

      SqlField("observationId", GmosNorthImagingView.ObservationId, key = true, hidden = true),
      SqlField("filters", GmosNorthImagingView.Filters),
      SqlField("initialFilters", GmosNorthImagingView.InitialFilters),

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
      CommonImagingFields.defaultRoi,

      SqlField("spatialOffsetsString", GmosNorthImagingView.ExplicitSpatialOffsets, hidden = true),
      CommonImagingFields.spatialOffsets,
      CommonImagingFields.explicitSpatialOffsets,
      CommonImagingFields.defaultSpatialOffsets,
    )

  lazy val GmosSouthImagingMapping: ObjectMapping =
    ObjectMapping(GmosSouthImagingType)(

      SqlField("observationId", GmosSouthImagingView.ObservationId, key = true, hidden = true),
      SqlField("filters", GmosSouthImagingView.Filters),
      SqlField("initialFilters", GmosSouthImagingView.InitialFilters),

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
      CommonImagingFields.defaultRoi,

      SqlField("spatialOffsetsString", GmosSouthImagingView.ExplicitSpatialOffsets, hidden = true),
      CommonImagingFields.spatialOffsets,
      CommonImagingFields.explicitSpatialOffsets,
      CommonImagingFields.defaultSpatialOffsets,
    )
}

object GmosImagingMapping:

  private val DefaultBin: GmosBinning = GmosBinning.Two
  private val DefaultAmpReadMode: GmosAmpReadMode = GmosAmpReadMode.Slow
  private val DefaultAmpGain: GmosAmpGain = GmosAmpGain.Low
  private val DefaultRoi: GmosRoi = GmosRoi.FullFrame

  def decodeSpatialOffsets(s: String): Json =
    if (s.trim.isEmpty) List.empty[Offset].asJson
    else OffsetsFormat.getOption(s).getOrElse(List.empty[Offset]).asJson

  val defaultSpatialOffsetsJson: Json =
    List.empty[Offset].asJson

