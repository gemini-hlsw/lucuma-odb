// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.Order
import grackle.Path
import grackle.Query.Binding
import grackle.Query.Filter
import grackle.Query.OrderBy
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import io.circe.*
import io.circe.syntax.*
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.MultipleFiltersMode
import lucuma.core.math.Offset
import lucuma.odb.data.ObservingModeRowVersion
import lucuma.odb.format.spatialOffsets.*
import lucuma.odb.graphql.predicate.LeafPredicates
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*
import lucuma.odb.json.offset.query.given

trait GmosImagingMapping[F[_]] extends GmosImagingView[F]
                                  with GmosImagingFilterTable[F]
                                  with OffsetGeneratorView[F]
                                  with OptionalFieldMapping[F]
                                  with Predicates[F] { this: SkunkMapping[F] =>

  import GmosImagingMapping.*

  def gmosGroupedFilterImagingMapping(
    path:           Path,
    offsetsJoinCol: ColumnRef,
    cols:           GmosImagingCommonColumns
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("observationId", cols.Grouped.ObservationId, key = true, hidden = true),
      SqlField("order",         cols.Grouped.WavelengthOrder),
      SqlField("skyCount",      cols.Sky.Count),
      SqlObject("offsets",      Join(offsetsJoinCol, OffsetGeneratorView.ObjectObservationId)),
      SqlObject("skyOffsets",   Join(offsetsJoinCol, OffsetGeneratorView.SkyObservationId)),
    )

  def gmosPreImagingMapping(
    path: Path,
    cols: GmosImagingCommonColumns
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("observationId", cols.PreImaging.ObservationId, key = true, hidden = true),
      SqlObject("offset1"),
      SqlObject("offset2"),
      SqlObject("offset3"),
      SqlObject("offset4")
    )

  def gmosImagingVariantMapping(
    path: Path,
    cols: GmosImagingCommonColumns
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("observationId", cols.ObservationId, key = true, hidden = true),
      SqlField("variantType", cols.ImagingType),
      SqlObject("grouped"),
      SqlObject("preImaging")
    )

  private object CommonImagingFields:

    val multipleFiltersMode: FieldMapping        = explicitOrElseDefault[MultipleFiltersMode]("multipleFiltersMode", "explicitMultipleFiltersMode", "defaultMultipleFiltersMode")
    val defaultMultipleFiltersMode: FieldMapping = CursorField[MultipleFiltersMode]("defaultMultipleFiltersMode", _ => Result(DefaultMultipleFiltersMode))

    val bin: FieldMapping = explicitOrElseDefault[GmosBinning]("bin", "explicitBin", "defaultBin")

    val ampReadMode: FieldMapping        = explicitOrElseDefault[GmosAmpReadMode]("ampReadMode", "explicitAmpReadMode", "defaultAmpReadMode")
    val defaultAmpReadMode: FieldMapping = CursorField[GmosAmpReadMode]("defaultAmpReadMode", _ => Result(DefaultAmpReadMode))

    val ampGain: FieldMapping        = explicitOrElseDefault[GmosAmpGain]("ampGain", "explicitAmpGain", "defaultAmpGain")
    val defaultAmpGain: FieldMapping = CursorField[GmosAmpGain]("defaultAmpGain", _ => Result(DefaultAmpGain))

    val roi: FieldMapping        = explicitOrElseDefault[GmosRoi]("roi", "explicitRoi", "defaultRoi")
    val defaultRoi: FieldMapping = CursorField[GmosRoi]("defaultRoi", _ => Result(DefaultRoi))

    val offsets: FieldMapping =
      CursorFieldJson("offsets",
        cursor =>
          cursor
            .field("offsetsString", None)
            .flatMap(_.as[String].map(s => if (s.isEmpty) defaultOffsetsJson else decodeSpatialOffsets(s))),
        List("offsetsString")
      )

  lazy val GmosNorthImagingMapping: ObjectMapping =
    ObjectMapping(GmosNorthImagingType)(

      SqlField("observationId",   GmosNorthImagingView.Common.ObservationId, key = true, hidden = true),

      SqlObject("variant"),

      SqlObject("filters",        Join(GmosNorthImagingView.Common.ObservationId, GmosNorthImagingFilterTable.ObservationId)),
      SqlObject("initialFilters", Join(GmosNorthImagingView.Common.ObservationId, GmosNorthImagingFilterTable.ObservationId)),

      SqlField("offsetsString", GmosNorthImagingView.Common.Offsets, hidden = true),
      CommonImagingFields.offsets,

      CommonImagingFields.multipleFiltersMode,
      SqlField("explicitMultipleFiltersMode", GmosNorthImagingView.Common.ExplicitMultipleFiltersMode),
      CommonImagingFields.defaultMultipleFiltersMode,

      CommonImagingFields.bin,
      SqlField("explicitBin", GmosNorthImagingView.Common.ExplicitBin),
      SqlField("defaultBin",  GmosNorthImagingView.Common.DefaultBin),

      CommonImagingFields.ampReadMode,
      SqlField("explicitAmpReadMode", GmosNorthImagingView.Common.ExplicitAmpReadMode),
      CommonImagingFields.defaultAmpReadMode,

      CommonImagingFields.ampGain,
      SqlField("explicitAmpGain", GmosNorthImagingView.Common.ExplicitAmpGain),
      CommonImagingFields.defaultAmpGain,

      CommonImagingFields.roi,
      SqlField("explicitRoi", GmosNorthImagingView.Common.ExplicitRoi),
      CommonImagingFields.defaultRoi,
    )

  // We need an elaborator in order to order the filters predictably and to
  // limit the results to either "current" or "initial".
  private def filterElaborator[L: Order](
    t: TypeRef,
    p: LeafPredicates[ObservingModeRowVersion],
    v: ObservingModeRowVersion
  ): Elab[Unit] =
    Elab.transformChild: child =>
        OrderBy(
          OrderSelections(List(OrderSelection[L](t / "filter"))),
          Filter(p.eql(v), child)
        )

  lazy val GmosNorthImagingElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (GmosNorthImagingType, "filters", Nil) =>
      filterElaborator[GmosNorthFilter](
        GmosNorthImagingType,
        Predicates.gmosNorthImagingFilter.version,
        ObservingModeRowVersion.Current
      )

    case (GmosNorthImagingType, "initialFilters", Nil) =>
      filterElaborator[GmosNorthFilter](
        GmosNorthImagingType,
        Predicates.gmosNorthImagingFilter.version,
        ObservingModeRowVersion.Initial
      )

  lazy val GmosSouthImagingMapping: ObjectMapping =
    ObjectMapping(GmosSouthImagingType)(
      SqlField("observationId",   GmosSouthImagingView.Common.ObservationId, key = true, hidden = true),

      SqlObject("variant"),

      SqlObject("filters",        Join(GmosSouthImagingView.Common.ObservationId, GmosSouthImagingFilterTable.ObservationId)),
      SqlObject("initialFilters", Join(GmosSouthImagingView.Common.ObservationId, GmosSouthImagingFilterTable.ObservationId)),

      SqlField("offsetsString", GmosSouthImagingView.Common.Offsets, hidden = true),
      CommonImagingFields.offsets,

      CommonImagingFields.multipleFiltersMode,
      SqlField("explicitMultipleFiltersMode", GmosSouthImagingView.Common.ExplicitMultipleFiltersMode),
      CommonImagingFields.defaultMultipleFiltersMode,

      CommonImagingFields.bin,
      SqlField("explicitBin", GmosSouthImagingView.Common.ExplicitBin),
      SqlField("defaultBin",  GmosSouthImagingView.Common.DefaultBin),

      CommonImagingFields.ampReadMode,
      SqlField("explicitAmpReadMode", GmosSouthImagingView.Common.ExplicitAmpReadMode),
      CommonImagingFields.defaultAmpReadMode,

      CommonImagingFields.ampGain,
      SqlField("explicitAmpGain", GmosSouthImagingView.Common.ExplicitAmpGain),
      CommonImagingFields.defaultAmpGain,

      CommonImagingFields.roi,
      SqlField("explicitRoi", GmosSouthImagingView.Common.ExplicitRoi),
      CommonImagingFields.defaultRoi,
    )

  lazy val GmosSouthImagingElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (GmosSouthImagingType, "filters", Nil) =>
      filterElaborator[GmosSouthFilter](
        GmosSouthImagingType,
        Predicates.gmosSouthImagingFilter.version,
        ObservingModeRowVersion.Current
      )

    case (GmosSouthImagingType, "initialFilters", Nil) =>
      filterElaborator[GmosSouthFilter](
        GmosSouthImagingType,
        Predicates.gmosSouthImagingFilter.version,
        ObservingModeRowVersion.Initial
      )

  lazy val GmosImagingMappings: List[TypeMapping] =
    List(
      gmosImagingVariantMapping(
        GmosNorthImagingType / "variant",
        GmosNorthImagingView.Common
      ),
      gmosImagingVariantMapping(
        GmosSouthImagingType / "variant",
        GmosSouthImagingView.Common
      ),
      gmosGroupedFilterImagingMapping(
        GmosNorthImagingType / "variant" / "grouped",
        GmosNorthImagingView.Common.ObservationId,
        GmosNorthImagingView.Common
      ),
      gmosGroupedFilterImagingMapping(
        GmosSouthImagingType / "variant" / "grouped",
        GmosSouthImagingView.Common.ObservationId,
        GmosSouthImagingView.Common
      ),
      gmosPreImagingMapping(
        GmosNorthImagingType / "variant" / "preImaging",
        GmosNorthImagingView.Common
      ),
      gmosPreImagingMapping(
        GmosSouthImagingType / "variant" / "preImaging",
        GmosSouthImagingView.Common
      ),
      GmosNorthImagingMapping,
      GmosSouthImagingMapping
    )
}

object GmosImagingMapping:

  private val DefaultAmpReadMode: GmosAmpReadMode = GmosAmpReadMode.Slow
  private val DefaultAmpGain: GmosAmpGain = GmosAmpGain.Low
  private val DefaultRoi: GmosRoi = GmosRoi.FullFrame
  private val DefaultMultipleFiltersMode: MultipleFiltersMode = MultipleFiltersMode.Grouped

  def decodeSpatialOffsets(s: String): Json =
    if (s.trim.isEmpty) List.empty[Offset].asJson
    else OffsetsFormat.getOption(s).getOrElse(List.empty[Offset]).asJson

  val defaultOffsetsJson: Json =
    List.empty[Offset].asJson
