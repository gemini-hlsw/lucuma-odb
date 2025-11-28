// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.Order
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

      SqlField("observationId",   GmosNorthImagingView.ObservationId, key = true, hidden = true),
      SqlObject("filters",        Join(GmosNorthImagingView.ObservationId, GmosNorthImagingFilterTable.ObservationId)),
      SqlObject("initialFilters", Join(GmosNorthImagingView.ObservationId, GmosNorthImagingFilterTable.ObservationId)),

      SqlField("offsetsString", GmosNorthImagingView.Offsets, hidden = true),
      CommonImagingFields.offsets,

      SqlObject("objectOffsetGenerator", Join(GmosNorthImagingView.ObservationId, OffsetGeneratorView.ObjectObservationId)),
      SqlObject("skyOffsetGenerator",    Join(GmosNorthImagingView.ObservationId, OffsetGeneratorView.SkyObservationId)),

      CommonImagingFields.multipleFiltersMode,
      SqlField("explicitMultipleFiltersMode", GmosNorthImagingView.ExplicitMultipleFiltersMode),
      CommonImagingFields.defaultMultipleFiltersMode,

      CommonImagingFields.bin,
      SqlField("explicitBin", GmosNorthImagingView.ExplicitBin),
      SqlField("defaultBin",  GmosNorthImagingView.DefaultBin),

      CommonImagingFields.ampReadMode,
      SqlField("explicitAmpReadMode", GmosNorthImagingView.ExplicitAmpReadMode),
      CommonImagingFields.defaultAmpReadMode,

      CommonImagingFields.ampGain,
      SqlField("explicitAmpGain", GmosNorthImagingView.ExplicitAmpGain),
      CommonImagingFields.defaultAmpGain,

      CommonImagingFields.roi,
      SqlField("explicitRoi", GmosNorthImagingView.ExplicitRoi),
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

      SqlField("observationId",   GmosSouthImagingView.ObservationId, key = true, hidden = true),
      SqlObject("filters",        Join(GmosSouthImagingView.ObservationId, GmosSouthImagingFilterTable.ObservationId)),
      SqlObject("initialFilters", Join(GmosSouthImagingView.ObservationId, GmosSouthImagingFilterTable.ObservationId)),

      SqlField("offsetsString", GmosSouthImagingView.Offsets, hidden = true),
      CommonImagingFields.offsets,

      SqlObject("objectOffsetGenerator", Join(GmosSouthImagingView.ObservationId, OffsetGeneratorView.ObjectObservationId)),
      SqlObject("skyOffsetGenerator",    Join(GmosSouthImagingView.ObservationId, OffsetGeneratorView.SkyObservationId)),

      CommonImagingFields.multipleFiltersMode,
      SqlField("explicitMultipleFiltersMode", GmosSouthImagingView.ExplicitMultipleFiltersMode),
      CommonImagingFields.defaultMultipleFiltersMode,

      CommonImagingFields.bin,
      SqlField("explicitBin", GmosSouthImagingView.ExplicitBin),
      SqlField("defaultBin",  GmosSouthImagingView.DefaultBin),

      CommonImagingFields.ampReadMode,
      SqlField("explicitAmpReadMode", GmosSouthImagingView.ExplicitAmpReadMode),
      CommonImagingFields.defaultAmpReadMode,

      CommonImagingFields.ampGain,
      SqlField("explicitAmpGain", GmosSouthImagingView.ExplicitAmpGain),
      CommonImagingFields.defaultAmpGain,

      CommonImagingFields.roi,
      SqlField("explicitRoi", GmosSouthImagingView.ExplicitRoi),
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
