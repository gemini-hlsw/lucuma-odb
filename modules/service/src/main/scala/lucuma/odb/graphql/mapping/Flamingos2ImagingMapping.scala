// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Query.Binding
import grackle.Query.Filter
import grackle.Query.OrderBy
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.math.Offset
import lucuma.odb.data.ObservingModeRowVersion
import lucuma.odb.format.spatialOffsets.*
import lucuma.odb.graphql.predicate.LeafPredicates
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*
import lucuma.odb.json.offset.query.given

trait Flamingos2ImagingMapping[F[_]]
  extends Flamingos2ImagingView[F]
     with ExposureTimeModeMapping[F]
     with OptionalFieldMapping[F]
     with Predicates[F] { this: SkunkMapping[F] =>

  private def decodeOffsets(s: String): Json =
    OffsetsFormat.getOption(s).map(_.asJson).getOrElse(List.empty[Offset].asJson)

  private val defaultOffsetsJson: Json =
    List.empty[Offset].asJson

  lazy val Flamingos2ImagingFilterMapping: ObjectMapping =
    ObjectMapping(Flamingos2ImagingFilterType)(
      SqlField("observationId",     Flamingos2ImagingFilterTable.ObservationId, key = true, hidden = true),
      SqlField("filter",            Flamingos2ImagingFilterTable.Filter, key = true),
      SqlField("version",           Flamingos2ImagingFilterTable.Version, key = true, hidden = true),
      SqlObject("exposureTimeMode", Join(Flamingos2ImagingFilterTable.ExposureTimeModeId, ExposureTimeModeView.Id))
    )

  lazy val Flamingos2ImagingMapping: ObjectMapping =
    ObjectMapping(Flamingos2ImagingType)(
      SqlField("observationId", Flamingos2ImagingView.ObservationId, key = true, hidden = true),

      SqlObject("filters",        Join(Flamingos2ImagingView.ObservationId, Flamingos2ImagingFilterTable.ObservationId)),
      SqlObject("initialFilters", Join(Flamingos2ImagingView.ObservationId, Flamingos2ImagingFilterTable.ObservationId)),

      SqlField("explicitReadMode", Flamingos2ImagingView.ReadMode),
      SqlField("explicitReads",    Flamingos2ImagingView.Reads),

      explicitOrElseDefault[Flamingos2Decker]("decker", "explicitDecker", "defaultDecker"),
      SqlField("defaultDecker",  Flamingos2ImagingView.DeckerDefault),
      SqlField("explicitDecker", Flamingos2ImagingView.Decker),

      explicitOrElseDefault[Flamingos2ReadoutMode]("readoutMode", "explicitReadoutMode", "defaultReadoutMode"),
      SqlField("defaultReadoutMode",  Flamingos2ImagingView.ReadoutModeDefault),
      SqlField("explicitReadoutMode", Flamingos2ImagingView.ReadoutMode),

      SqlField("offsetsString", Flamingos2ImagingView.Offsets, hidden = true),

      CursorFieldJson("spatialOffsets",
        cursor =>
          cursor
            .field("offsetsString", None)
            .flatMap(_.as[Option[String]].map(_.map(decodeOffsets)))
            .map(_.getOrElse(defaultOffsetsJson)),
        List("explicitSpatialOffsets", "defaultSpatialOffsets")
      ),

      CursorFieldJson("explicitSpatialOffsets",
        cursor =>
          cursor
            .field("offsetsString", None)
            .flatMap(_.as[Option[String]].map(_.map(decodeOffsets).asJson)),
        List("offsetsString")
      ),

      CursorFieldJson("defaultSpatialOffsets", _ => Result(defaultOffsetsJson), Nil)
    )

  // Order filters predictably and limit to either "current" or "initial".
  private def filterElaborator(t: TypeRef, p: LeafPredicates[ObservingModeRowVersion], v: ObservingModeRowVersion): Elab[Unit] =
    Elab.transformChild: child =>
      OrderBy(
        OrderSelections(List(OrderSelection[Flamingos2Filter](t / "filter"))),
        Filter(p.eql(v), child)
      )

  lazy val Flamingos2ImagingElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (Flamingos2ImagingType, "filters", Nil) =>
      filterElaborator(
        Flamingos2ImagingType,
        Predicates.flamingos2ImagingFilter.version,
        ObservingModeRowVersion.Current
      )

    case (Flamingos2ImagingType, "initialFilters", Nil) =>
      filterElaborator(
        Flamingos2ImagingType,
        Predicates.flamingos2ImagingFilter.version,
        ObservingModeRowVersion.Initial
      )

  lazy val Flamingos2ImagingMappings: List[TypeMapping] =
    List(
      Flamingos2ImagingFilterMapping,
      Flamingos2ImagingMapping
    )
}
