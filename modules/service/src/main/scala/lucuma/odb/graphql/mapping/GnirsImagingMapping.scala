// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Query.Binding
import grackle.Query.Filter
import grackle.Query.OrderBy
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import grackle.Query.Unique
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.data.ObservingModeRowVersion
import lucuma.odb.graphql.predicate.LeafPredicates
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*
import lucuma.odb.json.offset.query.given

trait GnirsImagingMapping[F[_]]
  extends GnirsImagingView[F]
     with ExposureTimeModeMapping[F]
     with TelescopeConfigGeneratorView[F]
     with OptionalFieldMapping[F]
     with Predicates[F] { this: SkunkMapping[F] =>

  lazy val GnirsImagingFilterMapping: ObjectMapping =
    ObjectMapping(GnirsImagingFilterType)(
      SqlField("observationId",     GnirsImagingFilterTable.ObservationId, key = true, hidden = true),
      SqlField("filter",            GnirsImagingFilterTable.Filter, key = true),
      SqlField("version",           GnirsImagingFilterTable.Version, key = true, hidden = true),
      SqlField("coadds",            GnirsImagingFilterTable.Coadds),
      SqlObject("exposureTimeMode", Join(GnirsImagingFilterTable.ExposureTimeModeId, ExposureTimeModeView.Id))
    )

  lazy val GnirsGroupedImagingMapping: ObjectMapping =
    ObjectMapping(GnirsImagingType / "variant" / "grouped")(
      SqlField("observationId", GnirsImagingView.Grouped.ObservationId, key = true, hidden = true),
      SqlField("order",         GnirsImagingView.Grouped.WavelengthOrder),
      SqlField("skyCount",      GnirsImagingView.Sky.Count),
      SqlObject("offsets",      Join(GnirsImagingView.ObservationId, TelescopeConfigGeneratorView.ObjectObservationId)),
      SqlObject("skyOffsets",   Join(GnirsImagingView.ObservationId, TelescopeConfigGeneratorView.SkyObservationId)),
    )

  lazy val GnirsInterleavedImagingMapping: ObjectMapping =
    ObjectMapping(GnirsImagingType / "variant" / "interleaved")(
      SqlField("observationId", GnirsImagingView.Interleaved.ObservationId, key = true, hidden = true),
      SqlField("skyCount",      GnirsImagingView.Sky.Count),
      SqlObject("offsets",      Join(GnirsImagingView.ObservationId, TelescopeConfigGeneratorView.ObjectObservationId)),
      SqlObject("skyOffsets",   Join(GnirsImagingView.ObservationId, TelescopeConfigGeneratorView.SkyObservationId)),
    )

  lazy val GnirsPreImagingMapping: ObjectMapping =
    ObjectMapping(GnirsImagingType / "variant" / "preImaging")(
      SqlField("observationId", GnirsImagingView.PreImaging.ObservationId, key = true, hidden = true),
      SqlObject("offset1"),
      SqlObject("offset2"),
      SqlObject("offset3"),
      SqlObject("offset4")
    )

  lazy val GnirsImagingVariantMapping: ObjectMapping =
    ObjectMapping(GnirsImagingType / "variant")(
      SqlField("observationId", GnirsImagingView.ObservationId, key = true, hidden = true),
      SqlField("variantType",   GnirsImagingView.Variant),
      SqlObject("grouped"),
      SqlObject("interleaved"),
      SqlObject("preImaging")
    )

  lazy val GnirsImagingAcquisitionMapping: ObjectMapping =
    ObjectMapping(GnirsImagingAcquisitionType)(

      SqlField("observationId", GnirsImagingView.ObservationId, key = true, hidden = true),

      SqlField("explicitAcquisitionType", GnirsImagingView.AcqType),
      SqlField("coadds",                  GnirsImagingView.AcqCoadds),

      // Acquisition filter: explicit override only. The effective/default filter is
      // determined in code (the first science filter) at sequence-generation time.
      SqlField("explicitFilter", GnirsImagingView.AcqFilter),

      SqlField("acqSkyOffPRaw", GnirsImagingView.AcqSkyOffsetP, hidden = true),
      SqlField("acqSkyOffQRaw", GnirsImagingView.AcqSkyOffsetQ, hidden = true),

      CursorFieldJson("skyOffset",
        cursor =>
          for
            p <- cursor.field("acqSkyOffPRaw", None).flatMap(_.as[Option[Angle]])
            q <- cursor.field("acqSkyOffQRaw", None).flatMap(_.as[Option[Angle]])
          yield (p, q) match
            case (Some(pa), Some(qa)) =>
              Offset(Offset.P(pa), Offset.Q(qa)).asJson
            case _ => Json.Null,
        List("acqSkyOffPRaw", "acqSkyOffQRaw")
      ),

      // The effective acquisition exposure time mode, and the explicit override.
      // Both read the same t_exposure_time_mode row; the explicit one is keyed on the
      // view's c_explicit_* columns so it is null when the mode is derived.
      SqlObject("exposureTimeMode", Join(GnirsImagingView.ObservationId, ExposureTimeModeView.ObservationId)),
      SqlObject("explicitExposureTimeMode", Join(GnirsImagingView.ObservationId, ExposureTimeModeView.ObservationId)),
    )

  lazy val GnirsImagingMapping: ObjectMapping =
    ObjectMapping(GnirsImagingType)(
      SqlField("observationId", GnirsImagingView.ObservationId, key = true, hidden = true),

      SqlObject("variant"),

      SqlObject("filters",        Join(GnirsImagingView.ObservationId, GnirsImagingFilterTable.ObservationId)),
      SqlObject("initialFilters", Join(GnirsImagingView.ObservationId, GnirsImagingFilterTable.ObservationId)),

      SqlField("camera", GnirsImagingView.Camera),

      SqlField("explicitReadMode", GnirsImagingView.ReadMode),

      explicitOrElseDefault[GnirsWellDepth]("wellDepth", "explicitWellDepth", "defaultWellDepth"),
      SqlField("defaultWellDepth",  GnirsImagingView.WellDepthDefault),
      SqlField("explicitWellDepth", GnirsImagingView.WellDepth),

      SqlObject("acquisition")
    )

  // Order filters predictably and limit to either "current" or "initial".
  private def gnirsFilterElaborator(t: TypeRef, p: LeafPredicates[ObservingModeRowVersion], v: ObservingModeRowVersion): Elab[Unit] =
    Elab.transformChild: child =>
      OrderBy(
        OrderSelections(List(OrderSelection[GnirsFilter](t / "filter"))),
        Filter(p.eql(v), child)
      )

  lazy val GnirsImagingElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (GnirsImagingType, "filters", Nil) =>
      gnirsFilterElaborator(
        GnirsImagingType,
        Predicates.gnirsImagingFilter.version,
        ObservingModeRowVersion.Current
      )

    case (GnirsImagingType, "initialFilters", Nil) =>
      gnirsFilterElaborator(
        GnirsImagingType,
        Predicates.gnirsImagingFilter.version,
        ObservingModeRowVersion.Initial
      )

    case (GnirsImagingAcquisitionType, "exposureTimeMode" | "explicitExposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Acquisition),
            child
          )
        )

  lazy val GnirsImagingMappings: List[TypeMapping] =
    List(
      GnirsImagingFilterMapping,
      GnirsGroupedImagingMapping,
      GnirsInterleavedImagingMapping,
      GnirsPreImagingMapping,
      GnirsImagingVariantMapping,
      GnirsImagingAcquisitionMapping,
      GnirsImagingMapping
    )
}
