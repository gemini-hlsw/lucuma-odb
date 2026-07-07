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
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsWellDepth
import lucuma.odb.data.ObservingModeRowVersion
import lucuma.odb.graphql.predicate.LeafPredicates
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*

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

  lazy val GnirsImagingMapping: ObjectMapping =
    ObjectMapping(GnirsImagingType)(
      SqlField("observationId", GnirsImagingView.ObservationId, key = true, hidden = true),

      SqlObject("variant"),

      SqlObject("filters",        Join(GnirsImagingView.ObservationId, GnirsImagingFilterTable.ObservationId)),
      SqlObject("initialFilters", Join(GnirsImagingView.ObservationId, GnirsImagingFilterTable.ObservationId)),

      SqlField("camera", GnirsImagingView.Camera),
      SqlField("coadds", GnirsImagingView.Coadds),

      SqlField("explicitReadMode", GnirsImagingView.ReadMode),

      explicitOrElseDefault[GnirsWellDepth]("wellDepth", "explicitWellDepth", "defaultWellDepth"),
      SqlField("defaultWellDepth",  GnirsImagingView.WellDepthDefault),
      SqlField("explicitWellDepth", GnirsImagingView.WellDepth)
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

  lazy val GnirsImagingMappings: List[TypeMapping] =
    List(
      GnirsImagingFilterMapping,
      GnirsGroupedImagingMapping,
      GnirsInterleavedImagingMapping,
      GnirsPreImagingMapping,
      GnirsImagingVariantMapping,
      GnirsImagingMapping
    )
}
