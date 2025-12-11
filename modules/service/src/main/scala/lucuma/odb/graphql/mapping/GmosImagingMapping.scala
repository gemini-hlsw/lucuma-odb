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
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.odb.data.ObservingModeRowVersion
import lucuma.odb.graphql.predicate.LeafPredicates
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*

trait GmosImagingMapping[F[_]] extends GmosImagingView[F]
                                  with GmosImagingFilterTable[F]
                                  with OffsetGeneratorView[F]
                                  with OptionalFieldMapping[F]
                                  with Predicates[F] { this: SkunkMapping[F] =>

  import GmosImagingMapping.*

  private def groupedImagingMapping(
    p: Path,
    c: GmosImagingCommonColumns
  ): ObjectMapping =
    ObjectMapping(p)(
      SqlField("observationId", c.Grouped.ObservationId, key = true, hidden = true),
      SqlField("order",         c.Grouped.WavelengthOrder),
      SqlField("skyCount",      c.Sky.Count),
      SqlObject("offsets",      Join(c.ObservationId, OffsetGeneratorView.ObjectObservationId)),
      SqlObject("skyOffsets",   Join(c.ObservationId, OffsetGeneratorView.SkyObservationId)),
    )

  lazy val GmosNorthGroupedImagingMapping: ObjectMapping =
    groupedImagingMapping(GmosNorthImagingType / "variant" / "grouped", GmosNorthImagingView.Common)

  lazy val GmosSouthGroupedImagingMapping: ObjectMapping =
    groupedImagingMapping(GmosSouthImagingType / "variant" / "grouped", GmosSouthImagingView.Common)


  private def interleavedImagingMapping(
    p: Path,
    c: GmosImagingCommonColumns
  ): ObjectMapping =
    ObjectMapping(p)(
      SqlField("observationId", c.Interleaved.ObservationId, key = true, hidden = true),
      SqlField("skyCount",      c.Sky.Count),
      SqlObject("offsets",      Join(c.ObservationId, OffsetGeneratorView.ObjectObservationId)),
      SqlObject("skyOffsets",   Join(c.ObservationId, OffsetGeneratorView.SkyObservationId)),
    )

  lazy val GmosNorthInterleavedImagingMapping: ObjectMapping =
    interleavedImagingMapping(GmosNorthImagingType / "variant" / "interleaved", GmosNorthImagingView.Common)

  lazy val GmosSouthInterleavedImagingMapping: ObjectMapping =
    interleavedImagingMapping(GmosSouthImagingType / "variant" / "interleaved", GmosSouthImagingView.Common)


  private def preImagingMapping(
    p: Path,
    c: GmosImagingCommonColumns
  ): ObjectMapping =
    ObjectMapping(p)(
      SqlField("observationId", c.PreImaging.ObservationId, key = true, hidden = true),
      SqlObject("offset1"),
      SqlObject("offset2"),
      SqlObject("offset3"),
      SqlObject("offset4")
    )

  lazy val GmosNorthPreImagingMapping: ObjectMapping =
    preImagingMapping(GmosNorthImagingType / "variant" / "preImaging", GmosNorthImagingView.Common)

  lazy val GmosSouthPreImagingMapping: ObjectMapping =
    preImagingMapping(GmosSouthImagingType / "variant" / "preImaging", GmosSouthImagingView.Common)


  private def imagingVariantMapping(
    p: Path,
    c: GmosImagingCommonColumns
  ): ObjectMapping =
    ObjectMapping(p)(
      SqlField("observationId", c.ObservationId, key = true, hidden = true),
      SqlField("variantType", c.Variant),
      SqlObject("grouped"),
      SqlObject("interleaved"),
      SqlObject("preImaging")
    )

  lazy val GmosNorthImagingVariantMapping: ObjectMapping =
    imagingVariantMapping(GmosNorthImagingType / "variant", GmosNorthImagingView.Common)

  lazy val GmosSouthImagingVariantMapping: ObjectMapping =
    imagingVariantMapping(GmosSouthImagingType / "variant", GmosSouthImagingView.Common)


  private object CommonImagingFields:

    val bin: FieldMapping = explicitOrElseDefault[GmosBinning]("bin", "explicitBin", "defaultBin")

    val ampReadMode: FieldMapping        = explicitOrElseDefault[GmosAmpReadMode]("ampReadMode", "explicitAmpReadMode", "defaultAmpReadMode")
    val defaultAmpReadMode: FieldMapping = CursorField[GmosAmpReadMode]("defaultAmpReadMode", _ => Result(DefaultAmpReadMode))

    val ampGain: FieldMapping        = explicitOrElseDefault[GmosAmpGain]("ampGain", "explicitAmpGain", "defaultAmpGain")
    val defaultAmpGain: FieldMapping = CursorField[GmosAmpGain]("defaultAmpGain", _ => Result(DefaultAmpGain))

    val roi: FieldMapping        = explicitOrElseDefault[GmosRoi]("roi", "explicitRoi", "defaultRoi")
    val defaultRoi: FieldMapping = CursorField[GmosRoi]("defaultRoi", _ => Result(DefaultRoi))

  private def imagingMapping(
    t: TypeRef,
    c: GmosImagingCommonColumns,
    f: ColumnRef
  ): ObjectMapping =
    ObjectMapping(t)(
      SqlField("observationId",  c.ObservationId, key = true, hidden = true),

      SqlObject("variant"),

      SqlObject("filters",        Join(c.ObservationId, f)),
      SqlObject("initialFilters", Join(c.ObservationId, f)),

      CommonImagingFields.bin,
      SqlField("explicitBin", c.ExplicitBin),
      SqlField("defaultBin",  c.DefaultBin),

      CommonImagingFields.ampReadMode,
      SqlField("explicitAmpReadMode", c.ExplicitAmpReadMode),
      CommonImagingFields.defaultAmpReadMode,

      CommonImagingFields.ampGain,
      SqlField("explicitAmpGain", c.ExplicitAmpGain),
      CommonImagingFields.defaultAmpGain,

      CommonImagingFields.roi,
      SqlField("explicitRoi", c.ExplicitRoi),
      CommonImagingFields.defaultRoi
    )

  lazy val GmosNorthImagingMapping: ObjectMapping =
    imagingMapping(
      GmosNorthImagingType,
      GmosNorthImagingView.Common,
      GmosNorthImagingFilterTable.ObservationId
    )

  lazy val GmosSouthImagingMapping: ObjectMapping =
    imagingMapping(
      GmosSouthImagingType,
      GmosSouthImagingView.Common,
      GmosSouthImagingFilterTable.ObservationId
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
      GmosNorthGroupedImagingMapping,
      GmosSouthGroupedImagingMapping,

      GmosNorthInterleavedImagingMapping,
      GmosSouthInterleavedImagingMapping,

      GmosNorthPreImagingMapping,
      GmosSouthPreImagingMapping,

      GmosNorthImagingVariantMapping,
      GmosSouthImagingVariantMapping,

      GmosNorthImagingMapping,
      GmosSouthImagingMapping
    )
}

object GmosImagingMapping:

  private val DefaultAmpReadMode: GmosAmpReadMode = GmosAmpReadMode.Slow
  private val DefaultAmpGain: GmosAmpGain = GmosAmpGain.Low
  private val DefaultRoi: GmosRoi = GmosRoi.FullFrame
