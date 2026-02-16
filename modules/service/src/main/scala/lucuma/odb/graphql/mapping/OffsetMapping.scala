// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path
import lucuma.core.math.Angle

import table.EnumeratedOffsetView
import table.GmosImagingView
import table.TelescopeConfigGeneratorView
import table.StepRecordView

trait OffsetMapping[F[_]] extends EnumeratedOffsetView[F]
                             with GmosImagingView[F]
                             with TelescopeConfigGeneratorView[F]
                             with StepRecordView[F]:

  private def offsetComponentMappingAtPath(
    path:        Path,
    valueColumn: ColumnRef,
    idColumns:   ColumnRef*
  ): ObjectMapping =
    ObjectMapping(path)(
      (
        idColumns.toList.zipWithIndex.map: (ref, idx) =>
          SqlField(s"synthetic_id$idx", ref, key = true, hidden = true)
      ) ++ List(
        SqlField("value", valueColumn, hidden = true),
        FieldRef[Angle]("value").as("microarcseconds", _.toMicroarcseconds),
        FieldRef[Angle]("value").as("milliarcseconds", a => BigDecimal(a.toMicroarcseconds) /     1_000L),
        FieldRef[Angle]("value").as("arcseconds",      a => BigDecimal(a.toMicroarcseconds) / 1_000_000L)
      )*
    )

  private def offsetMappingAtPath(path: Path, idColumns: ColumnRef*): ObjectMapping =
    val idFields = idColumns.toList.zipWithIndex.map: (ref, idx) =>
      SqlField(s"id$idx", ref, key = true, hidden = true)
    ObjectMapping(path)((idFields ++ List(SqlObject("p"), SqlObject("q")))*)

  private lazy val CornerAPath:    Path = UniformTelescopeConfigGeneratorType / "cornerA"
  private lazy val CornerBPath:    Path = UniformTelescopeConfigGeneratorType / "cornerB"
  private lazy val EnumeratedPath: Path = EnumeratedTelescopeConfigGeneratorType / "values" / "offset"
  private lazy val RandomPath:     Path = RandomTelescopeConfigGeneratorType / "center"
  private lazy val SpiralPath:     Path = SpiralTelescopeConfigGeneratorType / "center"
  private lazy val StepRecordPath: Path = StepRecordType / "telescopeConfig" / "offset"

  private def preImagingMappings(
    p: Path,
    c: GmosImagingCommonColumns
  ): List[TypeMapping] =
    List(
      offsetMappingAtPath(p / "offset1", c.PreImaging.ObservationId),
      offsetComponentMappingAtPath(p / "offset1" / "p", c.PreImaging.Offset1P),
      offsetComponentMappingAtPath(p / "offset1" / "q", c.PreImaging.Offset1Q),
      offsetMappingAtPath(p / "offset2", c.PreImaging.ObservationId),
      offsetComponentMappingAtPath(p / "offset2" / "p", c.PreImaging.Offset2P),
      offsetComponentMappingAtPath(p / "offset2" / "q", c.PreImaging.Offset2Q),
      offsetMappingAtPath(p / "offset3", c.PreImaging.ObservationId),
      offsetComponentMappingAtPath(p / "offset3" / "p", c.PreImaging.Offset3P),
      offsetComponentMappingAtPath(p / "offset3" / "q", c.PreImaging.Offset3Q),
      offsetMappingAtPath(p / "offset4", c.PreImaging.ObservationId),
      offsetComponentMappingAtPath(p / "offset4" / "p", c.PreImaging.Offset4P),
      offsetComponentMappingAtPath(p / "offset4" / "q", c.PreImaging.Offset4Q),
    )

  lazy val OffsetMappings: List[TypeMapping] =
    preImagingMappings(GmosNorthImagingType / "variant" / "preImaging", GmosNorthImagingView.Common) ++
    preImagingMappings(GmosSouthImagingType / "variant" / "preImaging", GmosSouthImagingView.Common) ++
    List(
      offsetMappingAtPath(EnumeratedPath, EnumeratedOffsetView.ObservationId, EnumeratedOffsetView.OffsetGeneratorRole, EnumeratedOffsetView.Index),
      offsetComponentMappingAtPath(EnumeratedPath / "p", EnumeratedOffsetView.OffsetP, EnumeratedOffsetView.ObservationId, EnumeratedOffsetView.OffsetGeneratorRole, EnumeratedOffsetView.Index),
      offsetComponentMappingAtPath(EnumeratedPath / "q", EnumeratedOffsetView.OffsetQ, EnumeratedOffsetView.ObservationId, EnumeratedOffsetView.OffsetGeneratorRole, EnumeratedOffsetView.Index),

      offsetMappingAtPath(CornerAPath, TelescopeConfigGeneratorView.Uniform.ObservationId, TelescopeConfigGeneratorView.Uniform.Role),
      offsetComponentMappingAtPath(CornerAPath / "p", TelescopeConfigGeneratorView.UniformCornerAP, TelescopeConfigGeneratorView.Uniform.ObservationId, TelescopeConfigGeneratorView.Uniform.Role),
      offsetComponentMappingAtPath(CornerAPath / "q", TelescopeConfigGeneratorView.UniformCornerAQ, TelescopeConfigGeneratorView.Uniform.ObservationId, TelescopeConfigGeneratorView.Uniform.Role),

      offsetMappingAtPath(CornerBPath, TelescopeConfigGeneratorView.Uniform.ObservationId, TelescopeConfigGeneratorView.Uniform.Role),
      offsetComponentMappingAtPath(CornerBPath / "p", TelescopeConfigGeneratorView.UniformCornerBP, TelescopeConfigGeneratorView.Uniform.ObservationId, TelescopeConfigGeneratorView.Uniform.Role),
      offsetComponentMappingAtPath(CornerBPath / "q", TelescopeConfigGeneratorView.UniformCornerBQ, TelescopeConfigGeneratorView.Uniform.ObservationId, TelescopeConfigGeneratorView.Uniform.Role),

      offsetMappingAtPath(RandomPath, TelescopeConfigGeneratorView.Random.ObservationId, TelescopeConfigGeneratorView.Random.Role),
      offsetComponentMappingAtPath(RandomPath / "p", TelescopeConfigGeneratorView.CenterOffsetP, TelescopeConfigGeneratorView.Random.ObservationId, TelescopeConfigGeneratorView.Random.Role),
      offsetComponentMappingAtPath(RandomPath / "q", TelescopeConfigGeneratorView.CenterOffsetQ, TelescopeConfigGeneratorView.Random.ObservationId, TelescopeConfigGeneratorView.Random.Role),

      offsetMappingAtPath(SpiralPath, TelescopeConfigGeneratorView.Spiral.ObservationId, TelescopeConfigGeneratorView.Spiral.Role),
      offsetComponentMappingAtPath(SpiralPath / "p", TelescopeConfigGeneratorView.CenterOffsetP, TelescopeConfigGeneratorView.Spiral.ObservationId, TelescopeConfigGeneratorView.Spiral.Role),
      offsetComponentMappingAtPath(SpiralPath / "q", TelescopeConfigGeneratorView.CenterOffsetQ, TelescopeConfigGeneratorView.Spiral.ObservationId, TelescopeConfigGeneratorView.Spiral.Role),

      offsetMappingAtPath(StepRecordPath, StepRecordView.Id),
      offsetComponentMappingAtPath(StepRecordPath / "p", StepRecordView.OffsetP, StepRecordView.Id),
      offsetComponentMappingAtPath(StepRecordPath / "q", StepRecordView.OffsetQ, StepRecordView.Id)
    )