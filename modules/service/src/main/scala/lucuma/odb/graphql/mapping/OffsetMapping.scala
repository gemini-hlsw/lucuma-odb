// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path
import lucuma.core.math.Angle

import table.EnumeratedOffsetView
import table.Flamingos2ImagingView
import table.GmosImagingView
import table.GnirsImagingView
import table.TelescopeConfigGeneratorView
import table.StepRecordView

trait OffsetMapping[F[_]] extends EnumeratedOffsetView[F]
                             with Flamingos2ImagingView[F]
                             with GnirsImagingView[F]
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
        FieldRef[Angle]("value").as("microarcseconds", a => Angle.signedMicroarcseconds.get(a)),
        FieldRef[Angle]("value").as("milliarcseconds", a => BigDecimal(Angle.signedMicroarcseconds.get(a)) /     1_000L),
        FieldRef[Angle]("value").as("arcseconds",      a => BigDecimal(Angle.signedMicroarcseconds.get(a)) / 1_000_000L)
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
    p:     Path,
    obsId: ColumnRef,
    o1p:   ColumnRef, o1q: ColumnRef,
    o2p:   ColumnRef, o2q: ColumnRef,
    o3p:   ColumnRef, o3q: ColumnRef,
    o4p:   ColumnRef, o4q: ColumnRef
  ): List[TypeMapping] =
    List(
      offsetMappingAtPath(p / "offset1", obsId),
      offsetComponentMappingAtPath(p / "offset1" / "p", o1p),
      offsetComponentMappingAtPath(p / "offset1" / "q", o1q),
      offsetMappingAtPath(p / "offset2", obsId),
      offsetComponentMappingAtPath(p / "offset2" / "p", o2p),
      offsetComponentMappingAtPath(p / "offset2" / "q", o2q),
      offsetMappingAtPath(p / "offset3", obsId),
      offsetComponentMappingAtPath(p / "offset3" / "p", o3p),
      offsetComponentMappingAtPath(p / "offset3" / "q", o3q),
      offsetMappingAtPath(p / "offset4", obsId),
      offsetComponentMappingAtPath(p / "offset4" / "p", o4p),
      offsetComponentMappingAtPath(p / "offset4" / "q", o4q),
    )

  private def gmosPreImagingMappings(p: Path, c: GmosImagingCommonColumns): List[TypeMapping] =
    preImagingMappings(
      p,
      c.PreImaging.ObservationId,
      c.PreImaging.Offset1P, c.PreImaging.Offset1Q,
      c.PreImaging.Offset2P, c.PreImaging.Offset2Q,
      c.PreImaging.Offset3P, c.PreImaging.Offset3Q,
      c.PreImaging.Offset4P, c.PreImaging.Offset4Q
    )

  private lazy val flamingos2PreImagingMappings: List[TypeMapping] =
    import Flamingos2ImagingView.PreImaging as PI
    preImagingMappings(
      Flamingos2ImagingType / "variant" / "preImaging",
      PI.ObservationId,
      PI.Offset1P, PI.Offset1Q,
      PI.Offset2P, PI.Offset2Q,
      PI.Offset3P, PI.Offset3Q,
      PI.Offset4P, PI.Offset4Q
    )

  private lazy val gnirsPreImagingMappings: List[TypeMapping] =
    import GnirsImagingView.PreImaging as PI
    preImagingMappings(
      GnirsImagingType / "variant" / "preImaging",
      PI.ObservationId,
      PI.Offset1P, PI.Offset1Q,
      PI.Offset2P, PI.Offset2Q,
      PI.Offset3P, PI.Offset3Q,
      PI.Offset4P, PI.Offset4Q
    )

  lazy val OffsetMappings: List[TypeMapping] =
    gmosPreImagingMappings(GmosNorthImagingType / "variant" / "preImaging", GmosNorthImagingView.Common) ++
    gmosPreImagingMappings(GmosSouthImagingType / "variant" / "preImaging", GmosSouthImagingView.Common) ++
    flamingos2PreImagingMappings ++
    gnirsPreImagingMappings ++
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