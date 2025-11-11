// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path
import lucuma.core.math.Angle

import table.EnumeratedOffsetView
import table.OffsetGeneratorView
import table.StepRecordView

trait OffsetMapping[F[_]] extends EnumeratedOffsetView[F]
                             with OffsetGeneratorView[F]
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

  private lazy val CornerAPath:    Path = GridOffsetGeneratorType / "cornerA"
  private lazy val CornerBPath:    Path = GridOffsetGeneratorType / "cornerB"
  private lazy val EnumeratedPath: Path = EnumeratedOffsetGeneratorType / "values" / "offset"
  private lazy val RandomPath:     Path = RandomOffsetGeneratorType / "center"
  private lazy val SpiralPath:     Path = SpiralOffsetGeneratorType / "center"
  private lazy val StepRecordPath: Path = StepRecordType / "telescopeConfig" / "offset"

  lazy val OffsetMappings: List[TypeMapping] =
    List(
      offsetMappingAtPath(EnumeratedPath, EnumeratedOffsetView.ObservationId, EnumeratedOffsetView.OffsetGeneratorRole, EnumeratedOffsetView.Index),
      offsetComponentMappingAtPath(EnumeratedPath / "p", EnumeratedOffsetView.OffsetP, EnumeratedOffsetView.ObservationId, EnumeratedOffsetView.OffsetGeneratorRole, EnumeratedOffsetView.Index),
      offsetComponentMappingAtPath(EnumeratedPath / "q", EnumeratedOffsetView.OffsetQ, EnumeratedOffsetView.ObservationId, EnumeratedOffsetView.OffsetGeneratorRole, EnumeratedOffsetView.Index),

      offsetMappingAtPath(CornerAPath, OffsetGeneratorView.Grid.ObservationId, OffsetGeneratorView.Grid.OffsetGeneratorRole),
      offsetComponentMappingAtPath(CornerAPath / "p", OffsetGeneratorView.GridCornerAP, OffsetGeneratorView.Grid.ObservationId, OffsetGeneratorView.Grid.OffsetGeneratorRole),
      offsetComponentMappingAtPath(CornerAPath / "q", OffsetGeneratorView.GridCornerAQ, OffsetGeneratorView.Grid.ObservationId, OffsetGeneratorView.Grid.OffsetGeneratorRole),

      offsetMappingAtPath(CornerBPath, OffsetGeneratorView.Grid.ObservationId, OffsetGeneratorView.Grid.OffsetGeneratorRole),
      offsetComponentMappingAtPath(CornerBPath / "p", OffsetGeneratorView.GridCornerBP, OffsetGeneratorView.Grid.ObservationId, OffsetGeneratorView.Grid.OffsetGeneratorRole),
      offsetComponentMappingAtPath(CornerBPath / "q", OffsetGeneratorView.GridCornerBQ, OffsetGeneratorView.Grid.ObservationId, OffsetGeneratorView.Grid.OffsetGeneratorRole),

      offsetMappingAtPath(RandomPath, OffsetGeneratorView.Random.ObservationId, OffsetGeneratorView.Random.OffsetGeneratorRole),
      offsetComponentMappingAtPath(RandomPath / "p", OffsetGeneratorView.CenterOffsetP, OffsetGeneratorView.Random.ObservationId, OffsetGeneratorView.Random.OffsetGeneratorRole),
      offsetComponentMappingAtPath(RandomPath / "q", OffsetGeneratorView.CenterOffsetQ, OffsetGeneratorView.Random.ObservationId, OffsetGeneratorView.Random.OffsetGeneratorRole),

      offsetMappingAtPath(SpiralPath, OffsetGeneratorView.ObservationId, OffsetGeneratorView.Spiral.ObservationId, OffsetGeneratorView.Spiral.OffsetGeneratorRole),
      offsetComponentMappingAtPath(SpiralPath / "p", OffsetGeneratorView.CenterOffsetP, OffsetGeneratorView.Spiral.ObservationId, OffsetGeneratorView.Spiral.OffsetGeneratorRole),
      offsetComponentMappingAtPath(SpiralPath / "q", OffsetGeneratorView.CenterOffsetQ, OffsetGeneratorView.Spiral.ObservationId, OffsetGeneratorView.Spiral.OffsetGeneratorRole),

      offsetMappingAtPath(StepRecordPath, StepRecordView.Id),
      offsetComponentMappingAtPath(StepRecordPath / "p", StepRecordView.OffsetP, StepRecordView.Id),
      offsetComponentMappingAtPath(StepRecordPath / "q", StepRecordView.OffsetQ, StepRecordView.Id)
    )