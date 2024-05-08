// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path
import lucuma.core.math.Angle

import table.StepRecordView

trait OffsetMapping[F[_]] extends StepRecordView[F] {

  private def offsetComponentMappingAtPath(
    path:        Path,
    idColumn:    ColumnRef,
    valueColumn: ColumnRef
  ): ObjectMapping =
    ObjectMapping(PathMatch(path))(
      SqlField("synthetic_id", idColumn, key = true, hidden = true),
      SqlField("value", valueColumn, hidden = true),
      FieldRef[Angle]("value").as("microarcseconds", _.toMicroarcseconds),
      FieldRef[Angle]("value").as("milliarcseconds", a => BigDecimal(a.toMicroarcseconds) /     1_000L),
      FieldRef[Angle]("value").as("arcseconds",      a => BigDecimal(a.toMicroarcseconds) / 1_000_000L)
    )

  private def offsetComponentSwitchMapping(
    name:        String,
    idColumn:    ColumnRef,
    valueColumn: ColumnRef
  ): List[TypeMapping] =
    List(
      offsetComponentMappingAtPath(StepRecordType / "stepConfig" / "offset" / name, idColumn, valueColumn)
    )

  lazy val OffsetPMappings: List[TypeMapping] =
    offsetComponentSwitchMapping("p", StepRecordView.Id, StepRecordView.Science.OffsetP)

  lazy val OffsetQMappings: List[TypeMapping] =
    offsetComponentSwitchMapping("q", StepRecordView.Id, StepRecordView.Science.OffsetQ)

  private def offsetMappingAtPath(
    path: Path,
    idColumn: ColumnRef
  ): ObjectMapping =
    ObjectMapping(PathMatch(path))(
      SqlField("id", idColumn, key = true, hidden = true),
      SqlObject("p"),
      SqlObject("q")
    )

  lazy val OffsetMappings: List[TypeMapping] =
    List(
      offsetMappingAtPath(StepRecordType / "stepConfig" / "offset", StepRecordView.Id)
    )

}
