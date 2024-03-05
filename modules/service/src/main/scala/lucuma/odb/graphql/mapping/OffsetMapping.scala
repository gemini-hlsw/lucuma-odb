// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.TypeRef
import lucuma.core.math.Angle

import table.StepRecordView

trait OffsetMapping[F[_]] extends StepRecordView[F] {

  private def offsetComponentMapping(
    typeRef:     TypeRef,
    idColumn:    ColumnRef,
    valueColumn: ColumnRef
  ): ObjectMapping =
    ObjectMapping(
      tpe = typeRef,
      fieldMappings = List(
        SqlField("synthetic_id", idColumn, key = true, hidden = true),
        SqlField("value", valueColumn, hidden = true),
        FieldRef[Angle]("value").as("microarcseconds", _.toMicroarcseconds),
        FieldRef[Angle]("value").as("milliarcseconds", a => BigDecimal(a.toMicroarcseconds) /     1_000L),
        FieldRef[Angle]("value").as("arcseconds",      a => BigDecimal(a.toMicroarcseconds) / 1_000_000L)
      )
    )

  private def offsetComponentSwitchMapping(
    name:        String,
    typeRef:     TypeRef,
    idColumn:    ColumnRef,
    valueColumn: ColumnRef
  ): TypeMapping =
    SwitchMapping(
      typeRef,
      List(
        StepRecordType / "stepConfig" / "offset" / name -> offsetComponentMapping(typeRef, idColumn, valueColumn)
      )
    )

  lazy val OffsetPMapping: TypeMapping =
    offsetComponentSwitchMapping("p", OffsetPType, StepRecordView.Id, StepRecordView.Science.OffsetP)

  lazy val OffsetQMapping: TypeMapping =
    offsetComponentSwitchMapping("q", OffsetQType, StepRecordView.Id, StepRecordView.Science.OffsetQ)

  private def offsetMapping(
    idColumn: ColumnRef
  ): ObjectMapping =
    ObjectMapping(
      tpe = OffsetType,
      fieldMappings = List(
        SqlField("id", idColumn, key = true, hidden = true),
        SqlObject("p"),
        SqlObject("q")
      )
    )

  lazy val OffsetMapping: TypeMapping =
    SwitchMapping(
      OffsetType,
      List(
        StepRecordType / "stepConfig" / "offset" -> offsetMapping(StepRecordView.Id)
      )
    )
}
