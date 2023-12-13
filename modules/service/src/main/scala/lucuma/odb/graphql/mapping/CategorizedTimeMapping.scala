// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.odb.graphql.table.TimeAccountingTable

trait CategorizedTimeMapping[F[_]] extends TimeAccountingTable[F] {

  lazy val CategorizedTimeMapping: TypeMapping =
    SwitchMapping(
      CategorizedTimeType,
      List(
        TimeChargeInvoiceType / "executionTime" -> categorizedTimeMapping(TimeAccountingTable.VisitId),
        TimeChargeInvoiceType / "finalCharge"   -> categorizedTimeMapping(TimeAccountingTable.VisitId)
      )
    )

  private def categorizedTimeMapping(keys: ColumnRef*): ObjectMapping =
    ObjectMapping(
      tpe = CategorizedTimeType,
      fieldMappings =
        keyFields(keys: _*) ++ List(
          SqlObject("nonCharged"),
          SqlObject("partner"),
          SqlObject("program")
        )
    )

  private def keyFields(keys: ColumnRef*): List[FieldMapping] =
    keys.toList.zipWithIndex.map { (col, n) =>
      SqlField(s"key_$n", col, key = true, hidden = true)
    }
}
