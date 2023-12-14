// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.odb.graphql.table.TimeAccountingTable

trait CategorizedTimeMapping[F[_]] extends TimeAccountingTable[F] {

  // Using a `SwitchMapping` here because I expect categorized times to appear
  // elsewhere.
  lazy val CategorizedTimeMapping: TypeMapping =
    SwitchMapping(
      CategorizedTimeType,
      List(
        TimeChargeInvoiceType / "executionTime" -> categorizedTimeMapping(TimeAccountingTable.VisitId),
        TimeChargeInvoiceType / "finalCharge"   -> categorizedTimeMapping(TimeAccountingTable.VisitId)
      )
    )

  private def categorizedTimeMapping(key: ColumnRef): ObjectMapping =
    ObjectMapping(
      tpe = CategorizedTimeType,
      fieldMappings = List(
        SqlField(s"key", key, key = true, hidden = true),
        SqlObject("nonCharged"),
        SqlObject("partner"),
        SqlObject("program")
      )
    )

}
