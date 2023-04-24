// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import lucuma.odb.graphql.table.TimingWindowTable

trait TimingWindowMapping[F[_]] extends TimingWindowTable[F] {

  lazy val TimingWindowMapping =
    ObjectMapping(
      tpe = TimingWindowType,
      fieldMappings = List(
        SqlField("id", TimingWindowTable.Id, key = true),
        SqlField("inclusion", TimingWindowTable.Inclusion),
        SqlField("start", TimingWindowTable.Start),
        SqlObject("end")
      )
    )

  }

