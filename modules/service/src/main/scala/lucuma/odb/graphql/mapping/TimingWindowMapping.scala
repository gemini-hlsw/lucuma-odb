// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import lucuma.odb.graphql.table.TimingWindowView

trait TimingWindowMapping[F[_]] extends TimingWindowView[F] {

  lazy val TimingWindowMapping =
    ObjectMapping(
      tpe = TimingWindowType,
      fieldMappings = List(
        SqlField("id", TimingWindowView.Id, key = true),
        SqlField("inclusion", TimingWindowView.Inclusion),
        SqlField("startUtc", TimingWindowView.Start),
        SqlObject("end")
      )
    )

  }

