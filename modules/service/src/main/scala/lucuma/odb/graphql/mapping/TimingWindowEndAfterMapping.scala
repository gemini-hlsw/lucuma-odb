// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import lucuma.odb.graphql.table.TimingWindowView

trait TimingWindowEndAfterMapping[F[_]] extends TimingWindowView[F] {

  lazy val TimingWindowEndAfterMapping =
    ObjectMapping(
      tpe = TimingWindowEndAfterType,
      fieldMappings = 
        List(
          SqlField("id", TimingWindowView.End.SyntheticId, key = true),
          SqlObject("duration"),
          SqlObject("repeat")
        )
    )
  }
