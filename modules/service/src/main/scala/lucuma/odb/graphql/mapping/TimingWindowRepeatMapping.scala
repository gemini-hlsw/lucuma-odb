// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import lucuma.odb.graphql.table.TimingWindowView

trait TimingWindowRepeatMapping[F[_]] extends TimingWindowView[F] {

  lazy val TimingWindowRepeatMapping =
    ObjectMapping(
      tpe = TimingWindowRepeatType,
      fieldMappings = 
        List(
          SqlField("id", TimingWindowView.End.Repeat.SyntheticId, key = true),
          SqlObject("period"),
          SqlField("times", TimingWindowView.End.Repeat.Times),
        )
    )
  }
