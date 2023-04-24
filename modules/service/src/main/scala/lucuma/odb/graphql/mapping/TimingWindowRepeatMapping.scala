// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import lucuma.odb.graphql.table.TimingWindowTable

trait TimingWindowRepeatMapping[F[_]] extends TimingWindowTable[F] {

  lazy val TimingWindowRepeatMapping =
    ObjectMapping(
      tpe = TimingWindowRepeatType,
      fieldMappings = 
        List(
          SqlField("id", TimingWindowTable.Id, key = true),
          SqlObject("period"),
          SqlField("times", TimingWindowTable.RepeatTimes),
        )
    )
  }
