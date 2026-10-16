// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.ExecutionTimestampsView

trait ExecutionTimestampsMapping[F[_]] extends ExecutionTimestampsView[F]:
  this: SkunkMapping[F] =>

  lazy val ExecutionTimestampsMapping =
    ObjectMapping(ExecutionTimestampsType)(
      SqlField("observationId", ExecutionTimestampsView.ObservationId, key = true, hidden = true),
      SqlField("firstEventTime", ExecutionTimestampsView.FirstEventTime),
      SqlField("lastEventTime",  ExecutionTimestampsView.LastEventTime),
      SqlField("completionTime", ExecutionTimestampsView.CompletionTime)
    )
