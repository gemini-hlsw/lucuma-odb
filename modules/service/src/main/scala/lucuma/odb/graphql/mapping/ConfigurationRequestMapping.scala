// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.ProgramTable

trait ConfigurationRequestMapping[F[_]] extends ConfigurationRequestView[F] with ProgramTable[F] {

  lazy val ConfigurationRequestMapping =
    ObjectMapping(ConfigurationRequestType)(
      SqlField("id", ConfigurationRequestView.Id, key = true),
      SqlField("status", ConfigurationRequestView.Status),
      SqlObject("program", Join(ConfigurationRequestView.ProgramId, ProgramTable.Id)),
      SqlObject("configuration")
    )

}

