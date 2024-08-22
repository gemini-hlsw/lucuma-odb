// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ConfigurationRequestTable

trait ConfigurationRequestMapping[F[_]] extends ConfigurationRequestTable[F] {

  lazy val ConfigurationRequestMapping =
    ObjectMapping(ConfigurationRequestType)(
      SqlField("id", ConfigurationRequestTable.Id, key = true),
      SqlField("status", ConfigurationRequestTable.Status),
      SqlObject("configuration")
    )

}

