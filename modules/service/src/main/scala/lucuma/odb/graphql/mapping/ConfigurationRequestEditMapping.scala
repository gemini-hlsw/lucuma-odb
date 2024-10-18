// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping


import lucuma.core.model.ConfigurationRequest
import lucuma.odb.data.EditType
import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.ProgramTable


trait ConfigurationRequestEditMapping[F[_]] extends ConfigurationRequestView[F] with ProgramTable[F] {

  // N.B. env is populated by the subscription elaborator
  lazy val ConfigurationRequestEditMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestEditType)(
      SqlField("synthetic-id", ProgramTable.Id, key = true, hidden = true),
      CursorField("editType", _.envR[EditType]("editType"), List("synthetic-id")),
      CursorField("configurationRequestId", _.envR[ConfigurationRequest.Id]("configurationRequestId"), List("synthetic-id")),
      SqlObject("configurationRequest", Join(ProgramTable.Id, ConfigurationRequestView.ProgramId))
    )

}
