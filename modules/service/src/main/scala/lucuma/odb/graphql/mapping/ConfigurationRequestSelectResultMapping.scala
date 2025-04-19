// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.odb.graphql.table.*

trait ConfigurationRequestSelectResultMapping[F[_]]
  extends ConfigurationRequestView[F]
     with ProgramTable[F]
     with ResultMapping[F] {

  lazy val ConfigurationRequestSelectResultMapping =
    nestedSelectResultMappingAtPath(ProgramType / "configurationRequests", ProgramTable.Id, Join(ProgramTable.Id, ConfigurationRequestView.ProgramId))

}
