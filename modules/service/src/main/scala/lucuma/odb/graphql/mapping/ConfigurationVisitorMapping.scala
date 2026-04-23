// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.VisitorTable

trait ConfigurationVisitorMappings[F[_]]
  extends VisitorTable[F]
     with ConfigurationRequestView[F] {

  lazy val ConfigurationVisitorMappings = List(
    ConfigurationVisitorMapping,
    ConfigurationRequestVisitorMapping,
  )

  private lazy val ConfigurationVisitorMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "visitor")(
      SqlField("observationId", VisitorTable.ObservationId, key = true, hidden = true),
      SqlField("mode", VisitorTable.ObservingModeType),
      SqlObject("radius"),
    )

  private lazy val ConfigurationRequestVisitorMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "visitor")(
      SqlField("id", ConfigurationRequestView.Visitor.Id, key = true, hidden = true),
      SqlField("mode", ConfigurationRequestView.Visitor.Mode),
      SqlObject("radius"),
    )

}
