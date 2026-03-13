// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Result
import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.Igrins2LongSlitView

trait ConfigurationIgrins2LongSlitMappings[F[_]]
  extends Igrins2LongSlitView[F]
     with ConfigurationRequestView[F] {

  lazy val ConfigurationIgrins2LongSlitMappings = List(
    ConfigurationIgrins2LongSlitMapping,
    ConfigurationRequestIgrins2LongSlitMapping,
  )

  private lazy val ConfigurationIgrins2LongSlitMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "igrins2LongSlit")(
      SqlField("observationId", Igrins2LongSlitView.ObservationId, key = true, hidden = true),
      CursorField[Option[String]]("ignore", _ => Result(None), Nil),
    )

  private lazy val ConfigurationRequestIgrins2LongSlitMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "igrins2LongSlit")(
      SqlField("id", ConfigurationRequestView.Igrins2LongSlit.Id, key = true, hidden = true),
      CursorField[Option[String]]("ignore", _ => Result(None), Nil),
    )

}
