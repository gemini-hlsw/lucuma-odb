// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.GmosLongSlitView

trait ConfigurationGmosLongSlitMapping[F[_]]
  extends GmosLongSlitView[F] {

  lazy val ConfigurationGmosNorthLongSlitMapping: ObjectMapping =
    ObjectMapping(ConfigurationGmosNorthLongSlitType)(
      SqlField("observationId", GmosNorthLongSlitView.Common.ObservationId, key = true, hidden = true),
      SqlField("grating", GmosNorthLongSlitView.Grating),
    )

  lazy val ConfigurationGmosSouthLongSlitMapping: ObjectMapping =
    ObjectMapping(ConfigurationGmosSouthLongSlitType)(
      SqlField("observationId", GmosSouthLongSlitView.Common.ObservationId, key = true, hidden = true),
      SqlField("grating", GmosSouthLongSlitView.Grating),
    )

}
