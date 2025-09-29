// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ExposureTimeModeView
import lucuma.odb.graphql.table.ObservationView

trait ScienceRequirementsMapping[F[_]] extends ObservationView[F] with ExposureTimeModeView[F]:

  lazy val ScienceRequirementsMapping: ObjectMapping =
    ObjectMapping(ScienceRequirementsType)(
      SqlField("id", ObservationView.Id, key = true, hidden = true),
      SqlField("mode", ObservationView.ScienceRequirements.Mode),
      SqlObject(
        "exposureTimeMode",
         Join(ObservationView.Id, ExposureTimeModeLink.ObservationId),
         Join(ExposureTimeModeLink.ExposureTimeModeId, ExposureTimeModeView.Id)
      ),
      SqlObject("spectroscopy"),
      SqlObject("imaging")
    )