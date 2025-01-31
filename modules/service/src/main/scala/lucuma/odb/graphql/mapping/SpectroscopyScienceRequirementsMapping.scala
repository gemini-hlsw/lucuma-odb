// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ObservationView


trait SpectroscopyScienceRequirementsMapping[F[_]] extends ObservationView[F] {

  import ObservationView.ScienceRequirements.Spectroscopy

  lazy val SpectroscopyScienceRequirementsMapping: ObjectMapping =
    ObjectMapping(SpectroscopyScienceRequirementsType)(
      SqlField("id", ObservationView.Id, key = true, hidden = true),
      SqlObject("wavelength"),
      SqlField("resolution",           Spectroscopy.Resolution),
      SqlObject("exposureTimeMode"),
      SqlObject("wavelengthCoverage"),
      SqlField("focalPlane",           Spectroscopy.FocalPlane),
      SqlObject("focalPlaneAngle"),
      SqlField("capability",           Spectroscopy.Capability)
    )

}
