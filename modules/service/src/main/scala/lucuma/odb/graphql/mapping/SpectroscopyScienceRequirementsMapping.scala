// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ObservationView


trait SpectroscopyScienceRequirementsMapping[F[_]]
  extends ObservationView[F]
{ this: SkunkMapping[F] =>

  lazy val SpectroscopyScienceRequirementsType: TypeRef =
    schema.ref("SpectroscopyScienceRequirements")

  import ObservationView.ScienceRequirements.Spectroscopy

  lazy val SpectroscopyScienceRequirementsMapping: ObjectMapping =
    ObjectMapping(
      tpe = SpectroscopyScienceRequirementsType,
      fieldMappings = List(
        SqlField("id", Spectroscopy.SyntheticId, key = true, hidden = true),
        SqlObject("wavelength"),
        SqlField("resolution",           Spectroscopy.Resolution),
        SqlField("signalToNoise",        Spectroscopy.SignalToNoise),
        SqlObject("signalToNoiseAt"),
        SqlObject("wavelengthCoverage"),
        SqlField("focalPlane",           Spectroscopy.FocalPlane),
        SqlObject("focalPlaneAngle"),
        SqlField("capability",           Spectroscopy.Capability)
      )
    )

}
