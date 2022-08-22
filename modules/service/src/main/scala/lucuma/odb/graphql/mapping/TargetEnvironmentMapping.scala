// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping

import table.ObservationView

trait TargetEnvironmentMapping[F[_]]
  extends ObservationView[F] { this: SkunkMapping[F] =>

  lazy val TargetEnvironmentType: TypeRef =
    schema.ref("TargetEnvironment")

  lazy val TargetEnvironmentMapping: ObjectMapping =
    ObjectMapping(
      tpe = TargetEnvironmentType,
      fieldMappings = List(
        SqlField("id", ObservationView.Id, key = true, hidden = true),
        SqlObject("explicitBase")
      )
    )

}

