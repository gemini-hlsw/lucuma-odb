// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import table.TargetView
import table.ProgramTable

import edu.gemini.grackle.skunk.SkunkMapping

trait NonsiderealMapping[F[_]]
  extends ProgramTable[F]
     with TargetView[F] { this: SkunkMapping[F] =>

  lazy val NonsiderealType = schema.ref("Nonsidereal")

  lazy val NonsiderealMapping =
    ObjectMapping(
      tpe = NonsiderealType,
      fieldMappings = List(
        SqlField("synthetic_id", TargetView.Nonsidereal.SyntheticId, key = true, hidden = true),
        SqlField("des", TargetView.Nonsidereal.Des),
        SqlField("keyType", TargetView.Nonsidereal.KeyType),
        SqlField("key", TargetView.Nonsidereal.Key),
      )
    )

  }

