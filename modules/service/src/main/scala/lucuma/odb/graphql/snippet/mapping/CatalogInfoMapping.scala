// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import table.TargetView
import table.ProgramTable

import edu.gemini.grackle.skunk.SkunkMapping

trait CatalogInfoMapping[F[_]]
  extends ProgramTable[F]
     with TargetView[F] { this: SkunkMapping[F] =>

  lazy val CatalogInfoType = schema.ref("CatalogInfo")

  lazy val CatalogInfoMapping =
    ObjectMapping(
      tpe = CatalogInfoType,
      fieldMappings = List(
        SqlField("synthetic_id", TargetView.Sidereal.Catalog.SyntheticId, key = true, hidden = true),
        SqlField("name", TargetView.Sidereal.Catalog.Name),
        SqlField("id", TargetView.Sidereal.Catalog.Id),
        SqlField("objectType", TargetView.Sidereal.Catalog.ObjectType),
      )
    )

  }

