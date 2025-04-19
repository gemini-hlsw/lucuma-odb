// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.TargetView
import table.ProgramTable

trait CatalogInfoMapping[F[_]] extends ProgramTable[F] with TargetView[F]  {

  lazy val CatalogInfoMapping =
    ObjectMapping(CatalogInfoType)(
      SqlField("synthetic_id", TargetView.Sidereal.Catalog.SyntheticId, key = true, hidden = true),
      SqlField("name", TargetView.Sidereal.Catalog.Name),
      SqlField("id", TargetView.Sidereal.Catalog.Id),
      SqlField("objectType", TargetView.Sidereal.Catalog.ObjectType),
    )

}

