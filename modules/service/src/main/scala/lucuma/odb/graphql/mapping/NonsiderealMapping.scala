// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.TargetView
import table.ProgramTable

trait NonsiderealMapping[F[_]] extends ProgramTable[F] with TargetView[F] {

  lazy val NonsiderealMapping =
    ObjectMapping(NonsiderealType)(
      SqlField("synthetic_id", TargetView.Nonsidereal.SyntheticId, key = true, hidden = true),
      SqlField("des", TargetView.Nonsidereal.Des),
      SqlField("keyType", TargetView.Nonsidereal.KeyType),
      SqlField("key", TargetView.Nonsidereal.Key),
    )

}

