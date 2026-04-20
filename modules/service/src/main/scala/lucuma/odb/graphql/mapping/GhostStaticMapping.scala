// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.GhostStaticTable

trait GhostStaticMapping[F[_]] extends GhostStaticTable[F]:

  lazy val GhostStaticMapping: ObjectMapping =
    ObjectMapping(GhostStaticType)(
      SqlField("id", GhostStaticTable.Id, key = true, hidden = true),
      SqlField("resolutionMode", GhostStaticTable.ResolutionMode)
    )