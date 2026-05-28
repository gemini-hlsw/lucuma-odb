// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.GnirsStaticTable

trait GnirsStaticMapping[F[_]] extends GnirsStaticTable[F]:

  lazy val GnirsStaticMapping: ObjectMapping =
    ObjectMapping(GnirsStaticType)(
      SqlField("id",        GnirsStaticTable.Id, key = true, hidden = true),
      SqlField("wellDepth", GnirsStaticTable.WellDepth)
    )
