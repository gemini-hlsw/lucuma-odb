// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.FilterTypeTable

trait FilterTypeMetaMapping[F[_]] extends FilterTypeTable[F] {

  lazy val FilterTypeMetaMapping =
    ObjectMapping(FilterTypeMetaType)(
      SqlField("tag", FilterTypeTable.Tag, key = true),
      SqlField("shortName", FilterTypeTable.ShortName),
      SqlField("longName", FilterTypeTable.LongName),
    )

}

