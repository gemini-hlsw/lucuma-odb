// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.FilterTypeTable

trait FilterTypeMetaMapping[F[_]] extends FilterTypeTable[F] {

  lazy val FilterTypeMetaMapping =
    ObjectMapping(
      tpe = FilterTypeMetaType,
      fieldMappings = List(
        SqlField("tag", FilterTypeTable.Tag, key = true),
        SqlField("shortName", FilterTypeTable.ShortName),
        SqlField("longName", FilterTypeTable.LongName),
      )
    )

}

