// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.AttachmentTypeTable

trait AttachmentTypeMetaMapping[F[_]] extends AttachmentTypeTable[F] {

  lazy val AttachmentTypeMetaMapping =
    ObjectMapping(
      tpe = AttachmentTypeMetaType,
      fieldMappings = List(
        SqlField("tag", AttachmentTypeTable.Tag, key = true),
        SqlField("shortName", AttachmentTypeTable.ShortName),
        SqlField("longName", AttachmentTypeTable.LongName)
      )
    )
}
