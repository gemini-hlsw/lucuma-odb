// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.graphql.snippet.table.PartnerTable

import table.ObservationView

trait PartnerMetaMapping[F[_]]
  extends PartnerTable[F] { this: SkunkMapping[F] =>

  lazy val PartnerMetaType = schema.ref("PartnerMeta")

  lazy val PartnerMetaMapping =
    ObjectMapping(
      tpe = PartnerMetaType,
      fieldMappings = List(
        SqlField("tag", PartnerTable.Tag, key = true),
        SqlField("shortName", PartnerTable.ShortName),
        SqlField("longName", PartnerTable.LongName),
        SqlField("active", PartnerTable.Active),
      )
    )

}

