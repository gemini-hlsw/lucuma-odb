// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.PartnerTable

trait PartnerMetaMapping[F[_]] extends PartnerTable[F] {

  lazy val PartnerMetaMapping =
    ObjectMapping(PartnerMetaType)(
      SqlField("tag", PartnerTable.Tag, key = true),
      SqlField("shortName", PartnerTable.ShortName),
      SqlField("longName", PartnerTable.LongName),
      SqlField("active", PartnerTable.Active),
    )

}

