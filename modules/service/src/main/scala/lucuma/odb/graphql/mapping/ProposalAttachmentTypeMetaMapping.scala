// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ProposalAttachmentTypeTable

trait ProposalAttachmentTypeMetaMapping[F[_]] extends ProposalAttachmentTypeTable[F] {

  lazy val ProposalAttachmentTypeMetaMapping =
    ObjectMapping(
      tpe = ProposalAttachmentTypeMetaType,
      fieldMappings = List(
        SqlField("tag", ProposalAttachmentTypeTable.Tag, key = true),
        SqlField("shortName", ProposalAttachmentTypeTable.ShortName),
        SqlField("longName", ProposalAttachmentTypeTable.LongName)
      )
    )
}
