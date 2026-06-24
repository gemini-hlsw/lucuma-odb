// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ProgramNoteTable
import lucuma.odb.graphql.table.ProgramView

trait ProgramNoteMapping[F[_]] extends ProgramView[F] with ProgramNoteTable[F]:

  lazy val ProgramNoteMapping: ObjectMapping =
    ObjectMapping(ProgramNoteType)(
      SqlField("id", ProgramNoteTable.Id, key = true),
      SqlObject("program", Join(ProgramNoteTable.ProgramId, ProgramView.Id)),
      SqlField("title", ProgramNoteTable.Title),
      SqlField("text", ProgramNoteTable.Text),
      SqlField("isPrivate", ProgramNoteTable.Private),
      SqlField("existence", ProgramNoteTable.Existence)
    )