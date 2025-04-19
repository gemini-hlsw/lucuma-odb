// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ProgramNoteTable

trait CreateProgramNoteResultMapping[F[_]] extends ProgramNoteTable[F]:

  lazy val CreateProgramNoteResultMapping =
    ObjectMapping(CreateProgramNoteResultType)(
      SqlField("id", ProgramNoteTable.Id, key = true, hidden = true),
      SqlObject("programNote")
    )