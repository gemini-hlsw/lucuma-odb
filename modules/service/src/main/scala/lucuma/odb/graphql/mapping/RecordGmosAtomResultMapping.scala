// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import edu.gemini.grackle.TypeRef

import table.AtomTable

trait RecordGmosAtomResultMapping[F[_]] extends AtomTable[F] {

  private def recordAtomResultMapping(
    typeRef: TypeRef
  ): ObjectMapping =
    ObjectMapping(
      tpe = typeRef,
      fieldMappings = List(
        SqlField("id", AtomTable.Id, key = true),
        SqlObject("atomRecord")
      )
    )

  lazy val RecordGmosNorthAtomResultMapping: ObjectMapping =
    recordAtomResultMapping(RecordGmosNorthAtomResultType)

  lazy val RecordGmosSouthAtomResultMapping: ObjectMapping =
    recordAtomResultMapping(RecordGmosSouthAtomResultType)

}
