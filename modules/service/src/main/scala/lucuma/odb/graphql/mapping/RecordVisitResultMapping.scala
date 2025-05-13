// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.TypeRef

import table.VisitTable

trait RecordVisitResultMapping[F[_]] extends VisitTable[F]:

  private def recordVisitResultMapping(
    typeRef: TypeRef
  ): ObjectMapping =
    ObjectMapping(typeRef)(
      SqlField("id", VisitTable.Id, key = true, hidden = true),
      SqlObject("visit")
    )

  lazy val RecordFlamingos2VisitResultMapping: ObjectMapping =
    recordVisitResultMapping(RecordFlamingos2VisitResultType)

  lazy val RecordGmosNorthVisitResultMapping: ObjectMapping =
    recordVisitResultMapping(RecordGmosNorthVisitResultType)

  lazy val RecordGmosSouthVisitResultMapping: ObjectMapping =
    recordVisitResultMapping(RecordGmosSouthVisitResultType)
