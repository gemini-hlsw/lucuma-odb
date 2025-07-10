// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Query.Binding
import grackle.Query.OrderBy
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import lucuma.odb.graphql.table.AtomDigestTable
import lucuma.odb.graphql.table.ObscalcTable

trait CalculatedAtomDigestsMapping[F[_]] extends ObscalcTable[F]
                                            with AtomDigestTable[F]:

  lazy val CalculatedAtomDigestsMapping: ObjectMapping =
    ObjectMapping(CalculatedAtomDigestsType)(
      SqlField("synthetic_id",  ObscalcTable.ObservationId, key = true),
      SqlField("state",         ObscalcTable.Workflow.State),
      SqlObject("value",        Join(ObscalcTable.ObservationId, AtomDigestTable.ObservationId))
    )

  lazy val CalculatedAtomDigestsElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (CalculatedAtomDigestsType, "value", Nil) =>
      Elab.transformChild: child =>
        OrderBy(
          OrderSelections(List(OrderSelection[Short](AtomDigestType / "index"))),
          child
        )