// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Query
import grackle.Query.Binding
import grackle.Query.Filter
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import lucuma.odb.data.Existence
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*

trait AsterismGroupMapping[F[_]]
  extends AsterismGroupView[F]
    with AsterismTargetTable[F]
    with ObservationView[F]
    with ProgramTable[F]
    with TargetView[F]
    with Predicates[F] {

  lazy val AsterismGroupMapping: TypeMapping =
    ObjectMapping(AsterismGroupType)(

      // Our key, which is hidden
      SqlField("asterismGroup", AsterismGroupView.AsterismGroup, key = true, hidden = true),

      // User-visible fields
      SqlObject("program", Join(AsterismGroupView.ProgramId, ProgramTable.Id)),
      // SqlObject("observations", Join(AsterismGroupView.AsterismGroup, ObservationView.AsterismGroup)),
      SqlObject("observations"),
      SqlObject("asterism", Join(AsterismGroupView.ExampleObservationId, AsterismTargetTable.ObservationId), Join(AsterismTargetTable.TargetId, TargetView.TargetId)),

    )

  // Make sure the asterism is filtered by existence
  lazy val AsterismGroupElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (AsterismGroupType, "asterism", Nil) =>
      Elab.transformChild { child =>
        Filter(
          Predicates.target.existence.eql(Existence.Present),
          child
        )
      }

}
