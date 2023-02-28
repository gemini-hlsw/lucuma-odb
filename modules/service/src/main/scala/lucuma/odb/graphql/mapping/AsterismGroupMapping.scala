// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table._
import edu.gemini.grackle.Query.FilterOrderByOffsetLimit
import edu.gemini.grackle.Predicate.*
import edu.gemini.grackle.Query.Filter
import edu.gemini.grackle.Query.Select
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.Result
import edu.gemini.grackle.Query
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.data.Existence

trait AsterismGroupMapping[F[_]] 
  extends AsterismGroupView[F] 
    with AsterismTargetTable[F]
    with ObservationView[F] 
    with ProgramTable[F]
    with TargetView[F]
    with Predicates[F] {

  lazy val AsterismGroupMapping: TypeMapping =
    ObjectMapping(
      tpe = AsterismGroupType,
      fieldMappings = List(

        // Our key, which is hidden
        SqlField("asterismGroup", AsterismGroupView.AsterismGroup, key = true, hidden = true),

        // User-visible fields
        SqlObject("program", Join(AsterismGroupView.ProgramId, ProgramTable.Id)),
        SqlObject("observations", Join(AsterismGroupView.AsterismGroup, ObservationView.AsterismGroup)),
        SqlObject("asterism", Join(AsterismGroupView.ExampleObservationId, AsterismTargetTable.ObservationId), Join(AsterismTargetTable.TargetId, TargetView.TargetId)),

      )
    )

  // Make sure the asterism is filtered by existence
  lazy val AsterismGroupElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    Map(
      AsterismGroupType -> {
        case Select("asterism", Nil, child) =>
          Result(
            Select("asterism", Nil, 
              Filter(
                Predicates.target.existence.eql(Existence.Present),
                child
              )
            )
          )
      }
    )

}
