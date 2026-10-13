// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.all.*
import grackle.Predicate.and
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import lucuma.core.model.Observation
import lucuma.odb.data.Existence
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*

trait AsterismGroupMapping[F[_]]
  extends AsterismGroupView[F]
    with AsterismTargetTable[F]
    with ObservationView[F]
    with ProgramView[F]
    with TargetView[F]
    with Predicates[F] {

  lazy val AsterismGroupMapping: TypeMapping =
    ObjectMapping(AsterismGroupType)(

      // Our key, which is hidden
      SqlField("asterismGroup", AsterismGroupView.AsterismGroup, key = true, hidden = true),

      // User-visible fields
      SqlObject("program", Join(AsterismGroupView.ProgramId, ProgramView.Id)),
      // SqlObject("observations", Join(AsterismGroupView.AsterismGroup, ObservationView.AsterismGroup)),
      SqlObject("observations"),
      SqlObject("asterism", Join(AsterismGroupView.ExampleObservationId, AsterismTargetTable.ObservationId), Join(AsterismTargetTable.TargetId, TargetView.TargetId)),

    )

  lazy val AsterismGroupElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    // Make sure the asterism is filtered by existence
    case (AsterismGroupType, "asterism", Nil) =>
      Elab.transformChild { child =>
        Filter(
          Predicates.target.existence.eql(Existence.Present),
          child
        )
      }

    case (AsterismGroupType, "observations", List(
      BooleanBinding("includeDeleted", rIncludeDeleted),
      ObservationIdBinding.Option("OFFSET", rOFFSET),
      BooleanBinding("OFFSET_INCLUDE", rOFFSET_INCLUDE),
      NonNegIntBinding.Option("LIMIT", rLIMIT),
    )) =>
      Elab.transformChild: child =>
        (rIncludeDeleted, rOFFSET, rOFFSET_INCLUDE, rLIMIT).parTupled.flatMap: (includeDeleted, OFFSET, OFFSET_INCLUDE, lim) =>
          val limit: Int = lim.fold(ResultMapping.MaxLimit)(_.value)
          ResultMapping.selectResult(child, limit): q =>
            FilterOrderByOffsetLimit(
              pred = Some(and(List(
                Predicates.observation.existence.includeDeleted(includeDeleted),
                Predicates.observation.id.offset(OFFSET, OFFSET_INCLUDE)
              ))),
              oss = Some(List(OrderSelection[Observation.Id](ObservationType / "id", true, true))),
              offset = None,
              limit = Some(limit + 1),
              q
            )

}
