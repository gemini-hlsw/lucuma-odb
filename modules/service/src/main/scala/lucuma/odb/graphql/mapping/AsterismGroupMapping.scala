// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.all.*
import grackle.Predicate
import grackle.Predicate.and
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.Result
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
      ObservationIdBinding.Option("OFFSET_AT", rOFFSET_AT),
      ObservationIdBinding.Option("OFFSET_AFTER", rOFFSET_AFTER),
      NonNegIntBinding.Option("LIMIT", rLIMIT),
    )) =>
      Elab.transformChild: child =>
        val rOffset: Result[Predicate] = PagingOffset.predicate(Predicates.observation.id, rOFFSET, rOFFSET_AT, rOFFSET_AFTER)
        (rIncludeDeleted, rOffset, rLIMIT).parTupled.flatMap: (includeDeleted, offset, lim) =>
          val limit: Int = lim.fold(ResultMapping.MaxLimit)(_.value)
          ResultMapping.selectResult(child, limit): q =>
            FilterOrderByOffsetLimit(
              pred = Some(and(List(
                Predicates.observation.existence.includeDeleted(includeDeleted),
                offset
              ))),
              oss = Some(List(OrderSelection[Observation.Id](ObservationType / "id", true, true))),
              offset = None,
              limit = Some(limit + 1),
              q
            )

}
