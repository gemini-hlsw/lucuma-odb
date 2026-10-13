// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.syntax.all.*
import grackle.Predicate
import grackle.Predicate.*
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import lucuma.core.model.Observation
import lucuma.odb.graphql.predicate.Predicates

import binding.*
import table.*

trait TargetGroupMapping[F[_]]
  extends TargetView[F] with ProgramView[F] with AsterismTargetTable[F] with ObservationView[F]
     with Predicates[F] {

  lazy val TargetGroupMapping =
    ObjectMapping(TargetGroupType)(
      SqlField("key", TargetView.TargetId, key = true, hidden = true),
      SqlObject("program", Join(TargetView.ProgramId, ProgramView.Id)),
      SqlObject("observations"),
      SqlObject("target"),
    )

  lazy val TargetGroupElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    case (
      TargetGroupType,
      "observations",
      List(
        BooleanBinding("includeDeleted", rIncludeDeleted),
        ObservationIdBinding.Option("OFFSET", rOFFSET),
        ObservationIdBinding.Option("OFFSET_AT", rOFFSET_AT),
        ObservationIdBinding.Option("OFFSET_AFTER", rOFFSET_AFTER),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
      )
    ) =>
      Elab.transformChild { child =>
        val rOffset: Result[Predicate] = PagingOffset.predicate(Predicates.observation.id, rOFFSET, rOFFSET_AT, rOFFSET_AFTER)
        (rIncludeDeleted, rOffset, rLIMIT).parTupled.flatMap { (includeDeleted, offset, lim) =>
          val limit = lim.fold(ResultMapping.MaxLimit)(_.value)
          ResultMapping.selectResult(child, limit) { q =>
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
        }
      }
  }

}

