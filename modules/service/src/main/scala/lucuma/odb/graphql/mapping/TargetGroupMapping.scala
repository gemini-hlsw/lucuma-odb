// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.syntax.all.*
import grackle.Predicate
import grackle.Predicate.*
import grackle.Query
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import lucuma.core.model.Observation
import lucuma.odb.graphql.predicate.Predicates

import table.ObservationView
import binding.*
import table.*

trait TargetGroupMapping[F[_]]
  extends TargetView[F] with ProgramTable[F] with AsterismTargetTable[F] with ObservationView[F]
     with Predicates[F] {

  lazy val TargetGroupMapping =
    ObjectMapping(TargetGroupType)(
      SqlField("key", TargetView.TargetId, key = true, hidden = true),
      SqlObject("program", Join(TargetView.ProgramId, ProgramTable.Id)),
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
        NonNegIntBinding.Option("LIMIT", rLIMIT),
      )
    ) => 
      Elab.transformChild { child =>
        (rIncludeDeleted, rOFFSET, rLIMIT).parTupled.flatMap { (includeDeleted, OFFSET, lim) =>
          val limit = lim.fold(ResultMapping.MaxLimit)(_.value)
          ResultMapping.selectResult(child, limit) { q =>
            FilterOrderByOffsetLimit(
              pred = Some(and(List(
                Predicates.observation.existence.includeDeleted(includeDeleted),
                OFFSET.fold[Predicate](True)(Predicates.observation.id.gtEql)
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

