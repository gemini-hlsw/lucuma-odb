// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.syntax.all._
import grackle.Predicate
import grackle.Predicate._
import grackle.Query
import grackle.Query._
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import lucuma.core.model.Observation
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.ConstraintSetGroupView

import table.ObservationView
import table.ProgramTable
import binding._

trait ConstraintSetGroupMapping[F[_]]
  extends ConstraintSetGroupView[F]
     with ObservationView[F]
     with ProgramTable[F]
     with Predicates[F] {

  lazy val ConstraintSetGroupMapping =
    ObjectMapping(
      tpe = ConstraintSetGroupType,
      fieldMappings = List(
        SqlField("key", ConstraintSetGroupView.ConstraintSetKey, key = true, hidden = true),
        SqlObject("program", Join(ConstraintSetGroupView.ProgramId, ProgramTable.Id)),
        SqlObject("observations", Join(ConstraintSetGroupView.ConstraintSetKey, ObservationView.ConstraintSet.Key)),
        SqlObject("constraintSet", Join(ConstraintSetGroupView.ObservationId, ObservationView.Id)),
      )
    )

  lazy val ConstraintSetGroupElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (ConstraintSetGroupType, "observations", List(
      BooleanBinding("includeDeleted", rIncludeDeleted),
      ObservationIdBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT),
    )) =>
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

