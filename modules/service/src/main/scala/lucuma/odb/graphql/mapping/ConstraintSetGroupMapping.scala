// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.syntax.all._
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.Mapping
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Existence
import lucuma.odb.graphql.predicate.Predicates
import table.ObservationView
import lucuma.odb.graphql.table.ConstraintSetGroupView
import binding._
import input._
import table._

trait ConstraintSetGroupMapping[F[_]]
  extends ConstraintSetGroupView[F] 
     with ObservationView[F] 
     with Predicates[F] {

  lazy val ConstraintSetGroupMapping =
    ObjectMapping(
      tpe = ConstraintSetGroupType,
      fieldMappings = List(
        SqlField("key", ConstraintSetGroupView.ConstraintSetKey, key = true, hidden = true),
        SqlField("programId", ConstraintSetGroupView.ProgramId),
        SqlObject("observations", Join(ConstraintSetGroupView.ConstraintSetKey, ObservationView.ConstraintSet.Key)),
        SqlObject("constraintSet", Join(ConstraintSetGroupView.ObservationId, ObservationView.Id)),
      )
    )

  lazy val ConstraintSetGroupElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    Map(
      ConstraintSetGroupType -> {
        case Select("observations", List(
          BooleanBinding("includeDeleted", rIncludeDeleted),
          ObservationIdBinding.Option("OFFSET", rOFFSET),
          NonNegIntBinding.Option("LIMIT", rLIMIT),
        ), child) =>
          (rIncludeDeleted, rOFFSET, rLIMIT).parTupled.flatMap { (includeDeleted, OFFSET, lim) =>
            val limit = lim.fold(ResultMapping.MaxLimit)(_.value)
            ResultMapping.selectResult("observations", child, limit) { q =>
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
    )
}

