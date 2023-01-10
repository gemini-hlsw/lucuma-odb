// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.syntax.all._
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
import binding._
import input._
import table._

trait TargetGroupMapping[F[_]]
  extends TargetView[F] with ProgramTable[F] with AsterismTargetTable[F] with ObservationView[F]
     with Predicates[F] {

  lazy val TargetGroupMapping =
    ObjectMapping(
      tpe = TargetGroupType,
      fieldMappings = List(
        SqlField("key", TargetView.TargetId, key = true, hidden = true),
        SqlObject("program", Join(TargetView.ProgramId, ProgramTable.Id)),
        SqlObject("observations"),
        SqlObject("target"),
      )
    )

  lazy val TargetGroupElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    Map(
      TargetGroupType -> {
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

