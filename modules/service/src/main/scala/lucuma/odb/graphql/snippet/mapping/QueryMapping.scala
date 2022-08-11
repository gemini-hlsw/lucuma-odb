// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import lucuma.odb.instances.given
import lucuma.core.model.User
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.Predicate._
import lucuma.odb.graphql.snippet.predicates.ObservationPredicates
import lucuma.odb.graphql.snippet.predicates.ProgramPredicates
import scala.reflect.ClassTag
import lucuma.odb.graphql.util.Bindings._
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.TypeRef
import lucuma.odb.graphql.snippet.input.WhereProgram
import cats.syntax.all._
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.graphql.snippet.input.WhereObservation
import cats.effect.kernel.Par
import lucuma.odb.data.Tag

trait QueryMapping[F[_]]
  extends ObservationPredicates[F]
     with ProgramPredicates[F]
 { this: SkunkMapping[F] =>

  lazy val QueryType = schema.ref("Query")

  lazy val QueryMapping =
    ObjectMapping(
      tpe = QueryType,
      fieldMappings = List(
        SqlRoot("observation"),
        SqlRoot("observations"),
        SqlRoot("partnerMeta"),
        SqlRoot("program"),
        SqlRoot("programs"),
      )
    )

  lazy val QueryElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    List(
      Observation,
      Observations,
      PartnerMeta,
      Program,
      Programs,
    ).foldMap(pf => Map(QueryType -> pf))

  def user: User

  private val Observation: PartialFunction[Select, Result[Query]] =
    case Select("observation", List(
      ObservationIdBinding("observationId", rOid)
    ), child) =>
      rOid.map { oid =>
        Select("observation", Nil,
          Unique(
            Filter(
              And(
                ObservationPredicates.hasObservationId(oid),
                ObservationPredicates.isVisibleTo(user)
              ),
              child
            )
          )
        )
      }

  private val Observations: PartialFunction[Select, Result[Query]] =
    case Select("observations", List(
      WhereObservation.Binding.Option("WHERE", rWHERE),
      ObservationIdBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT),
      BooleanBinding("includeDeleted", rIncludeDeleted)
    ), child) =>
      (rWHERE, rOFFSET, rLIMIT, rIncludeDeleted).parMapN { (WHERE, OFFSET, LIMIT, includeDeleted) =>
        Select("observations", Nil,
          Limit(
            LIMIT.foldLeft(1000)(_ min _.value),  // TODO: we need a common place for the max limit
            Filter(
              And.all(
                OFFSET.map(oid => GtEql(UniquePath(List("id")), Const(oid))).getOrElse(True),
                ObservationPredicates.includeDeleted(includeDeleted),
                ObservationPredicates.isVisibleTo(user),
                WHERE.getOrElse(True)
              ),
              child
            )
          )
        )
      }

  private val PartnerMeta: PartialFunction[Select, Result[Query]] =
    case Select("partnerMeta", Nil, child) =>
      Result(Select("partnerMeta", Nil,
        OrderBy(OrderSelections(List(OrderSelection(UniquePath[Tag](List("tag"))))), child)
      ))

  private val Program: PartialFunction[Select, Result[Query]] =
    case Select("program", List(
      ProgramIdBinding("programId", rPid),
    ), child) =>
      rPid.map { pid =>
        Select("program", Nil,
          Unique(
            Filter(
              And(
                ProgramPredicates.hasProgramId(pid),
                ProgramPredicates.isVisibleTo(user),
              ),
              child
            )
          )
        )
      }

  private val Programs: PartialFunction[Select, Result[Query]] =
    case Select("programs", List(
      WhereProgram.Binding.Option("WHERE", rWHERE),
      ProgramIdBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT),
      BooleanBinding("includeDeleted", rIncludeDeleted)
    ), child) =>
      (rWHERE, rOFFSET, rLIMIT, rIncludeDeleted).parMapN { (WHERE, OFFSET, LIMIT, includeDeleted) =>
        Select("programs", Nil,
          Limit(
            LIMIT.foldLeft(1000)(_ min _.value),
            Filter(
              And.all(
                OFFSET.map(pid => GtEql(UniquePath(List("id")), Const(pid))).getOrElse(True),
                ProgramPredicates.includeDeleted(includeDeleted),
                ProgramPredicates.isVisibleTo(user),
                WHERE.getOrElse(True)
              ),
              child
            )
          )
        )
      }

}