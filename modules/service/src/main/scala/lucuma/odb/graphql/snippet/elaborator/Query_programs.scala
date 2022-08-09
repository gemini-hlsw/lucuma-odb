// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package elaborator

import input._
import mapping.QueryMapping

import predicates.ProgramPredicates
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import util.Bindings._
import cats.syntax.all._
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.TypeRef
import lucuma.core.model.User

trait Query_programs[F[_]]
  extends QueryMapping[F]
     with ProgramPredicates[F] { self: SkunkMapping[F] =>

  def Query_programs(user: User): (TypeRef, PartialFunction[Select, Result[Query]]) =
    QueryType -> {
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

}