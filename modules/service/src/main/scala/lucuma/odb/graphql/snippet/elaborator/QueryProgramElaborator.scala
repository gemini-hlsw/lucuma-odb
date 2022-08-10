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
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.TypeRef
import lucuma.core.model.User

trait QueryProgramElaborator[F[_]]
  extends QueryMapping[F]
     with ProgramPredicates[F] { self: SkunkMapping[F] =>

  def user: User

  def QueryProgramElaborator: (TypeRef, PartialFunction[Select, Result[Query]]) =
    QueryType -> {
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
    }

}