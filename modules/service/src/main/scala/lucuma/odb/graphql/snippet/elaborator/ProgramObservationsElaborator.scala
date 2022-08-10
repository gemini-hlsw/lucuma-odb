// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package elaborator

import mapping.ProgramMapping
import predicates.ProgramPredicates
import util.Bindings._

import cats.syntax.all._
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.TypeRef
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.data.Existence

trait ProgramObservationsElaborator[F[_]]
  extends ProgramMapping[F]
     with ProgramPredicates[F] { self: SkunkMapping[F] =>

  def user: User

  def ProgramObservationsElaborator: (TypeRef, PartialFunction[Select, Result[Query]]) =
      ProgramType -> {

        case Select("observations", List(
          BooleanBinding("includeDeleted", rIncludeDeleted),
          ObservationIdBinding.Option("OFFSET", rOFFSET),
          NonNegIntBinding.Option("LIMIT", rLIMIT),
        ), child) =>
          (rIncludeDeleted, rOFFSET, rLIMIT).parMapN { (includeDeleted, OFFSET, _) =>
            Select("observations", Nil,
              Filter(and(List(
                if (includeDeleted) True else Eql[Existence](UniquePath(List("existence")), Const(Existence.Present)),
                OFFSET.fold[Predicate](True)(o => GtEql[Observation.Id](UniquePath(List("id")), Const(o))),
              )),
              child
              )
            )
          }

      }

}