// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import table._
import input._

import cats.syntax.all._
import edu.gemini.grackle.Mapping
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.TypeRef
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Existence
import util.Bindings._
import edu.gemini.grackle.Predicate
import lucuma.core.model.Observation

trait ProgramMapping[F[_]]
  extends ProgramTable[F]
     with UserTable[F]
     with ProgramUserTable[F]
     with ObservationView[F] { this: SkunkMapping[F] =>

  def user: User

  lazy val ProgramType = schema.ref("Program")

  lazy val ProgramMapping =
    ObjectMapping(
      tpe = ProgramType,
      fieldMappings = List(
        SqlField("id", ProgramTable.Id, key = true),
        SqlField("existence", ProgramTable.Existence, hidden = true),
        SqlField("name", ProgramTable.Name),
        SqlField("piUserId", ProgramTable.PiUserId, hidden = true),
        SqlObject("pi", Join(ProgramTable.PiUserId, UserTable.UserId)),
        SqlObject("users", Join(ProgramTable.Id, ProgramUserTable.ProgramId)),
        SqlObject("plannedTime"),
        SqlObject("observations", Join(ProgramTable.Id, ObservationView.ProgramId)),
      ),
    )

  lazy val ProgramElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    Map(
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
    )

}

