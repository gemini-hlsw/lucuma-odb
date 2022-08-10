// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package elaborator

import input.CreateProgramInput
import mapping.MutationMapping

import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.TypeRef
import lucuma.core.model.User

trait MutationCreateProgramElaborator[F[_]]
  extends MutationMapping[F] { self: SkunkMapping[F] =>

  lazy val MutationCreateProgramElaborator: (TypeRef, PartialFunction[Select, Result[Query]]) =
      MutationType -> {

        case Select("createProgram", List(
          CreateProgramInput.Binding("input", rInput)
        ), child) =>
          rInput.map { input =>
            Environment(
              Env("name" -> input.SET.name),
              Select("createProgram", Nil, child)
            )
          }

    }

}