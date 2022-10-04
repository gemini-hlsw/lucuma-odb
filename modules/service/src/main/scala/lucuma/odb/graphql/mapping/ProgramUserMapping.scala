// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.syntax.all._
import edu.gemini.grackle.Mapping
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Existence

import binding._
import input._
import table._

trait ProgramUserMapping[F[_]]
  extends ProgramTable[F]
     with UserTable[F]
     with ProgramUserTable[F] { this: SkunkMapping[F] =>

  lazy val ProgramUserType = schema.ref("ProgramUser")

  lazy val ProgramUserMapping =
    ObjectMapping(
      tpe = ProgramUserType,
      fieldMappings = List(
        SqlField("programId", ProgramUserTable.ProgramId, hidden = true, key = true),
        SqlField("userId", ProgramUserTable.UserId, key = true),
        SqlField("role", ProgramUserTable.Role),
        SqlObject("user", Join(ProgramUserTable.UserId, UserTable.UserId))
      )
    )

}

