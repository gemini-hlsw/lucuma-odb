// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.util.Bindings._
import lucuma.odb.service.ProgramService
import lucuma.odb.service.ProgramService.LinkUserRequest

object LinkUserInput {

  val Binding: Matcher[LinkUserRequest] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding("programId", rProgramId),
        UserIdBinding("userId", rUserId),
        ProgramUserRoleBinding("role", rRole),
        ProgramUserSupportRoleTypeBinding.Option("supportType", rSupportType),
        TagBinding.Option("supportPartner", rPartner),
      ) =>
        (rProgramId, rUserId, rRole, rSupportType, rPartner).parTupled.flatMap { (pid, uid, role, tpe, tag) =>
          ProgramService.LinkUserRequest.validate(pid, uid, role, tpe, tag) match {
            case Left(err)  => Result.failure(err)
            case Right(req) => Result(req)
          }
        }
    }

}

