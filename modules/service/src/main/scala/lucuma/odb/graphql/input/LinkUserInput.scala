// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import grackle.Result
import lucuma.odb.graphql.binding.*
import lucuma.odb.service.ProgramService
import lucuma.odb.service.ProgramService.LinkUserRequest

object LinkUserInput {

  val Binding: Matcher[LinkUserRequest] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding("programId", rProgramId),
        UserIdBinding("userId", rUserId),
        ProgramUserRoleBinding("role", rRole),
      ) =>
        (rProgramId, rUserId, rRole).parTupled.map { (pid, uid, role) =>
          ProgramService.LinkUserRequest(role, pid, uid) 
        }
    }

}

