// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import grackle.Result
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.asFailure
import lucuma.odb.data.ProgramUserRole.Coi
import lucuma.odb.data.ProgramUserRole.CoiRO
import lucuma.odb.data.ProgramUserRole.Support
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
        PartnerBinding.Option("partner", rPartner),
      ) =>
        (rProgramId, rUserId, rRole, rPartner).parTupled.flatMap {
          case (pid, uid, Coi, Some(partner)) => Result(LinkUserRequest.Coi(pid, partner, uid))
          case (pid, uid, CoiRO, Some(partner)) => Result(LinkUserRequest.CoiRo(pid, partner, uid))
          case (pid, uid, Support, None) => Result(LinkUserRequest.Support(pid, uid))
          case (_, _, Coi | CoiRO, None) => OdbError.InvalidArgument("A partner must be specified for co-investigators.".some).asFailure
          case (_, _, Support, Some(_)) => OdbError.InvalidArgument("A partner may not be specified for support users.".some).asFailure
        }
    }

}

