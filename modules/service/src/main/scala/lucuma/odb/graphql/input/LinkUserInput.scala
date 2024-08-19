// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import grackle.Result
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.asFailure
import lucuma.core.model.PartnerLink
import lucuma.core.model.PartnerLink.HasUnspecifiedPartner
import lucuma.core.enums.ProgramUserRole.Coi
import lucuma.core.enums.ProgramUserRole.CoiRO
import lucuma.core.enums.ProgramUserRole.Pi
import lucuma.core.enums.ProgramUserRole.Support
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
        PartnerLinkInput.Binding.Option("partnerLink", rPartnerLink),
      ) =>
        (rProgramId, rUserId, rRole, rPartnerLink).parTupled.flatMap {
          case (pid, uid, Coi, p) => Result(LinkUserRequest.Coi(pid, p.getOrElse(HasUnspecifiedPartner), uid))
          case (pid, uid, CoiRO, p) => Result(LinkUserRequest.CoiRo(pid, p.getOrElse(HasUnspecifiedPartner), uid))
          case (pid, uid, Support, None) => Result(LinkUserRequest.Support(pid, uid))
          case (pid, uid, Support, Some(PartnerLink.HasUnspecifiedPartner)) => Result(LinkUserRequest.Support(pid, uid))
          case (_, _, Support, _) => OdbError.InvalidArgument("A partnerLink may not be specified for support users.".some).asFailure
          case (_, _, Pi, _) => OdbError.InvalidArgument("PIs are linked at program creation time.".some).asFailure
        }
    }

}

