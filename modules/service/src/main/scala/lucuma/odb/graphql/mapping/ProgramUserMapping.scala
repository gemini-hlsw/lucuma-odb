// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import eu.timepit.refined.cats.*
import grackle.Query.Binding
import grackle.Query.OrderBy
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import io.circe.syntax.*
import lucuma.core.enums.Partner
import lucuma.core.enums.PartnerLinkType
import lucuma.core.model.PartnerLink
import lucuma.core.model.UserInvitation
import lucuma.odb.json.partnerlink.given

import table.*

trait ProgramUserMapping[F[_]]
  extends ProgramTable[F]
     with UserTable[F]
     with UserInvitationTable[F]
     with ProgramUserView[F] { this: SkunkMapping[F] =>

  lazy val ProgramUserMapping =
    ObjectMapping(ProgramUserType)(
      SqlField("id", ProgramUserView.ProgramUserId, key = true),
      SqlField("programId", ProgramUserView.ProgramId, hidden = true),
      SqlField("userId", ProgramUserView.UserId, hidden = true),
      SqlField("role", ProgramUserView.Role),
      SqlField("linkType", ProgramUserView.PartnerLink, hidden = true),
      SqlField("partner", ProgramUserView.Partner, hidden = true),
      SqlField("educationalStatus", ProgramUserView.EducationalStatus),
      SqlField("thesis", ProgramUserView.Thesis),
      SqlField("gender", ProgramUserView.Gender),
      SqlField("affiliation", ProgramUserView.Affiliation),
      SqlField("hasDataAccess", ProgramUserView.HasDataAccess),
      SqlField("displayName", ProgramUserView.DisplayName),
      SqlField("email", ProgramUserView.Email),
      CursorFieldJson("partnerLink", c =>
        for {
          l <- c.fieldAs[PartnerLinkType]("linkType")
          p <- c.fieldAs[Option[Partner]]("partner")
          r <- Result.fromEither(PartnerLink.fromLinkType(l, p))
        } yield r.asJson,
        List("partner", "linkType")
      ),
      SqlObject("program", Join(ProgramUserView.ProgramId, ProgramTable.Id)),
      SqlObject("user", Join(ProgramUserView.UserId, UserTable.UserId)),
      SqlObject("preferredProfile"),
      SqlObject("invitations", Join(ProgramUserView.ProgramUserId, UserInvitationTable.ProgramUserId))
    )

  lazy val ProgramUserElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    case (ProgramUserType, "invitations", Nil) =>
      Elab.transformChild: child =>
        OrderBy(OrderSelections(List(
          OrderSelection[UserInvitation.Id](UserInvitationType / "id")
        )), child)
  }

}
