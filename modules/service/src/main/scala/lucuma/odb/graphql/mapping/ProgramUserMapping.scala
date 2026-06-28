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
import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.Partner
import lucuma.core.enums.PartnerLinkType
import lucuma.core.model.PartnerLink
import lucuma.core.model.UserInvitation
import lucuma.odb.json.partnerlink.given

import table.*

trait ProgramUserMapping[F[_]]
  extends ProgramView[F]
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
      SqlField("geminiPartner", ProgramUserView.GeminiPartner, hidden = true),
      SqlField("exchangePartner", ProgramUserView.ExchangePartner, hidden = true),
      SqlField("educationalStatus", ProgramUserView.EducationalStatus),
      SqlField("thesis", ProgramUserView.Thesis),
      SqlField("gender", ProgramUserView.Gender),
      SqlField("affiliation", ProgramUserView.Affiliation),
      SqlField("hasDataAccess", ProgramUserView.HasDataAccess),
      SqlField("classicalVisitor", ProgramUserView.ClassicalVisitor),
      SqlField("displayName", ProgramUserView.DisplayName),
      SqlField("email", ProgramUserView.Email),
      CursorFieldJson("partnerLink", c =>
        for {
          l <- c.fieldAs[PartnerLinkType]("linkType")
          p <- c.fieldAs[Option[Partner]]("geminiPartner")
          e <- c.fieldAs[Option[ExchangePartner]]("exchangePartner")
          r <- Result.fromEither(PartnerLink.fromLinkType(l, p, e))
        } yield r.asJson,
        List("geminiPartner", "exchangePartner", "linkType")
      ),
      SqlObject("program", Join(ProgramUserView.ProgramId, ProgramView.Id)),
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
