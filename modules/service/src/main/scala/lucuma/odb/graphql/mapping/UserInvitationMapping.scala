// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Result
import grackle.skunk.SkunkMapping
import io.circe.syntax.*
import lucuma.core.enums.Partner
import lucuma.core.enums.PartnerLinkType
import lucuma.core.model.PartnerLink
import lucuma.odb.json.partnerlink.given

import table.*

trait UserInvitationMapping[F[_]]
  extends ProgramTable[F]
  with UserTable[F]
  with UserInvitationTable[F]
  with EmailTable[F] {

  lazy val UserInvitationMapping =
    ObjectMapping(UserInvitationType)(
      SqlField("id", UserInvitationTable.InvitationId, key = true),
      SqlField("status", UserInvitationTable.Status),
      SqlObject("issuer", Join(UserInvitationTable.IssuerId, UserTable.UserId)),
      SqlObject("program", Join(UserInvitationTable.ProgramId, ProgramTable.Id)),
      SqlField("recipientEmail", UserInvitationTable.RecipientEmail),
      SqlField("role", UserInvitationTable.Role),
      SqlField("linkType", UserInvitationTable.PartnerLink, hidden = true),
      SqlField("partner", UserInvitationTable.Partner, hidden = true),
      CursorFieldJson("partnerLink", c =>
        for {
          l <- c.fieldAs[PartnerLinkType]("linkType")
          p <- c.fieldAs[Option[Partner]]("partner")
          r <- Result.fromEither(PartnerLink.fromLinkType(l, p))
        } yield r.asJson,
        List("partner", "linkType")
      ),
      SqlObject("redeemer", Join(UserInvitationTable.RedeemerId, UserTable.UserId)),
      SqlObject("email", Join(UserInvitationTable.EmailId, EmailTable.EmailId))
    )

}

