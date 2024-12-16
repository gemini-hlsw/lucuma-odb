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

trait ProgramUserMapping[F[_]]
  extends ProgramTable[F]
     with UserTable[F]
     with ProgramUserTable[F] { this: SkunkMapping[F] =>

  lazy val ProgramUserMapping =
    ObjectMapping(ProgramUserType)(
      SqlField("programId", ProgramUserTable.ProgramId, hidden = true, key = true),
      SqlField("userId", ProgramUserTable.UserId, hidden = true, key = true),
      SqlField("role", ProgramUserTable.Role),
      SqlField("linkType", ProgramUserTable.PartnerLink, hidden = true),
      SqlField("partner", ProgramUserTable.Partner, hidden = true),
      SqlField("educationalStatus", ProgramUserTable.EducationalStatus),
      SqlField("thesis", ProgramUserTable.Thesis),
      SqlField("gender", ProgramUserTable.Gender),
      CursorFieldJson("partnerLink", c =>
        for {
          l <- c.fieldAs[PartnerLinkType]("linkType")
          p <- c.fieldAs[Option[Partner]]("partner")
          r <- Result.fromEither(PartnerLink.fromLinkType(l, p))
        } yield r.asJson,
        List("partner", "linkType")
      ),
      SqlObject("program", Join(ProgramUserTable.ProgramId, ProgramTable.Id)),
      SqlObject("user", Join(ProgramUserTable.UserId, UserTable.UserId)),
      SqlObject("fallbackProfile")
    )

}

