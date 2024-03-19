// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.enums

import cats.Functor
import cats.syntax.all.*
import grackle.EnumType
import grackle.EnumValueDefinition
import skunk.*
import skunk.codec.all.*
import skunk.syntax.all.*

object ProposalAttachmentTypeEnumType {

  def fetch[F[_]: Functor](s: Session[F]): F[EnumType] =
    s.execute(sql"select c_tag, c_long_name from t_proposal_attachment_type order by c_tag".query(varchar ~ varchar)).map { elems =>
      EnumType(
        "ProposalAttachmentType",
        Some("Enumerated type of proposal attachments."),
        elems.map { case tag ~ desc => EnumValueDefinition(tag.toUpperCase(), Some(desc), Nil) },
        Nil
      )
    }
}
