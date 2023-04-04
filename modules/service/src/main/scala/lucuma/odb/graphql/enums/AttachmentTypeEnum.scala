// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.enums

import cats.Functor
import cats.syntax.all._
import edu.gemini.grackle.EnumType
import edu.gemini.grackle.EnumValue
import lucuma.odb.util.Codecs._
import skunk._
import skunk.codec.all._
import skunk.syntax.all._

object AttachmentTypeEnumType {

  def fetch[F[_]: Functor](s: Session[F]): F[EnumType] =
    s.execute(sql"select c_tag, c_long_name from t_attachment_type order by c_tag".query(varchar ~ varchar)).map { elems =>
      EnumType(
        "AttachmentType",
        Some("Enumerated type of attachments."),
        elems.map { case tag ~ desc => EnumValue(tag.toUpperCase(), Some(desc)) }
      )
    }
}
