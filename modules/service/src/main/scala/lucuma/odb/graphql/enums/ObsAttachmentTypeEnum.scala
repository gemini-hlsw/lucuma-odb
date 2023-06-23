// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.enums

import cats.Functor
import cats.syntax.all._
import edu.gemini.grackle.EnumType
import edu.gemini.grackle.EnumValue
import skunk._
import skunk.codec.all._
import skunk.syntax.all._

object ObsAttachmentTypeEnumType {

  def fetch[F[_]: Functor](s: Session[F]): F[EnumType] =
    s.execute(sql"select c_tag, c_long_name from t_obs_attachment_type order by c_tag".query(varchar ~ varchar)).map { elems =>
      EnumType(
        "ObsAttachmentType",
        Some("Enumerated type of observation attachments."),
        elems.map { case tag ~ desc => EnumValue(tag.toUpperCase(), Some(desc)) }
      )
    }
}
