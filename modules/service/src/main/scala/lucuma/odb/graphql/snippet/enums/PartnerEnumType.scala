// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet.enums

import cats.Functor
import cats.syntax.all._
import edu.gemini.grackle.EnumType
import edu.gemini.grackle.EnumValue
import lucuma.odb.util.Codecs._
import skunk._
import skunk.codec.all._
import skunk.syntax.all._

object PartnerEnumType {

  def fetch[F[_]: Functor](s: Session[F]): F[EnumType] =
    s.execute(sql"select c_tag, c_long_name, c_active from t_partner order by c_tag".query(varchar ~ varchar ~ bool)).map { elems =>
      EnumType(
        "Partner",
        Some("Enumerated type of partners."),
        elems.map { case tag ~ desc ~ active => EnumValue(tag.toUpperCase(), Some(desc), !active) }
      )
    }

  }