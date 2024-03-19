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

object PartnerEnumType {

  def fetch[F[_]: Functor](s: Session[F]): F[EnumType] =
    s.execute(sql"select c_tag, c_long_name, c_active from t_partner order by c_tag".query(varchar ~ varchar ~ bool)).map { elems =>
      EnumType(
        "Partner",
        Some("Enumerated type of partners."),
        elems.map { case tag ~ desc ~ _ => EnumValueDefinition(tag.toUpperCase(), Some(desc), Nil) }, // TODO: deprecated directive
        Nil
      )
    }

  }
