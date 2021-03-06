// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet

import cats.Functor
import cats.syntax.all._
import edu.gemini.grackle.EnumType
import edu.gemini.grackle.EnumValue
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query.OrderBy
import edu.gemini.grackle.Query.OrderSelection
import edu.gemini.grackle.Query.OrderSelections
import edu.gemini.grackle.Query.Select
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.data.Tag
import lucuma.odb.graphql.util._
import lucuma.odb.util.Codecs._
import skunk._
import skunk.codec.all._
import skunk.syntax.all._

object PartnerSnippet {

  def apply[F[_]](m: SnippetMapping[F] with SkunkMapping[F]): m.Snippet = {
    import m.{ TableDef, ObjectMapping, Snippet, SqlRoot, SqlField, LeafMapping, col, schema }

    val QueryType        = schema.ref("Query")
    val PartnerType      = schema.ref("Partner")
    val PartnerMetaType  = schema.ref("PartnerMeta")

    object Partner extends TableDef("t_partner") {
      val Tag       = col("c_tag", tag)
      val ShortName = col("c_short_name", varchar)
      val LongName  = col("c_long_name", varchar)
      val Active    = col("c_active", bool)
      // val c_sites   = col("c_sites", _site) // need array support in grackle
    }

    val typeMappings =
      List(
        ObjectMapping(
          tpe = QueryType,
          fieldMappings = List(
            SqlRoot("partnerMeta"),
          )
        ),
        ObjectMapping(
          tpe = PartnerMetaType,
          fieldMappings = List(
            SqlField("tag", Partner.Tag, key = true),
            SqlField("shortName", Partner.ShortName),
            SqlField("longName", Partner.LongName),
            SqlField("active", Partner.Active),
          )
        ),
        LeafMapping[Tag](PartnerType)
      )

    val elaborator = Map[TypeRef, PartialFunction[Select, Result[Query]]](
      QueryType -> {
        case Select("partnerMeta", Nil, child) =>
          Result(Select("partnerMeta", Nil,
            OrderBy(OrderSelections(List(OrderSelection(UniquePath[Tag](List("tag"))))), child)
          ))
      }
    )

    Snippet(typeMappings, elaborator)

  }

  def enumType[F[_]: Functor](s: Session[F]): F[EnumType] =
    s.execute(sql"select c_tag, c_long_name, c_active from t_partner order by c_tag".query(varchar ~ varchar ~ bool)).map { elems =>
      EnumType(
        "Partner",
        Some("Enumerated type of partners."),
        elems.map { case tag ~ desc ~ active => EnumValue(tag.toUpperCase(), Some(desc), !active) }
      )
    }

  }