package lucuma.odb.graphql
package snippet

import edu.gemini.grackle.syntax._
import skunk.codec.all._
import edu.gemini.grackle.skunk.SkunkMapping
import cats.Functor
import skunk._
import edu.gemini.grackle.EnumType
import skunk.syntax.all._
import cats.syntax.all._
import edu.gemini.grackle.EnumValue
import lucuma.odb.graphql.util._

object PartnerSnippet {

  def apply[F[_]](m: SnippetMapping[F] with SkunkMapping[F]): m.Snippet = {
    import m.{ TableDef, ObjectMapping, Snippet, SqlRoot, SqlField, PrimitiveMapping }

    val schema =
      schema"""
        type Query {
          partnerMeta: [PartnerMeta!]!
        }
        type PartnerMeta {
          tag:        Partner!
          shortName:  String!
          longName:	  String!
          active:     Boolean!
          #sites:     [Site!]!
        }
      """

    val QueryType        = schema.ref("Query")
    val PartnerType      = schema.ref("Partner")
    val PartnerMetaType  = schema.ref("PartnerMeta")

    object Partner extends TableDef("t_partner") {
      val Tag       = col("c_tag", varchar)
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
        PrimitiveMapping(PartnerType)
      )

    Snippet(schema, typeMappings)

  }

  def enumType[F[_]: Functor](s: Session[F]): F[EnumType] =
    s.execute(sql"select c_tag, c_long_name, c_active from t_partner".query(varchar ~ varchar ~ bool)).map { elems =>
      EnumType(
        "Partner",
        Some("Enumerated type of partners."),
        elems.map { case tag ~ desc ~ active => EnumValue(tag, Some(desc), !active) }
      )
    }

  }