package lucuma.odb.graphql
package snippet

import edu.gemini.grackle.syntax._
import skunk.codec.all._
import edu.gemini.grackle.skunk.SkunkMapping

object PartnerSnippet {

  def apply[F[_]](m: SnippetMapping[F] with SkunkMapping[F]): m.Snippet = {
    import m.{ TableDef, ObjectMapping, Snippet, SqlRoot, SqlField }

    val schema =
      schema"""
        type Query {
          partnerMeta: [PartnerMeta!]!
        }

        type PartnerMeta {
          tag:        String!
          shortName:  String!
          longName:	  String!
          active:     Boolean!
          #sites:     [Site!]!
        }
      """

    val QueryType        = schema.ref("Query")
    val PartnerMetaType  = schema.ref("PartnerMeta")

    object Partner extends TableDef("t_partner") {
      val Tag       = col("c_tag", varchar)
      val ShortName = col("c_short_name", varchar)
      val LongName  = col("c_long_name", varchar)
      val Active    = col("c_active", bool)
      // val c_sites = col("c_sites", varchar)
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
          ),
        )
      )

      Snippet(schema, typeMappings)

    }

}