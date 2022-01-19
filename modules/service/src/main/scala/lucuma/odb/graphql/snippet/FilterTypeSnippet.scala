package lucuma.odb.graphql
package snippet

import edu.gemini.grackle.syntax._
import skunk.codec.all._
import edu.gemini.grackle.skunk.SkunkMapping

object FilterTypeSnippet {

  def apply[F[_]](m: SnippetMapping[F] with SkunkMapping[F]): m.Snippet = {
    import m.{ TableDef, ObjectMapping, Snippet, SqlRoot, SqlField }

    val schema =
      schema"""
        type Query {
          filterTypeMeta: [FilterType!]!
        }

        type FilterType {
          tag:        String!
          shortName:  String!
          longName:	  String!
        }
      """

    val QueryType        = schema.ref("Query")
    val PartnerMetaType  = schema.ref("FilterType")

    object FilterType extends TableDef("t_filter_type") {
      val Tag       = col("c_tag", varchar)
      val ShortName = col("c_short_name", varchar)
      val LongName  = col("c_long_name", varchar)
    }

    val typeMappings =
      List(
        ObjectMapping(
          tpe = QueryType,
          fieldMappings = List(
            SqlRoot("filterTypeMeta"),
          )
        ),
        ObjectMapping(
          tpe = PartnerMetaType,
          fieldMappings = List(
            SqlField("tag", FilterType.Tag, key = true),
            SqlField("shortName", FilterType.ShortName),
            SqlField("longName", FilterType.LongName),
          ),
        )
      )

      Snippet(schema, typeMappings)

    }

}