package lucuma.odb.graphql
package snippet

import edu.gemini.grackle.syntax._
import edu.gemini.grackle.skunk.SkunkMapping
import skunk.Session
import skunk.syntax.all._
import skunk.codec.all._
import edu.gemini.grackle.EnumType
import cats.Functor
import cats.syntax.all._
import edu.gemini.grackle.EnumValue

object FilterTypeSnippet {

  def apply[F[_]](m: SnippetMapping[F] with SkunkMapping[F]): m.Snippet = {
    import m.{ TableDef, ObjectMapping, Snippet, SqlRoot, SqlField, PrimitiveMapping }

    val schema =
      schema"""
        type Query {
          filterTypeMeta: [FilterTypeMeta!]!
        }

        type FilterTypeMeta {
          tag:        FilterType!
          shortName:  String!
          longName:	  String!
        }
      """

    val QueryType          = schema.ref("Query")
    val FilterTypeType     = schema.ref("FilterType")
    val FilterTypeMetaType = schema.ref("FilterTypeMeta")

    object FilterTypeMeta extends TableDef("t_filter_type") {
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
          tpe = FilterTypeMetaType,
          fieldMappings = List(
            SqlField("tag", FilterTypeMeta.Tag, key = true),
            SqlField("shortName", FilterTypeMeta.ShortName),
            SqlField("longName", FilterTypeMeta.LongName),
          )
        ),
        PrimitiveMapping(FilterTypeType)
      )

      Snippet(schema, typeMappings)

    }

    def enumType[F[_]: Functor](s: Session[F]): F[EnumType] =
      s.execute(sql"select c_tag, c_long_name from t_filter_type".query(varchar ~ varchar)).map { elems =>
        EnumType(
          "FilterType",
          Some("Enumerated type of filters."),
          elems.map { case (tag, desc) => EnumValue(tag, Some(desc)) }
        )
      }

}