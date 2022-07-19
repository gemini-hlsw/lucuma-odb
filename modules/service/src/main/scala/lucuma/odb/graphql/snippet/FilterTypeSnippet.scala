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
import lucuma.odb.graphql.util._
import lucuma.odb.util.Codecs._
import lucuma.odb.data.Tag
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.Result
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query.{ Select, OrderBy, OrderSelections, OrderSelection }
import edu.gemini.grackle.Path.UniquePath

object FilterTypeSnippet {

  def apply[F[_]](m: SnippetMapping[F] with SkunkMapping[F]): m.Snippet = {
    import m.{ TableDef, ObjectMapping, Snippet, SqlRoot, SqlField, LeafMapping }

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
      val Tag       = col("c_tag", tag)
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
        LeafMapping[Tag](FilterTypeType)
      )

    val elaborator = Map[TypeRef, PartialFunction[Select, Result[Query]]](
      QueryType -> {
        case Select("filterTypeMeta", Nil, child) =>
          Result(Select("filterTypeMeta", Nil,
            OrderBy(OrderSelections(List(OrderSelection(UniquePath[Tag](List("tag"))))), child)
          ))
      }
    )

    Snippet(schema, typeMappings, elaborator)

  }

    def enumType[F[_]: Functor](s: Session[F]): F[EnumType] =
      s.execute(sql"select c_tag, c_long_name from t_filter_type order by c_tag".query(varchar ~ varchar)).map { elems =>
        EnumType(
          "FilterType",
          Some("Enumerated type of filters."),
          elems.map { case (tag, desc) => EnumValue(tag, Some(desc)) }
        )
      }

}