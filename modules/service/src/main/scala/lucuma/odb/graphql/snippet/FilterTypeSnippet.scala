// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet

import cats.Functor
import cats.syntax.all._
import edu.gemini.grackle.EnumType
import edu.gemini.grackle.EnumValue
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.syntax._
import lucuma.odb.data.Tag
import lucuma.odb.graphql.util._
import lucuma.odb.util.Codecs._
import skunk.Session
import skunk.codec.all._
import skunk.syntax.all._

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