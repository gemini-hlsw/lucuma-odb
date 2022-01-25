package lucuma.odb.graphql
package snippet

import cats.syntax.all._
import edu.gemini.grackle.syntax._
import edu.gemini.grackle.Query._
import skunk.codec.all._
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.QueryCompiler
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.Result
import edu.gemini.grackle.Query
import lucuma.odb.util.Codecs._

object ProgramSnippet {

  def apply[F[_]](m: SnippetMapping[F] with SkunkMapping[F]): m.Snippet = {
    import m.{ TableDef, ObjectMapping, Snippet, SqlRoot, SqlField }

    val schema =
      schema"""
        type Query {
          programs: [Program!]!
        }

        scalar ProgramId

        type Program {
          id:         ProgramId!
          name:       String!
        }
      """

    val QueryType    = schema.ref("Query")
    val ProgramType  = schema.ref("Program")

    object Program extends TableDef("t_program") {
      val Id        = col("c_program_id", varchar)
      val Existence = col("c_existence", existence)
      val Name      = col("c_name", text)
    }

    val typeMappings =
      List(
        ObjectMapping(
          tpe = QueryType,
          fieldMappings = List(
            SqlRoot("programs"),
          )
        ),
        ObjectMapping(
          tpe = ProgramType,
          fieldMappings = List(
            SqlField("id", Program.Id, key = true),
            SqlField("existence", Program.Existence, hidden = true),
            SqlField("name", Program.Name),
          ),
        )
      )

    val elaborator = Map[TypeRef, PartialFunction[Select, Result[Query]]](
      QueryType -> {
        case Select("programs", Nil, child) =>
          Select("programs", Nil, Unique(Filter(Eql(UniquePath(List("existence")), Const(true)), child))).rightIor
      }
    )

    Snippet(schema, typeMappings, elaborator)

  }

}
