package lucuma.odb.graphql
package snippet

import edu.gemini.grackle.syntax._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.refined._
import lucuma.odb.graphql.util._
import lucuma.odb.data.Existence

/** Snippet with base types, mostly scalars. */
object BaseSnippet {

  def apply[F[_]](m: SnippetMapping[F]): m.Snippet = {
    import m.{ Snippet, LeafMapping }

    val schema =
      schema"""
        scalar NonEmptyString

        enum Existence {
          PRESENT
          DELETED
        }
      """

    val NonEmptyStringType = schema.ref("NonEmptyString")
    val ExistenceType = schema.ref("Existence")

    val typeMappings =
      List(
        LeafMapping[NonEmptyString](NonEmptyStringType),
        LeafMapping[Existence](ExistenceType),
      )

      Snippet(schema, typeMappings)

    }

}