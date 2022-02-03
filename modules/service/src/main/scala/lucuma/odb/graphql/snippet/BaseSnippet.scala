package lucuma.odb.graphql
package snippet

import edu.gemini.grackle.syntax._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.refined._
import lucuma.odb.graphql.util._

/** Snippet with base types, mostly scalars. */
object BaseSnippet {

  def apply[F[_]](m: SnippetMapping[F]): m.Snippet = {
    import m.{ Snippet, LeafMapping }

    val schema =
      schema"""
        scalar NonEmptyString
      """

    val NonEmptyStringType = schema.ref("NonEmptyString")

    val typeMappings =
      List(
        LeafMapping[NonEmptyString](NonEmptyStringType)
      )

      Snippet(schema, typeMappings)

    }

}