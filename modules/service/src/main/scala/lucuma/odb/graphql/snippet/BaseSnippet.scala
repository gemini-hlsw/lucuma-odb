package lucuma.odb.graphql
package snippet

import edu.gemini.grackle.syntax._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.refined._
import lucuma.odb.graphql.util._
import lucuma.odb.data.Existence
import java.time.Duration

/** Snippet with base types, mostly scalars. */
object BaseSnippet {

  def apply[F[_]](m: SnippetMapping[F]): m.Snippet = {
    import m.{ Snippet, LeafMapping }

    val schema =
      schema"""
        scalar NonEmptyString
        scalar Long
        scalar BigDecimal
        scalar HmsString
        scalar DmsString

        "ISO-8601 duration, as String"
        scalar Duration

        enum Existence {
          PRESENT
          DELETED
        }

        scalar Cursor

        type PageInfo {
          hasPreviousPage: Boolean!
          hasNextPage: Boolean!
          startCursor: Cursor!
          endCursor: Cursor!
        }

      """

    val NonEmptyStringType = schema.ref("NonEmptyString")
    val ExistenceType = schema.ref("Existence")
    val DurationType = schema.ref("Duration")
    val BigDecimalType = schema.ref("BigDecimal")

    val typeMappings =
      List(
        LeafMapping[NonEmptyString](NonEmptyStringType),
        LeafMapping[Existence](ExistenceType),
        LeafMapping[Duration](DurationType),
        LeafMapping[BigDecimal](BigDecimalType),
      )

      Snippet(schema, typeMappings)

    }

}