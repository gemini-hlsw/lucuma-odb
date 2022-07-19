package lucuma.odb.data

import io.circe._
import lucuma.core.syntax.string._
import cats.kernel.Order

/**
 * A tag, for generic treatment of enumerated types. We assume they are stored in lowercase in the
 * database but are upper-cased for GraphQL.
 */
final case class Tag(value: String)
object Tag {

  implicit val TagCirceCodec: Encoder[Tag] with Decoder[Tag] =
    new Encoder[Tag] with Decoder[Tag] {
      def apply(c: HCursor) = c.as[String].map(s => Tag(s.toLowerCase))
      def apply(a: Tag) = Json.fromString(a.value.toScreamingSnakeCase)
    }

  implicit val OrderTag: Order[Tag] =
    Order.by(_.value)

}