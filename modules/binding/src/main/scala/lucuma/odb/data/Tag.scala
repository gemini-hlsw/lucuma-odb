// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.kernel.Order
import io.circe.*
import lucuma.core.syntax.string.*

/**
 * A tag, for generic treatment of enumerated types. We assume they are stored in lowercase in the
 * database but are upper-cased for GraphQL.
 */
final case class Tag(value: String)
object Tag {

  implicit val TagCirceCodec: Encoder[Tag] & Decoder[Tag] =
    new Encoder[Tag] with Decoder[Tag] {
      def apply(c: HCursor) = c.as[String].map(s => Tag(s.toLowerCase))
      def apply(a: Tag) = Json.fromString(a.value.toScreamingSnakeCase)
    }

  implicit val OrderTag: Order[Tag] =
    Order.by(_.value)

}