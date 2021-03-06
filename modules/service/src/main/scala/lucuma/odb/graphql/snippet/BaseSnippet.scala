// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet

import eu.timepit.refined.types.string.NonEmptyString
import io.circe.refined._
import lucuma.odb.data.Existence
import lucuma.odb.graphql.util._

/** Snippet with base types, mostly scalars. */
object BaseSnippet {

  def apply[F[_]](m: SnippetMapping[F]): m.Snippet = {
    import m.{ Snippet, LeafMapping, schema }

    val NonEmptyStringType = schema.ref("NonEmptyString")
    val ExistenceType = schema.ref("Existence")
    val BigDecimalType = schema.ref("BigDecimal")

    val typeMappings =
      List(
        LeafMapping[NonEmptyString](NonEmptyStringType),
        LeafMapping[Existence](ExistenceType),
        LeafMapping[BigDecimal](BigDecimalType),
      )

      Snippet(typeMappings)

    }

}