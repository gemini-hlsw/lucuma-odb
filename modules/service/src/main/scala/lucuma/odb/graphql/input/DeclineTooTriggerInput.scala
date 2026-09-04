// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.odb.data.TooTrigger
import lucuma.odb.graphql.binding.*

case class DeclineTooTriggerInput(
  tooTriggerId: TooTrigger.Id,
  reason:       Option[NonEmptyString]
)

object DeclineTooTriggerInput:

  private val TooTriggerIdBinding = gidBinding[TooTrigger.Id]("too trigger")

  val Binding: Matcher[DeclineTooTriggerInput] =
    ObjectFieldsBinding.rmap:
      case List(
        TooTriggerIdBinding("tooTriggerId", rId),
        NonEmptyStringBinding.Option("reason", rReason)
      ) =>
        (rId, rReason).parMapN(apply)
