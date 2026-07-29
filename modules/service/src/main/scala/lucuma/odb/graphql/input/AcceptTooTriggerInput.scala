// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.odb.data.TooTrigger
import lucuma.odb.graphql.binding.*

case class AcceptTooTriggerInput(tooTriggerId: TooTrigger.Id)

object AcceptTooTriggerInput:

  private val TooTriggerIdBinding = gidBinding[TooTrigger.Id]("too trigger")

  val Binding: Matcher[AcceptTooTriggerInput] =
    ObjectFieldsBinding.rmap:
      case List(
        TooTriggerIdBinding("tooTriggerId", rId)
      ) =>
        rId.map(apply)
