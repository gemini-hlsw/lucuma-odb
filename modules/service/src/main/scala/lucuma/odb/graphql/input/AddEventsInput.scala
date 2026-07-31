// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.NonEmptyList
import cats.syntax.all.*
import grackle.Result
import lucuma.odb.graphql.binding.*

case class AddEventsInput(events: NonEmptyList[AddEventInput])

object AddEventsInput:

  val Binding: Matcher[AddEventsInput] =
    ObjectFieldsBinding.rmap:
      case List(
        AddEventInput.Binding.List("events", rEvents)
      ) =>
        rEvents.flatMap: events =>
          NonEmptyList
            .fromList(events)
            .toRight("An 'addEvents' batch must contain at least one event.")
            .flatMap: nel =>
              // Every event carries a key (enforced per element), so a duplicate
              // here would make the multi-row insert conflict with itself; reject
              // it up front with a clearer message.
              val keys = nel.toList.flatMap(_.idempotencyKey)
              Either.cond(
                keys.distinct.size === keys.size,
                AddEventsInput(nel),
                "Idempotency keys within a batch must be distinct."
              )
            .fold(Result.failure, Result.success)
