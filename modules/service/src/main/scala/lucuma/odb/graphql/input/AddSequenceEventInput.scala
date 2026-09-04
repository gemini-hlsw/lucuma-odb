// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.SequenceCommand
import lucuma.core.model.Visit
import lucuma.core.util.IdempotencyKey
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.binding.*

case class AddSequenceEventInput(
  visitId:        Visit.Id,
  command:        SequenceCommand,
  clientTime:     Option[Timestamp],
  idempotencyKey: Option[IdempotencyKey]
)

object AddSequenceEventInput:

  val Binding: Matcher[AddSequenceEventInput] =
    ObjectFieldsBinding.rmap:
      case List(
        VisitIdBinding("visitId", rVisitId),
        SequenceCommandBinding("command", rCommand),
        TimestampBinding.Option("clientTime", rClientTime),
        IdempotencyKeyBinding.Option("idempotencyKey", rIdm)
      ) =>
        (rVisitId, rCommand, rClientTime, rIdm).parMapN: (vid, cmd, clientTime, idm) =>
          AddSequenceEventInput(vid, cmd, clientTime, idm)