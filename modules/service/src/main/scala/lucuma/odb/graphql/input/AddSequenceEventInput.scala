// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.SequenceCommand
import lucuma.core.model.Visit
import lucuma.core.util.IdempotencyKey
import lucuma.odb.graphql.binding.*

case class AddSequenceEventInput(
  visitId:        Visit.Id,
  command:        SequenceCommand,
  idempotencyKey: Option[IdempotencyKey]
)

object AddSequenceEventInput:

  val Binding: Matcher[AddSequenceEventInput] =
    ObjectFieldsBinding.rmap:
      case List(
        VisitIdBinding("visitId", rVisitId),
        SequenceCommandBinding("command", rCommand),
        IdempotencyKeyBinding.Option("idempotencyKey", rIdm)
      ) =>
        (rVisitId, rCommand, rIdm).parMapN: (vid, cmd, idm) =>
          AddSequenceEventInput(vid, cmd, idm)