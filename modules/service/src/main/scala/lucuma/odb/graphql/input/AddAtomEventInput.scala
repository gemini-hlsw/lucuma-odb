// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.enums.AtomStage
import lucuma.core.model.sequence.Atom
import lucuma.core.util.IdempotencyKey
import lucuma.odb.graphql.binding.*

case class AddAtomEventInput(
  atomId:         Atom.Id,
  atomStage:      AtomStage,
  idempotencyKey: Option[IdempotencyKey]
)

object AddAtomEventInput:

  val Binding: Matcher[AddAtomEventInput] =
    ObjectFieldsBinding.rmap:
      case List(
        AtomIdBinding("atomId", rAtomId),
        AtomStageBinding("atomStage", rAtomStage),
        ClientIdBinding.Option("clientId", rCid),
        IdempotencyKeyBinding.Option("idempotencyKey", rIdm)
      ) =>
        (rAtomId, rAtomStage, rCid, rIdm).parMapN: (aid, stage, cid, idm) =>
          AddAtomEventInput(aid, stage, idm orElse cid.map(c => IdempotencyKey(c.toUuid)))