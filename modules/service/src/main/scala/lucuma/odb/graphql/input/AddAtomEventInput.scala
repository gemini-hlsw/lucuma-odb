// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.model.sequence.Atom
import lucuma.odb.data.AtomStage
import lucuma.odb.graphql.binding.*

case class AddAtomEventInput(
  atomId:    Atom.Id,
  atomStage: AtomStage
)

object AddAtomEventInput {

  val Binding: Matcher[AddAtomEventInput] =
    ObjectFieldsBinding.rmap {
      case List(
        AtomIdBinding("atomId", rAtomId),
        AtomStageBinding("atomStage", rAtomStage)
      ) =>
        (rAtomId, rAtomStage).parMapN { (aid, stage) =>
          AddAtomEventInput(aid, stage)
        }
    }

}