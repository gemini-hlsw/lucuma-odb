// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.data.NonEmptyList
import cats.syntax.all.*
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.odb.graphql.binding.*

final case class CloneTargetInput(
  targetId:   Target.Id,
  SET:        Option[TargetPropertiesInput.Edit],
  REPLACE_IN: Option[NonEmptyList[Observation.Id]],
)

object CloneTargetInput {

 val Binding: Matcher[CloneTargetInput] =
    ObjectFieldsBinding.rmap {
      case List(
        TargetIdBinding("targetId", rTargetId),
        TargetPropertiesInput.EditBinding.Option("SET", rSET),
        ObservationIdBinding.List.Option("REPLACE_IN", rREPLACE_IN)
      ) =>
        (rTargetId, rSET, rREPLACE_IN.map(_.flatMap(_.toNel))).mapN(CloneTargetInput.apply)
    }

}
