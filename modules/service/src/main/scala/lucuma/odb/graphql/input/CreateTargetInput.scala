// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import lucuma.core.model.Program
import lucuma.odb.graphql.util.Bindings._

case class CreateTargetInput(
  programId: Program.Id,
  SET: TargetPropertiesInput,
)

object CreateTargetInput {

  val Binding: Matcher[CreateTargetInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding("programId", rProgramId),
        TargetPropertiesInput.Binding("SET", rSET),
      ) =>
        (rProgramId, rSET).parMapN(CreateTargetInput(_, _))
    }

}