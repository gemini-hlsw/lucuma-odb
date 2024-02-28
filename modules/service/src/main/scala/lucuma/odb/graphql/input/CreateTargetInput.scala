// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import lucuma.core.model.Program
import lucuma.odb.data.ProgramReference
import lucuma.odb.graphql.binding._

case class CreateTargetInput(
  programId:        Option[Program.Id],
  programReference: Option[ProgramReference],
  SET:              TargetPropertiesInput.Create,
)

object CreateTargetInput {

  val Binding: Matcher[CreateTargetInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding.Option("programId", rPid),
        ProgramReferenceBinding.Option("programReference", rRef),
        TargetPropertiesInput.Binding("SET", rSET),
      ) =>
        (rPid, rRef, rSET).parMapN(apply)
    }

}