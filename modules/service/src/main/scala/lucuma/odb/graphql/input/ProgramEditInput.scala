// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import lucuma.core.model.Program
import lucuma.odb.graphql.util.Bindings._

case class ProgramEditInput(programId: Option[Program.Id])

object ProgramEditInput {

  val Binding = ObjectFieldsBinding.rmap {
    case List(
      ProgramIdBinding.Option("programId", rProgramId)
    ) =>
      rProgramId.map(apply)
  }

}