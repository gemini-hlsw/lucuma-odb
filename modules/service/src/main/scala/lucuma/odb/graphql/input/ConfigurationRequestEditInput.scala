// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import lucuma.core.model.Program
import lucuma.odb.graphql.binding.*

case class ConfigurationRequestEditInput(programId: Option[Program.Id])

object ConfigurationRequestEditInput {

  val Binding = ObjectFieldsBinding.rmap {
    case List(
      ProgramIdBinding.Option("programId", rProgramId)
    ) =>
      rProgramId.map(apply)
  }

}