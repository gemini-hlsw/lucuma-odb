// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Program
import lucuma.odb.graphql.binding.*

case class AddProgramUserInput(
  programId: Program.Id,
  role:      ProgramUserRole,
  SET:       Option[ProgramUserPropertiesInput]
)

object AddProgramUserInput:
  def ensuringNotPi(role: ProgramUserRole): Result[ProgramUserRole] =
    role match
      case ProgramUserRole.Pi => Result.failure(s"PIs are added at program creation time.")
      case _                  => role.success

  val Binding: Matcher[AddProgramUserInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ProgramIdBinding("programId", rProgramId),
        ProgramUserRoleBinding("role", rRole),
        ProgramUserPropertiesInput.Binding.Option("SET", rProps)
      ) => (
        rProgramId,
        rRole.flatMap(ensuringNotPi),
        rProps
      ).parMapN(AddProgramUserInput.apply)