// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.OrcidId
import lucuma.core.model.Program
import lucuma.core.syntax.string.toScreamingSnakeCase
import lucuma.odb.graphql.binding.*

case class CreatePreAuthProgramUserInput(
  orcidId:   OrcidId,
  programId: Program.Id,
  role:      ProgramUserRole,
  SET:       Option[ProgramUserPropertiesInput]
)

object CreatePreAuthProgramUserInput:
  def ensuringCoi(role: ProgramUserRole): Result[ProgramUserRole] =
    role match
      case ProgramUserRole.Coi | ProgramUserRole.CoiRO =>
        role.success
      case _                                           =>
        Result.failure(s"Only co-investigators who have not accepted an invitation may be linked via this method, not ${role.tag.toScreamingSnakeCase}")

  val Binding: Matcher[CreatePreAuthProgramUserInput] =
    ObjectFieldsBinding.rmap:
      case List(
        OrcidIdBinding("orcidId", rOrcidId),
        ProgramIdBinding("programId", rProgramId),
        ProgramUserRoleBinding("role", rRole),
        ProgramUserPropertiesInput.Binding.Option("SET", rProps)
      ) => (
        rOrcidId,
        rProgramId,
        rRole.flatMap(ensuringCoi),
        rProps
      ).parMapN(CreatePreAuthProgramUserInput.apply)