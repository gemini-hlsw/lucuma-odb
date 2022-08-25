// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.odb.data.Existence
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.util.Bindings._

case class ProgramPropertiesInput(
  name: Option[NonEmptyString],
  proposal: Option[ProposalInput],
  existence: Option[Existence]
)

object ProgramPropertiesInput {

  val Default: ProgramPropertiesInput =
    ProgramPropertiesInput(None, None, None)

  private def binding(
    proposal: Matcher[ProposalInput]
  ): Matcher[ProgramPropertiesInput] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Option("name", rName),
        ProposalInput.CreateBinding.Option("proposal", rProposal),
        ExistenceBinding.Option("existence", rExistence),
      ) =>
        (rName, rProposal, rExistence).parMapN(apply)
    }

  val CreateBinding: Matcher[ProgramPropertiesInput] =
    binding(ProposalInput.CreateBinding)

  val EditBinding: Matcher[ProgramPropertiesInput] =
    binding(ProposalInput.EditBinding)

}