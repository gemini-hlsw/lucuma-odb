// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.odb.data.Existence
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding._

object ProgramPropertiesInput {

  case class Create(
    name: Option[NonEmptyString],
    proposal: Option[ProposalInput.Create]
  )

  case class Edit(
    name: Option[NonEmptyString],
    proposalStatus: Option[Tag],
    proposal: Option[ProposalInput.Edit],
    existence: Option[Existence]
  )

  private def data[A](
    proposal: Matcher[A]
  ): Matcher[(
    Option[NonEmptyString],
    Option[Tag],
    Option[A],
    Option[Existence]
  )] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Option("name", rName),
        TagBinding.Option("proposalStatus", rPs),
        proposal.Option("proposal", rProposal),
        ExistenceBinding.Option("existence", rExistence),
      ) =>
        (rName, rPs, rProposal, rExistence).parTupled
    }

  val CreateBinding: Matcher[ProgramPropertiesInput.Create] =
    data(ProposalInput.CreateBinding).rmap {
      case (n, None, p, _) => Result(Create(n, p))
      case _               => Matcher.validationFailure("proposalStatus cannot be specified during program creation.")
    }

  val EditBinding: Matcher[Edit] =
    data(ProposalInput.EditBinding).map(Edit.apply)

}
