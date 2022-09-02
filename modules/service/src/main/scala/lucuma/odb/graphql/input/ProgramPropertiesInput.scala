// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.odb.data.Existence
import lucuma.odb.graphql.binding._

object ProgramPropertiesInput {

  case class Create(
    name: Option[NonEmptyString],
    proposal: Option[ProposalInput.Create],
    existence: Option[Existence]
  )

  case class Edit(
    name: Option[NonEmptyString],
    proposal: Option[ProposalInput.Edit],
    existence: Option[Existence]
  )

  private def data[A](
    proposal: Matcher[A]
  ): Matcher[(
    Option[NonEmptyString],
    Option[A],
    Option[Existence]
  )] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Option("name", rName),
        proposal.Option("proposal", rProposal),
        ExistenceBinding.Option("existence", rExistence),
      ) =>
        (rName, rProposal, rExistence).parTupled
    }

  val CreateBinding: Matcher[ProgramPropertiesInput.Create] =
    data(ProposalInput.CreateBinding).map(Create.apply)

  val EditBinding: Matcher[Edit] =
    data(ProposalInput.EditBinding).map(Edit.apply)

}