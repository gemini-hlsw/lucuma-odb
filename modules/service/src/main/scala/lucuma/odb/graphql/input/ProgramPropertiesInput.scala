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

  val Binding: Matcher[ProgramPropertiesInput] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Option("name", rName),
        ProposalInput.Binding.Option("proposal", rProposal),
        ExistenceBinding.Option("existence", rExistence),
      ) =>
        (rName, rProposal, rExistence).parMapN { (on, op, oe) =>
          ProgramPropertiesInput(on, op, oe)
       }
    }

}