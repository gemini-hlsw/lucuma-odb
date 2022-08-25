// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.odb.data.Existence
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.util.Bindings._
import lucuma.odb.data.Tag
import lucuma.core.enums.ToOActivation
import lucuma.core.model.IntPercent
import edu.gemini.grackle.Result
import lucuma.odb.data.Nullable

case class ProposalInput(
  title: Nullable[NonEmptyString],
  proposalClass: Option[ProposalClassInput],
  category: Nullable[Tag],
  toOActivation: Option[ToOActivation],
  abstrakt: Nullable[NonEmptyString],
  partnerSplits: Option[Map[Tag, IntPercent]],
)

object ProposalInput {

  private val PartnerSplitsInput: Matcher[Map[Tag, IntPercent]] =
    PartnerSplitInput.Binding.List.rmap { splits =>
      val map = splits.map(a => (a.partner -> a.percent)).toMap
      if splits.length != map.size
      then Result.failure("Each partner may only appear once.")
      else
        if splits.foldMap(_.percent.value) != 100
        then Result.failure("Percentages must sum to exactly 100.")
        else Result(map)
    }

  private def binding(
    proposalClass: Matcher[ProposalClassInput]
  ): Matcher[ProposalInput] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Nullable("title", rTitle),
        proposalClass.Option("proposalClass", rProposalClass),
        TagBinding.Nullable("category", rCategory),
        ToOActivationBinding.Option("toOActivation", rToOActivation),
        NonEmptyStringBinding.Nullable("abstract", rAbstract),
        PartnerSplitsInput.Option("partnerSplits", rSplits),
      ) =>
        (rTitle, rProposalClass, rCategory, rToOActivation, rAbstract, rSplits).parMapN(apply)
    }

  val CreateBinding: Matcher[ProposalInput] =
    binding(ProposalClassInput.CreateBinding).rmap {
      case ok @ ProposalInput(_, Some(_), _, Some(_), _, Some(_)) => Result(ok)
      case _ => Result.failure("All of proposalClass, toOActivation, and partnerSplits are required on creation.")
    }

  val EditBinding: Matcher[ProposalInput] =
    binding(ProposalClassInput.EditBinding)

}

