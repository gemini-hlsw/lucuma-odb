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

case class ProposalInput(
  title: Option[NonEmptyString],
  proposalClass: ProposalClassInput,
  category: Option[Tag],
  toOActivation: Option[ToOActivation],
  abstrakt: Option[NonEmptyString],
  partnerSplits: Option[Map[Tag, IntPercent]],
)

object ProposalInput {

  val PartnerSplitsInput: Matcher[Map[Tag, IntPercent]] =
    PartnerSplitInput.Binding.List.rmap { splits =>
      val map = splits.map(a => (a.partner -> a.percent)).toMap
      if splits.length != map.size
      then Result.failure("Each partner may only appear once.")
      else
        if splits.foldMap(_.percent.value) != 100
        then Result.failure("Percentages must sum to exactly 100.")
        else Result(map)
    }

  val Binding: Matcher[ProposalInput] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Option("title", rTitle),
        ProposalClassInput.Binding("proposalClass", rProposalClass),
        TagBinding.Option("category", rCategory),
        ToOActivationBinding.Option("toOActivation", rToOActivation),
        NonEmptyStringBinding.Option("abstract", rAbstract),
        PartnerSplitsInput.Option("partnerSplits", rSplits),
      ) =>
        (rTitle, rProposalClass, rCategory, rToOActivation, rAbstract, rSplits).parMapN(apply)
    }

}

