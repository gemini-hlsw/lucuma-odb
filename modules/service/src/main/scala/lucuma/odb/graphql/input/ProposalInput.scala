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
import lucuma.core.enums.ProposalClass

case class ProposalInput(
  title: Nullable[NonEmptyString],
  proposalClass: Option[ProposalClassInput],
  category: Nullable[Tag],
  toOActivation: Option[ToOActivation],
  abstrakt: Nullable[NonEmptyString],
  partnerSplits: Option[Map[Tag, IntPercent]],
)


object ProposalInput {

  case class Create(
    title: Nullable[NonEmptyString],
    proposalClass: ProposalClassInput,
    category: Nullable[Tag],
    toOActivation: ToOActivation,
    abstrakt: Nullable[NonEmptyString],
    partnerSplits: Map[Tag, IntPercent],
  )

  case class Edit(
    title: Nullable[NonEmptyString],
    proposalClass: Option[ProposalClassInput],
    category: Nullable[Tag],
    toOActivation: Option[ToOActivation],
    abstrakt: Nullable[NonEmptyString],
    partnerSplits: Option[Map[Tag, IntPercent]],
  )

  private def data[A](
    proposalClass: Matcher[A]
  ): Matcher[(
      Nullable[NonEmptyString],
      Option[A],
      Nullable[Tag],
      Option[ToOActivation],
      Nullable[NonEmptyString],
      Option[Map[Tag, IntPercent]],
  )] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Nullable("title", rTitle),
        proposalClass.Option("proposalClass", rProposalClass),
        TagBinding.Nullable("category", rCategory),
        ToOActivationBinding.Option("toOActivation", rToOActivation),
        NonEmptyStringBinding.Nullable("abstract", rAbstract),
        PartnerSplitsInput.Option("partnerSplits", rSplits),
      ) =>
        (rTitle, rProposalClass, rCategory, rToOActivation, rAbstract, rSplits).parTupled
    }

  val CreateBinding: Matcher[Create] =
    data(ProposalClassInput.CreateBinding).rmap {
      case (title, Some(propClass), cat, Some(too), abs, Some(splits)) => Result(Create(title, propClass, cat, too, abs, splits))
      case _ => Result.failure("All of proposalClass, toOActivation, and partnerSplits are required on creation.")
    }

  val EditBinding: Matcher[Edit] =
    data(ProposalClassInput.CreateBinding).map(Edit.apply)

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

  // private def binding(
  //   proposalClass: Matcher[ProposalClassInput]
  // ): Matcher[ProposalInput] =
  //   ObjectFieldsBinding.rmap {
  //     case List(
  //       NonEmptyStringBinding.Nullable("title", rTitle),
  //       proposalClass.Option("proposalClass", rProposalClass),
  //       TagBinding.Nullable("category", rCategory),
  //       ToOActivationBinding.Option("toOActivation", rToOActivation),
  //       NonEmptyStringBinding.Nullable("abstract", rAbstract),
  //       PartnerSplitsInput.Option("partnerSplits", rSplits),
  //     ) =>
  //       (rTitle, rProposalClass, rCategory, rToOActivation, rAbstract, rSplits).parMapN(apply)
  //   }

  // val CreateBinding: Matcher[ProposalInput] =
  //   binding(ProposalClassInput.CreateBinding).rmap {
  //     case ok @ ProposalInput(_, Some(_), _, Some(_), _, Some(_)) => Result(ok)
  //     case _ => Result.failure("All of proposalClass, toOActivation, and partnerSplits are required on creation.")
  //   }

  // val EditBinding: Matcher[ProposalInput] =
  //   binding(ProposalClassInput.EditBinding)

}

