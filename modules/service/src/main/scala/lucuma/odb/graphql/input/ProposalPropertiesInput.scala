// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.syntax.*
import lucuma.core.enums.Observatory
import lucuma.core.enums.Partner
import lucuma.core.enums.ScienceSubtype
import lucuma.core.model.CallForProposals
import lucuma.core.model.IntPercent
import lucuma.odb.data.Nullable
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding.*

object ProposalPropertiesInput {

  // A proposal is one of three mutually-exclusive kinds: a Gemini proposal (with
  // a science subtype), a Keck exchange proposal, or a Subaru exchange proposal.

  private val onlyOneMessage: String =
    "Specify only one of 'gemini', 'keck' or 'subaru'."

  case class Create(
    category:  Option[Tag],
    callId:    Option[CallForProposals.Id],
    gemini:    Option[GeminiProposalTypeInput.Create],
    keck:      Option[KeckProposalTypeInput.Create],
    subaru:    Option[SubaruProposalTypeInput.Create]
  ) {
    def scienceSubtype: Option[ScienceSubtype] =
      gemini.map(_.scienceSubtype)

    // Every proposal has an observatory; Gemini proposals are at Gemini.
    def observatory: Observatory =
      if keck.isDefined then Observatory.Keck
      else if subaru.isDefined then Observatory.Subaru
      else Observatory.Gemini

    def partnerSplits: Map[Partner, IntPercent] =
      gemini.map(_.partnerSplits)
        .orElse(keck.map(_.partnerSplits))
        .orElse(subaru.map(_.partnerSplits))
        .getOrElse(Map.empty)
  }

  case class Edit(
    category:  Nullable[Tag],
    callId:    Nullable[CallForProposals.Id],
    gemini:    Option[GeminiProposalTypeInput.Edit],
    keck:      Option[KeckProposalTypeInput.Edit],
    subaru:    Option[SubaruProposalTypeInput.Edit]
  ) {
    def scienceSubtype: Option[ScienceSubtype] =
      gemini.map(_.scienceSubtype)

    // The observatory implied by this edit, or none when the edit doesn't change
    // the proposal-type variant.
    def observatory: Option[Observatory] =
      if gemini.isDefined then Observatory.Gemini.some
      else if keck.isDefined then Observatory.Keck.some
      else if subaru.isDefined then Observatory.Subaru.some
      else none

    // The edited partner splits, drawn from whichever variant is present.
    def partnerSplits: Nullable[Map[Partner, IntPercent]] =
      gemini.map(_.partnerSplits)
        .orElse(keck.map(_.partnerSplits))
        .orElse(subaru.map(_.partnerSplits))
        .getOrElse(Nullable.Absent)

    // True when the edit specifies a proposal-type variant.
    def hasType: Boolean =
      gemini.isDefined || keck.isDefined || subaru.isDefined
  }

  object Edit {
    val Empty: Edit =
      Edit(Nullable.Absent, Nullable.Absent, None, None, None)
  }

  // Reject more than one of gemini/keck/subaru.
  private def atMostOne[A](gemini: Option[?], keck: Option[?], subaru: Option[?], a: => A): grackle.Result[A] =
    if List(gemini, keck, subaru).count(_.isDefined) > 1 then Matcher.validationFailure(onlyOneMessage)
    else a.success

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap {
      case List(
        TagBinding.Option("category", rCategory),
        CallForProposalsIdBinding.Option("callId", rCid),
        GeminiProposalTypeInput.Create.Binding.Option("gemini", rGemini),
        KeckProposalTypeInput.Create.Binding.Option("keck", rKeck),
        SubaruProposalTypeInput.Create.Binding.Option("subaru", rSubaru)
      ) =>
        (rCategory, rCid, rGemini, rKeck, rSubaru).parTupled.flatMap { case (category, cid, gemini, keck, subaru) =>
          atMostOne(gemini, keck, subaru, {
            // If no variant is specified, assume a regular semester queue proposal.
            val g = if gemini.isEmpty && keck.isEmpty && subaru.isEmpty then GeminiProposalTypeInput.Create.Default.some else gemini
            Create(category, cid, g, keck, subaru)
          })
        }
    }

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap {
      case List(
        TagBinding.Nullable("category", rCategory),
        CallForProposalsIdBinding.Nullable("callId", rCid),
        GeminiProposalTypeInput.Edit.Binding.Option("gemini", rGemini),
        KeckProposalTypeInput.Edit.Binding.Option("keck", rKeck),
        SubaruProposalTypeInput.Edit.Binding.Option("subaru", rSubaru)
      ) =>
        (rCategory, rCid, rGemini, rKeck, rSubaru).parTupled.flatMap { case (category, cid, gemini, keck, subaru) =>
          atMostOne(gemini, keck, subaru, Edit(category, cid, gemini, keck, subaru))
        }
    }

}
