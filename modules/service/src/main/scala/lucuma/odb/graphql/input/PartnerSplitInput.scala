// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.Partner
import lucuma.core.model.IntPercent
import lucuma.core.model.ProgramUser
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

case class PartnerSplitInput(
  partner: Partner,
  percent: IntPercent,
  leadId:  Nullable[ProgramUser.Id]
)

/**
 * A complete set of partner splits, with any explicit partner lead choices that came with them.
 * `leads` holds an entry only for a split whose `leadId` was given: `Some(id)` to choose `id`, and
 * `None` to discard the current lead in favor of the default.
 */
case class PartnerSplits(
  percents: Map[Partner, IntPercent],
  leads:    Map[Partner, Option[ProgramUser.Id]]
):
  def isEmpty: Boolean  = percents.isEmpty
  def nonEmpty: Boolean = percents.nonEmpty

object PartnerSplits:
  val Empty: PartnerSplits = PartnerSplits(Map.empty, Map.empty)

object PartnerSplitInput:

  val Binding: Matcher[PartnerSplitInput] =
    ObjectFieldsBinding.rmap:
      case List(
        PartnerBinding("partner", rPartner),
        IntPercentBinding("percent", rPercent),
        ProgramUserIdBinding.Nullable("leadId", rLead)
      ) =>
        (rPartner, rPercent, rLead).parMapN(apply)

  val BindingAll: Matcher[PartnerSplits] =
    Binding.List.Option.rmap: splits =>
      val all = splits.getOrElse(List.empty)
      val map = all.map(a => (a.partner -> a.percent)).toMap

      // An absent leadId makes no choice; null asks for the default.
      val leads: Map[Partner, Option[ProgramUser.Id]] =
        all.flatMap: a =>
          a.leadId.fold((a.partner -> none).some, none, id => (a.partner -> id.some).some)
        .toMap

      (
        Result
          .failure("Each partner may only appear once.")
          .unlessA(splits.forall(_.length === map.size)),
        Result
          .failure("Percentages must sum to exactly 100.")
          .unlessA(splits.forall(_.foldMap(_.percent.value) === 100))
      ).parTupled.as(PartnerSplits(map, leads))
