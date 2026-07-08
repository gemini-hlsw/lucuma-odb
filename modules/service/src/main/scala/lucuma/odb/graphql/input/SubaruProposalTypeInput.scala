// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.option.*
import cats.syntax.parallel.*
import lucuma.core.enums.Partner
import lucuma.core.enums.SubaruCallForProposalsType
import lucuma.core.model.IntPercent
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

object SubaruProposalTypeInput:

  private val HundredPercent = IntPercent.unsafeFrom(100)

  case class Create(
    callType:       SubaruCallForProposalsType = SubaruCallForProposalsType.Normal,
    minPercentTime: IntPercent                 = HundredPercent,
    partnerSplits:  Map[Partner, IntPercent]   = Map.empty
  ):
    def asEdit: Edit =
      Edit(callType.some, minPercentTime.some, Nullable.NonNull(partnerSplits))

  object Create:
    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          SubaruCallForProposalsTypeBinding.Option("type", rType),
          IntPercentBinding.Option("minPercentTime", rMin),
          PartnerSplitInput.BindingAll.Option("partnerSplits", rSplits)
        ) => (rType, rMin, rSplits).parMapN: (t, min, splits) =>
          Create(t.getOrElse(SubaruCallForProposalsType.Normal), min.getOrElse(HundredPercent), splits.getOrElse(Map.empty))

  case class Edit(
    callType:       Option[SubaruCallForProposalsType] = none,
    minPercentTime: Option[IntPercent]                 = none,
    partnerSplits:  Nullable[Map[Partner, IntPercent]] = Nullable.Null
  ):
    def asCreate: Create =
      Create(
        callType.getOrElse(SubaruCallForProposalsType.Normal),
        minPercentTime.getOrElse(HundredPercent),
        partnerSplits.toOption.getOrElse(Map.empty)
      )

  object Edit:
    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          SubaruCallForProposalsTypeBinding.Option("type", rType),
          IntPercentBinding.Option("minPercentTime", rMin),
          PartnerSplitInput.BindingAll.Nullable("partnerSplits", rSplits)
        ) => (rType, rMin, rSplits).parMapN(Edit.apply)