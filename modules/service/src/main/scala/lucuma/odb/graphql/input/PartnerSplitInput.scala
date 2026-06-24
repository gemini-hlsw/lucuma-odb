// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import grackle.syntax.*
import lucuma.core.enums.Partner
import lucuma.core.model.IntPercent
import lucuma.odb.graphql.binding.*

case class PartnerSplitInput(
  partner: Partner,
  percent: IntPercent,
)

object PartnerSplitInput:

  val Binding: Matcher[PartnerSplitInput] =
    ObjectFieldsBinding.rmap:
      case List(
        PartnerBinding("partner", rPartner),
        IntPercentBinding("percent", rPercent),
      ) =>
        (rPartner, rPercent).parMapN(apply)

  val BindingAll: Matcher[Map[Partner, IntPercent]] =
    Binding.List.Option.rmap: splits =>
      val map = splits.getOrElse(List.empty).map(a => (a.partner -> a.percent)).toMap

      Matcher
        .validationFailure("Each partner may only appear once.")
        .unlessA(splits.forall(_.length === map.size)) *>
      Matcher
        .validationFailure("Percentages must sum to exactly 100.")
        .unlessA(splits.forall(_.foldMap(_.percent.value) === 100)) *>
      map.success