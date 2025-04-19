// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import lucuma.core.enums.Partner
import lucuma.core.model.IntPercent
import lucuma.odb.graphql.binding.*

case class PartnerSplitInput(
  partner: Partner,
  percent: IntPercent,
)

object PartnerSplitInput {

  val Binding: Matcher[PartnerSplitInput] =
    ObjectFieldsBinding.rmap {
      case List(
        PartnerBinding("partner", rPartner),
        IntPercentBinding("percent", rPercent),
      ) =>
        (rPartner, rPercent).parMapN(apply)
    }

}

