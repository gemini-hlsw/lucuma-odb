// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import lucuma.core.model.IntPercent
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding.*

case class PartnerSplitInput(
  partner: Tag,
  percent: IntPercent,
)

object PartnerSplitInput {

  val Binding: Matcher[PartnerSplitInput] =
    ObjectFieldsBinding.rmap {
      case List(
        TagBinding("partner", rPartner),
        IntPercentBinding("percent", rPercent),
      ) =>
        (rPartner, rPercent).parMapN(apply)
    }

}

