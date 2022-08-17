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

