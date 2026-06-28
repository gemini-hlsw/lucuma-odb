// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.core.enums.Partner
import lucuma.core.model.IntPercent
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

object KeckProposalTypeInput:

  case class Create(
    partnerSplits: Map[Partner, IntPercent] = Map.empty
  ):
    def asEdit: Edit =
      Edit(Nullable.NonNull(partnerSplits))

  object Create:
    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          PartnerSplitInput.BindingAll.Option("partnerSplits", rSplits)
        ) => rSplits.map(s => Create(s.getOrElse(Map.empty)))

  case class Edit(
    partnerSplits: Nullable[Map[Partner, IntPercent]] = Nullable.Null
  ):
    def asCreate: Create =
      Create(partnerSplits.toOption.getOrElse(Map.empty))

  object Edit:
    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          PartnerSplitInput.BindingAll.Nullable("partnerSplits", rSplits)
        ) => rSplits.map(Edit.apply)
