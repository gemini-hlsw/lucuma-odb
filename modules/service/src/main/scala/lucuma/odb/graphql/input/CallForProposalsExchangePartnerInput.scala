// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.ExchangePartner
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.binding.ExchangePartnerBinding
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.TimestampBinding

case class CallForProposalsExchangePartnerInput(
  exchangePartner: ExchangePartner,
  deadline:        Option[Timestamp]
)

object CallForProposalsExchangePartnerInput:

  val Binding = ObjectFieldsBinding.rmap:
    case List(
      ExchangePartnerBinding("exchangePartner", rPartner),
      TimestampBinding.Option("submissionDeadlineOverride", rDeadline)
    ) =>
      (rPartner, rDeadline).parMapN(apply)