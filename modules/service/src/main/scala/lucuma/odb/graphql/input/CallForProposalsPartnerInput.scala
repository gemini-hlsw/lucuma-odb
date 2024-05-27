// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.model.Partner
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.PartnerBinding
import lucuma.odb.graphql.binding.TimestampBinding

case class CallForProposalsPartnerInput(
  partner:  Partner,
  deadline: Option[Timestamp]
)

object CallForProposalsPartnerInput {

  val Binding = ObjectFieldsBinding.rmap {
    case List(
      PartnerBinding("partner", rPartner),
      TimestampBinding.Option("submissionDeadlineOverride", rDeadline)
    ) =>
      (rPartner, rDeadline).parMapN(apply)
  }

}