// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.util.Timestamp
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.TagBinding
import lucuma.odb.graphql.binding.TimestampBinding

case class CallForProposalsPartnerInput(
  partner:  Tag,
  deadline: Option[Timestamp]
)

object CallForProposalsPartnerInput {

  val Binding = ObjectFieldsBinding.rmap {
    case List(
      TagBinding("partner", rPartner),
      TimestampBinding.Option("submissionDeadlineOverride", rDeadline)
    ) =>
      (rPartner, rDeadline).parMapN(apply)
  }

}