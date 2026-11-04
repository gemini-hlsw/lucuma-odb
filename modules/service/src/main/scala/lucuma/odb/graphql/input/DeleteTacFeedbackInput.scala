// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import lucuma.core.enums.Partner
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProposalReference
import lucuma.odb.graphql.binding.*

case class DeleteTacFeedbackInput(
  programId:         Option[Program.Id],
  proposalReference: Option[ProposalReference],
  programReference:  Option[ProgramReference],
  partner:           Partner
)

object DeleteTacFeedbackInput:

  val Binding: Matcher[DeleteTacFeedbackInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ProgramIdBinding.Option("programId", rPid),
        ProposalReferenceBinding.Option("proposalReference", rProp),
        ProgramReferenceBinding.Option("programReference", rProg),
        PartnerBinding("partner", rPartner)
      ) =>
        (rPid, rProp, rProg, rPartner).parMapN(apply)
