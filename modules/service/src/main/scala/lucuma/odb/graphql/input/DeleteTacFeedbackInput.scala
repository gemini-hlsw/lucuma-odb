// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProposalReference
import lucuma.odb.graphql.binding.*

case class DeleteTacFeedbackInput(
  programId:              Option[Program.Id],
  proposalReference:      Option[ProposalReference],
  programReference:       Option[ProgramReference],
  timeAccountingCategory: TimeAccountingCategory
)

object DeleteTacFeedbackInput:

  val Binding: Matcher[DeleteTacFeedbackInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ProgramIdBinding.Option("programId", rPid),
        ProposalReferenceBinding.Option("proposalReference", rProp),
        ProgramReferenceBinding.Option("programReference", rProg),
        TimeAccountingCategoryBinding("timeAccountingCategory", rCategory)
      ) =>
        (rPid, rProp, rProg, rCategory).parMapN(apply)
