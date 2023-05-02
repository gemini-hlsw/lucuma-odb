// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel.*
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.model.Program
import lucuma.odb.graphql.binding._

case class UpdateProposalAttachmentsInput(
  programId: Program.Id,
  SET:       ProposalAttachmentPropertiesInput.Edit,
  WHERE:     Option[Predicate],
  LIMIT:     Option[NonNegInt]
)

object UpdateProposalAttachmentsInput {

  def binding(path: Path): Matcher[UpdateProposalAttachmentsInput] = {
    val WhereProposalAttachmentBinding = WhereProposalAttachment.binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding("programId", rPid),
        ProposalAttachmentPropertiesInput.EditBinding("SET", rSET),
        WhereProposalAttachmentBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT)
      ) =>
        (rPid, rSET, rWHERE, rLIMIT).parMapN(apply)
    }
  }
}
