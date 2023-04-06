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

case class UpdateAttachmentsInput(
  programId: Program.Id,
  SET:       AttachmentPropertiesInput.Edit,
  WHERE:     Option[Predicate],
  LIMIT:     Option[NonNegInt]
)

object UpdateAttachmentsInput {

  def binding(path: Path): Matcher[UpdateAttachmentsInput] = {
    val WhereAttachmentBinding = WhereAttachment.binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding("programId", rPid),
        AttachmentPropertiesInput.EditBinding("SET", rSET),
        WhereAttachmentBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT)
      ) =>
        (rPid, rSET, rWHERE, rLIMIT).parMapN(apply)
    }
  }
}
