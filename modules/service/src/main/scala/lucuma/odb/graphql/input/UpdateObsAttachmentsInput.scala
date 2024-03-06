// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Path
import grackle.Predicate
import lucuma.odb.graphql.binding._

case class UpdateObsAttachmentsInput(
  SET:       ObsAttachmentPropertiesInput.Edit,
  WHERE:     Option[Predicate],
  LIMIT:     Option[NonNegInt]
)

object UpdateObsAttachmentsInput {

  def binding(path: Path): Matcher[UpdateObsAttachmentsInput] = {
    val WhereObsAttachmentBinding = WhereObsAttachment.binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        ObsAttachmentPropertiesInput.EditBinding("SET", rSET),
        WhereObsAttachmentBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT)
      ) =>
        (rSET, rWHERE, rLIMIT).parMapN(apply)
    }
  }
}
