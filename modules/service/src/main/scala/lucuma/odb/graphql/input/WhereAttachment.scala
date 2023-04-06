// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import lucuma.core.model.Attachment
import lucuma.odb.graphql.binding._

object WhereAttachment {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereOrderAttachmentId = WhereOrder.binding[Attachment.Id](path / "id", AttachmentIdBinding)
    val WhereFileNameBinding   = WhereOptionString.binding(path / "fileName")

    lazy val WhereAttachmentBinding = binding(path)
    ObjectFieldsBinding.rmap {
      case List(
            WhereAttachmentBinding.List.Option("AND", rAND),
            WhereAttachmentBinding.List.Option("OR", rOR),
            WhereAttachmentBinding.Option("NOT", rNOT),
            WhereOrderAttachmentId.Option("id", rId),
            WhereFileNameBinding.Option("fileName", rFileName)
          ) =>
        (rAND, rOR, rNOT, rId, rFileName).parMapN { (AND, OR, NOT, id, name) =>
          and(
            List(
              AND.map(and),
              OR.map(or),
              NOT.map(Not(_)),
              id,
              name
            ).flatten
          )
        }
    }
  }
}
