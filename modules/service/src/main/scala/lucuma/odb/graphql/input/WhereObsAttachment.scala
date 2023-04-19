// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import lucuma.core.model.ObsAttachment
import lucuma.odb.graphql.binding._

object WhereObsAttachment {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereOrderObsAttachmentId = WhereOrder.binding[ObsAttachment.Id](path / "id", ObsAttachmentIdBinding)
    val WhereFileNameBinding   = WhereOptionString.binding(path / "fileName")

    lazy val WhereObsAttachmentBinding = binding(path)
    ObjectFieldsBinding.rmap {
      case List(
            WhereObsAttachmentBinding.List.Option("AND", rAND),
            WhereObsAttachmentBinding.List.Option("OR", rOR),
            WhereObsAttachmentBinding.Option("NOT", rNOT),
            WhereOrderObsAttachmentId.Option("id", rId),
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
