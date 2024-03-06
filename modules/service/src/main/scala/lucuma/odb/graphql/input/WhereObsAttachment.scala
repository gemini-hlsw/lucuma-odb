// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import grackle.Path
import grackle.Predicate
import grackle.Predicate._
import lucuma.core.model.ObsAttachment
import lucuma.odb.graphql.binding._

object WhereObsAttachment {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereOrderObsAttachmentId = WhereOrder.binding[ObsAttachment.Id](path / "id", ObsAttachmentIdBinding)
    val WhereFileNameBinding   = WhereString.binding(path / "fileName")
    val WhereDescriptionBinding   = WhereOptionString.binding(path / "description")
    val WhereAttachmentTypeBinding = WhereUnorderedTag.binding(path / "attachmentType", TagBinding)
    val WhereProgramBinding = WhereProgram.binding(path / "program")

    lazy val WhereObsAttachmentBinding = binding(path)
    ObjectFieldsBinding.rmap {
      case List(
            WhereObsAttachmentBinding.List.Option("AND", rAND),
            WhereObsAttachmentBinding.List.Option("OR", rOR),
            WhereObsAttachmentBinding.Option("NOT", rNOT),
            WhereOrderObsAttachmentId.Option("id", rId),
            WhereFileNameBinding.Option("fileName", rFileName),
            WhereDescriptionBinding.Option("description", rDescription),
            WhereAttachmentTypeBinding.Option("attachmentType", rAttachmentType),
            BooleanBinding.Option("checked", rChecked),
            WhereProgramBinding.Option("program", rProgram)
          ) =>
        (rAND, rOR, rNOT, rId, rFileName, rDescription, rAttachmentType, rChecked, rProgram).parMapN {
          (AND, OR, NOT, id, name, desc, atType, checked, program) =>
            and(
              List(
                AND.map(and),
                OR.map(or),
                NOT.map(Not(_)),
                id,
                name,
                desc,
                atType,
                checked.map(b => Eql(path / "checked", Const(b))),
                program
              ).flatten
            )
        }
    }
  }
}
