// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import grackle.Path
import grackle.Predicate
import grackle.Predicate._
import lucuma.odb.graphql.binding._

object WhereProposalAttachment {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereFileNameBinding   = WhereString.binding(path / "fileName")
    val WhereDescriptionBinding   = WhereOptionString.binding(path / "description")
    val WhereAttachmentTypeBinding = WhereUnorderedTag.binding(path / "attachmentType", TagBinding)

    lazy val WhereObsAttachmentBinding = binding(path)
    ObjectFieldsBinding.rmap {
      case List(
            WhereObsAttachmentBinding.List.Option("AND", rAND),
            WhereObsAttachmentBinding.List.Option("OR", rOR),
            WhereObsAttachmentBinding.Option("NOT", rNOT),
            WhereFileNameBinding.Option("fileName", rFileName),
            WhereDescriptionBinding.Option("description", rDescription),
            WhereAttachmentTypeBinding.Option("attachmentType", rAttachmentType),
            BooleanBinding.Option("checked", rChecked)
          ) =>
        (rAND, rOR, rNOT, rFileName, rDescription, rAttachmentType, rChecked).parMapN { 
          (AND, OR, NOT, name, desc, atType, checked) =>
            and(
              List(
                AND.map(and),
                OR.map(or),
                NOT.map(Not(_)),
                name,
                desc,
                atType,
                checked.map(b => Eql(path / "checked", Const(b)))
              ).flatten
            )
        }
    }
  }
}
