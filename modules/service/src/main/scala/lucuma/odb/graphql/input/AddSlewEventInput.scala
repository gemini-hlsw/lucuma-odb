// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.enums.SlewStage
import lucuma.core.model.Visit
import lucuma.odb.graphql.binding.*

case class AddSlewEventInput(
  visitId:   Visit.Id,
  slewStage: SlewStage
)

object AddSlewEventInput {

  val Binding: Matcher[AddSlewEventInput] =
    ObjectFieldsBinding.rmap {
      case List(
        VisitIdBinding("visitId", rVisitId),
        SlewStageBinding("slewStage", rStage)
      ) =>
        (rVisitId, rStage).parMapN { (vid, stg) =>
          AddSlewEventInput(vid, stg)
        }
    }

}
