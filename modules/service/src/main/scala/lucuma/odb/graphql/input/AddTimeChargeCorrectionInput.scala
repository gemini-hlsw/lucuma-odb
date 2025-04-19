// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.model.Visit
import lucuma.odb.graphql.binding.*

case class AddTimeChargeCorrectionInput(
  visitId:    Visit.Id,
  correction: TimeChargeCorrectionInput
)

object AddTimeChargeCorrectionInput {

  val Binding: Matcher[AddTimeChargeCorrectionInput] =
    ObjectFieldsBinding.rmap {
      case List(
        VisitIdBinding("visitId", rVisitId),
        TimeChargeCorrectionInput.Binding("correction", rCorrection)
      ) => (rVisitId, rCorrection).parMapN { (visitId, correction) =>
        AddTimeChargeCorrectionInput(visitId, correction)
      }
    }

}
