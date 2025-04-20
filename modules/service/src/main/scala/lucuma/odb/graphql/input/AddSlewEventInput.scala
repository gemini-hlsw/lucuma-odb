// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.enums.SlewStage
import lucuma.core.model.Observation
import lucuma.odb.graphql.binding.*

case class AddSlewEventInput(
  observationId: Observation.Id,
  slewStage:     SlewStage
)

object AddSlewEventInput:

  val Binding: Matcher[AddSlewEventInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ObservationIdBinding("observationId", rObsId),
        SlewStageBinding("slewStage", rStage)
      ) =>
        (rObsId, rStage).parMapN: (oid, stg) =>
          AddSlewEventInput(oid, stg)