// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.enums.SlewStage
import lucuma.core.model.Client
import lucuma.core.model.Observation
import lucuma.odb.graphql.binding.*

case class AddSlewEventInput(
  observationId: Observation.Id,
  slewStage:     SlewStage,
  clientId:      Option[Client.Id]
)

object AddSlewEventInput:

  val Binding: Matcher[AddSlewEventInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ObservationIdBinding("observationId", rObsId),
        SlewStageBinding("slewStage", rStage),
        ClientIdBinding.Option("clientId", rCid)
      ) =>
        (rObsId, rStage, rCid).parMapN: (oid, stg, cid) =>
          AddSlewEventInput(oid, stg, cid)