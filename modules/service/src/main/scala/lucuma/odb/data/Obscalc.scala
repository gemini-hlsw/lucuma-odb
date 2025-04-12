// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.util.Enumerated
import lucuma.core.util.Timestamp
import lucuma.odb.service.ItcService

case class Obscalc(
  observationId:    Observation.Id,
  state:            Obscalc.State,
  lastInvalidation: Timestamp,
  lastUpdate:       Timestamp,
  result:          Obscalc.Result
)

object Obscalc:

  case class PendingCalc(
    programId:        Program.Id,
    observationId:    Observation.Id,
    lastInvalidation: Timestamp
  )

  enum State(val tag: String) derives Enumerated:
    case Pending     extends State("pending")
    case Calculating extends State("calculating")
    case Ready       extends State("ready")

  case class Result(
    itc:    ItcService.Result,
    digest: Option[ExecutionDigest]
  )