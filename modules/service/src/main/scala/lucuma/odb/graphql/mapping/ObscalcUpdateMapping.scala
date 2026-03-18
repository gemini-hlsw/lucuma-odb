// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.core.model.Observation
import lucuma.core.util.CalculationState
import lucuma.odb.data.EditType
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.ProgramTable

trait ObscalcUpdateMapping[F[_]] extends ObservationView[F] with ProgramTable[F]:

  // This is slightly complicated by hard deletion of observations.  The id has
  // to be the program id and we need a join to get to the "value" observation.

  // N.B. env is populated by the subscription elaborator
  lazy val ObscalcUpdateMapping =
    ObjectMapping(ObscalcUpdateType)(
      SqlField("synthetic-id", ProgramTable.Id, key = true, hidden = true),
      CursorField("editType",            _.envR[EditType]("editType"),                 List("synthetic-id")),
      CursorField("oldCalculationState", _.envR[Option[CalculationState]]("oldState"), List("synthetic-id")),
      CursorField("oldState",            _.envR[Option[CalculationState]]("oldState"), List("synthetic-id")),
      CursorField("newCalculationState", _.envR[Option[CalculationState]]("newState"), List("synthetic-id")),
      CursorField("newState",            _.envR[Option[CalculationState]]("newState"), List("synthetic-id")),
      CursorField("observationId",       _.envR[Observation.Id]("observationId"),      List("synthetic-id")),
      SqlObject("value", Join(ProgramTable.Id, ObservationView.ProgramId))
    )
