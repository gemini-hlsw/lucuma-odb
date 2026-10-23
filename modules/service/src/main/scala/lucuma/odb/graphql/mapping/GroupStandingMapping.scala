// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path
import lucuma.odb.graphql.table.GroupStandingView

// CalculatedGroupStanding appears under both Group and Observation, each with
// its own view, so it is mapped by path.
trait GroupStandingMapping[F[_]] extends GroupStandingView[F]:

  private def calculatedGroupStandingMappings(
    path:     Path,
    key:      ColumnRef,
    standing: ColumnRef,
    deciding: ColumnRef,
    calc:     ColumnRef
  ): List[TypeMapping] =
    List(
      ObjectMapping(path)(
        SqlField("synthetic_id",     key, hidden = true, key = true),
        SqlField("calculationState", calc),
        SqlObject("value")
      ),
      ObjectMapping(path / "value")(
        SqlField("synthetic_id", key, hidden = true, key = true),
        SqlField("status",       standing),
        SqlField("groupId",      deciding)
      )
    )

  lazy val GroupStandingMappings: List[TypeMapping] =
    calculatedGroupStandingMappings(
      GroupType / "standing",
      GroupStandingView.GroupId,
      GroupStandingView.Standing,
      GroupStandingView.DecidingGroupId,
      GroupStandingView.CalculationState
    ) ++
    calculatedGroupStandingMappings(
      ObservationType / "groupStanding",
      ObservationGroupStandingView.ObservationId,
      ObservationGroupStandingView.Standing,
      ObservationGroupStandingView.DecidingGroupId,
      ObservationGroupStandingView.CalculationState
    )
