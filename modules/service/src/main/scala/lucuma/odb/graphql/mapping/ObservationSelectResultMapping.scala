// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.odb.graphql.table.*

trait ObservationSelectResultMapping[F[_]]
  extends ConstraintSetGroupView[F]
     with ObservationView[F]
     with ProgramTable[F]
     with TargetView[F]
     with AsterismTargetTable[F]
     with AsterismGroupView[F]
     with ObservingModeGroupView[F]
     with ResultMapping[F] {

  lazy val ObservationSelectResultMappings: List[TypeMapping] =
    List(
      topLevelSelectResultMappingAtPath(QueryType / "observations"),
      nestedSelectResultMappingAtPath(ProgramType / "observations", ProgramTable.Id, Join(ProgramTable.Id, ObservationView.ProgramId)),
      nestedSelectResultMappingAtPath(ConstraintSetGroupType / "observations", ConstraintSetGroupView.ConstraintSetKey, Join(ConstraintSetGroupView.ConstraintSetKey, ObservationView.ConstraintSet.Key)),
      nestedSelectResultMappingAtPath(ObservingModeGroupType / "observations", ObservingModeGroupView.ObservingModeKey, Join(ObservingModeGroupView.ObservingModeKey, ObservationView.ObservingMode.Key)),
      nestedSelectResultMappingAtPath(TargetGroupType / "observations", TargetView.TargetId, Join(TargetView.TargetId, AsterismTargetTable.TargetId), Join(AsterismTargetTable.ObservationId, ObservationView.Id)),
      nestedSelectResultMappingAtPath(AsterismGroupType / "observations", AsterismGroupView.AsterismGroup, Join(AsterismGroupView.AsterismGroup, ObservationView.AsterismGroup)),
    )

}
