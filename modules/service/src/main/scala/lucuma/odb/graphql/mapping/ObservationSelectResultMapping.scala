// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Result
import lucuma.odb.graphql.BaseMapping
import lucuma.odb.graphql.table.AsterismTargetTable
import lucuma.odb.graphql.table.ConstraintSetGroupView
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.ProgramTable
import lucuma.odb.graphql.table.TargetView
import skunk.codec.numeric.int8

import scala.tools.util.PathResolver.Environment

trait ObservationSelectResultMapping[F[_]] 
  extends ConstraintSetGroupView[F] with ObservationView[F] with ProgramTable[F] with TargetView[F] with AsterismTargetTable[F] with ResultMapping[F] {

  lazy val ObservationSelectResultMapping: TypeMapping =
    SwitchMapping(
      ObservationSelectResultType, 
      List(
        (QueryType, "observations", topLevelSelectResultMapping(ObservationSelectResultType)),
        (ProgramType, "observations", nestedSelectResultMapping(ObservationSelectResultType, ProgramTable.Id, Join(ProgramTable.Id, ObservationView.ProgramId))),
        (ConstraintSetGroupType, "observations", nestedSelectResultMapping(ObservationSelectResultType, ConstraintSetGroupView.ConstraintSetKey, Join(ConstraintSetGroupView.ConstraintSetKey, ObservationView.ConstraintSet.Key))),
        (TargetGroupType, "observations", nestedSelectResultMapping(ObservationSelectResultType, TargetView.TargetId, Join(TargetView.TargetId, AsterismTargetTable.TargetId), Join(AsterismTargetTable.ObservationId, ObservationView.Id))),
      )
    )
    
}