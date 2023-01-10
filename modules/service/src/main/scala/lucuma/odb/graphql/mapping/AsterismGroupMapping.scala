// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table._

trait AsterismGroupMapping[F[_]] 
  extends AsterismGroupView[F] 
    with AsterismTargetTable[F]
    with ObservationView[F] 
    with ProgramTable[F]
    with TargetView[F] {

  lazy val AsterismGroupMapping: TypeMapping =
    ObjectMapping(
      tpe = AsterismGroupType,
      fieldMappings = List(

        // Our key, which is hidden
        SqlField("asterismGroup", AsterismGroupView.AsterismGroup, key = true, hidden = true),

        // User-visible fields
        SqlObject("program", Join(AsterismGroupView.ProgramId, ProgramTable.Id)),
        SqlObject("observations", Join(AsterismGroupView.AsterismGroup, ObservationView.AsterismGroup)),
        SqlObject("asterism", Join(AsterismGroupView.ExampleObservationId, AsterismTargetTable.ObservationId), Join(AsterismTargetTable.TargetId, TargetView.TargetId)),

      )
    )

}
