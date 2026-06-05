// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.TypeRef
import grackle.Query.Binding
import grackle.Query.FilterOrderByOffsetLimit
import grackle.Query.OrderSelection
import grackle.QueryCompiler.Elab
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.TimingWindowView

trait SchedulingPropertiesMapping[F[_]]
  extends ObservationView[F]
    with TimingWindowView[F]:

  lazy val SchedulingPropertiesMapping: ObjectMapping =
    ObjectMapping(SchedulingPropertiesType)(
      SqlField("id", ObservationView.Id, key = true, hidden = true),
      SqlField("isSplittable", ObservationView.IsSplittable),
      SqlObject("timingWindows", Join(ObservationView.Id, TimingWindowView.ObservationId))
    )

  lazy val SchedulingPropertiesElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {

    case (SchedulingPropertiesType, "timingWindows", Nil) =>
        Elab.transformChild: child =>
          FilterOrderByOffsetLimit(
            pred = None,
            oss = Some(List(
              OrderSelection[Long](TimingWindowType / "id", true, true)
            )),
            offset = None,
            limit = None,
            child
          )

  }