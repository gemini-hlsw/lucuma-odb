// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ObservationView

trait ObservationTimeEstimateMapping[F[_]] extends ObservationView[F]:

  import ObservationView.OriginalEstimate

  lazy val ObservationTimeEstimateMappings: List[TypeMapping] =
    List(
      ObjectMapping(ExecutionType / "originalEstimate")(
        SqlField("id", OriginalEstimate.SyntheticId, key = true, hidden = true),
        SqlObject("setup"),
        SqlField("setupCount", OriginalEstimate.SetupCount),
        SqlObject("science"),
        SqlObject("fullTimeEstimate")
      ),

      ObjectMapping(ExecutionType / "originalEstimate" / "setup")(
        SqlField("id", OriginalEstimate.SyntheticId, key = true, hidden = true),
        SqlObject("full"),
        SqlObject("reacquisition")
      )
    )
