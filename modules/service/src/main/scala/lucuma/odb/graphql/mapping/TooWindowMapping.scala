// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ObservationView

// How long a Target of Opportunity is open for once triggered.  Absent when the
// PI stated nothing, in which case the activation supplies the default; present
// with a null duration when what they stated was Forever.
trait TooWindowMapping[F[_]] extends ObservationView[F]:

  lazy val TooWindowMapping: ObjectMapping =
    ObjectMapping(SchedulingConstraintsType / "tooWindow")(
      SqlField("synthetic_id", ObservationView.TooWindow.SyntheticId, key = true, hidden = true),
      SqlField("forever",      ObservationView.TooWindow.Forever),
      SqlObject("duration")
    )
