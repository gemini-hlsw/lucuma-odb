// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.GhostStaticView

trait GhostStaticMapping[F[_]] extends GhostStaticView[F]:

  lazy val GhostStaticMapping: ObjectMapping =
    ObjectMapping(GhostStaticType)(
      SqlField("id", GhostStaticView.Id, key = true, hidden = true),
      SqlField("resolutionMode", GhostStaticView.ResolutionMode),
      SqlObject("slitViewingCameraExposureTime")
    )