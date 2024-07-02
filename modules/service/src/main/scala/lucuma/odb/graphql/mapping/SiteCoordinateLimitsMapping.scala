// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import grackle.Path
import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.CallForProposalsView

trait SiteCoordinateLimitsMapping[F[_]] extends CallForProposalsView[F] {

  private def mappingAtPath(
    path:     Path,
    idColumn: ColumnRef
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("synthetic_id", idColumn, key = true, hidden = true),
      SqlObject("north"),
      SqlObject("south")
    )

  lazy val SiteCoordinateLimitsMappings: List[TypeMapping] =
    List(
      mappingAtPath(CallForProposalsType / "coordinateLimits", CallForProposalsView.Id)
    )

}

