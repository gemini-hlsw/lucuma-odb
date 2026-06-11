// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import grackle.Path
import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.CallForProposalsView

trait CoordinateLimitsMapping[F[_]] extends CallForProposalsView[F]:

  private def mappingAtPath(
    path:     Path,
    idColumn: ColumnRef
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("synthetic_id", idColumn, key = true, hidden = true),
      SqlObject("raStart"),
      SqlObject("raEnd"),
      SqlObject("decStart"),
      SqlObject("decEnd")
    )

  lazy val CoordinateLimitsMappings: List[TypeMapping] =
    List(
      mappingAtPath(GeminiCallPropertiesType / "coordinateLimits" / "north", CallForProposalsView.gemini.Id),
      mappingAtPath(GeminiCallPropertiesType / "coordinateLimits" / "south", CallForProposalsView.gemini.Id),
      mappingAtPath(KeckCallPropertiesType   / "coordinateLimits",           CallForProposalsView.keck.Id),
      mappingAtPath(SubaruCallPropertiesType / "coordinateLimits",           CallForProposalsView.subaru.Id)
    )