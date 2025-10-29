// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import lucuma.odb.data.ObservingModeRowVersion

class GmosImagingFilterPredicates(path: Path):
  lazy val version: LeafPredicates[ObservingModeRowVersion] =
    LeafPredicates[ObservingModeRowVersion](path / "version")
