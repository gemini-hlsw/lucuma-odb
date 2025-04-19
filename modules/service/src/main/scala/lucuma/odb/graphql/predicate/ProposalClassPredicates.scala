// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import lucuma.odb.data.Tag

class ProposalClassPredicates(path: Path) {
  val discriminator = LeafPredicates[Tag](path / "discriminator")
}