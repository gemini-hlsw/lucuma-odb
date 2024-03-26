// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package predicate

import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.enums.FocalPlane
import grackle.Path

class SpectroscopyConfigOptionPredicates(path: Path) {

  // i think we define predicates for all the things in sco and then in the
  // query mapping we take science requirements as a parameter and use it to
  // define a big predicate that matches
//  lazy val slitWidth =
  lazy val capability = new LeafPredicates[SpectroscopyCapabilities](path / "capability")
  lazy val focalPlane = new LeafPredicates[FocalPlane](path / "focalPlane")
  lazy val resolution = new LeafPredicates[PosInt](path / "resolution")
}
