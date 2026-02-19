// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package predicate

import eu.timepit.refined.types.numeric.PosInt
import grackle.Path
import lucuma.core.model.sequence.Atom
import lucuma.core.util.Timestamp
import lucuma.odb.data.AtomExecutionState

class AtomRecordPredicates(path: Path):
  lazy val id            = LeafPredicates[Atom.Id](path / "id")
  lazy val index         = LeafPredicates[PosInt](path / "index")
  lazy val lastEventTime = LeafPredicates[Timestamp](path / "_lastEventTime")
  lazy val visit         = new VisitPredicates(path / "visit")
  lazy val execution     = LeafPredicates[AtomExecutionState](path / "executionState")