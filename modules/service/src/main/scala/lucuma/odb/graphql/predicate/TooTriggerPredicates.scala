// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import lucuma.core.enums.TooActivation
import lucuma.core.model.Program
import lucuma.core.util.Timestamp
import lucuma.odb.data.TooTrigger
import lucuma.odb.data.TooTriggerStatus

class TooTriggerPredicates(path: Path):
  val id          = LeafPredicates[TooTrigger.Id](path / "id")
  val programId   = LeafPredicates[Program.Id](path / "programId")
  val observation = ObservationPredicates(path / "observation")
  val status      = LeafPredicates[TooTriggerStatus](path / "status")
  val activation  = LeafPredicates[TooActivation](path / "tooActivation")
  val requestedAt = LeafPredicates[Timestamp](path / "requestedAt")
  val updatedAt   = LeafPredicates[Timestamp](path / "updatedAt")
