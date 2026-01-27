// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.syntax.option.*
import lucuma.core.model.Observation
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.ObservingMode.Syntax.*

object GeneratorError:
  def sequenceUnavailable(oid: Observation.Id, message: String): OdbError =
    OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $message".some)

  def sequenceTooLong(oid: Observation.Id): OdbError =
    sequenceUnavailable(oid, s"The generated sequence is too long (more than ${Generator.SequenceAtomLimit} atoms).")

  def programNotFound(oid: Observation.Id): OdbError =
    OdbError.InvalidObservation(oid, s"Program for observation $oid not found.".some)

  def unexpectedMode(oid: Observation.Id, expected: String, actual: ObservingMode): OdbError =
    OdbError.InvalidObservation(oid, s"Expected observation $oid as $expected, but was ${actual.name}".some)