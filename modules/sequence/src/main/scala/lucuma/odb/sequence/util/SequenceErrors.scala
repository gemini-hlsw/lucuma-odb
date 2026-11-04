// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.util

import cats.syntax.option.*
import lucuma.core.model.Observation
import lucuma.odb.data.OdbError

def definitionError(oid: Observation.Id, msg: String): OdbError =
  OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $msg".some)
