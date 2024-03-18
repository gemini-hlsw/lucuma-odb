// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

enum ExecutionEventType(val dbTag: String):
  case Sequence extends ExecutionEventType("sequence")
  case Slew     extends ExecutionEventType("slew")
  case Step     extends ExecutionEventType("step")
  case Dataset  extends ExecutionEventType("dataset")

object ExecutionEventType:

  given Enumerated[ExecutionEventType] =
    Enumerated.from(
      Sequence,
      Slew,
      Step,
      Dataset
    ).withTag(_.dbTag)