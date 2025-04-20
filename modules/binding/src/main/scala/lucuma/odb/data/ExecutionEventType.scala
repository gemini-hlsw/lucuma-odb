// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

enum ExecutionEventType(val tag: String) derives Enumerated:
  case Sequence extends ExecutionEventType("sequence")
  case Slew     extends ExecutionEventType("slew")
  case Atom     extends ExecutionEventType("atom")
  case Step     extends ExecutionEventType("step")
  case Dataset  extends ExecutionEventType("dataset")