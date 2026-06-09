// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model

import lucuma.core.util.Enumerated

enum TooSupport(val tag: String) derives Enumerated:
  case Standard  extends TooSupport("Standard")
  case Interrupt extends TooSupport("Interrupt")
  case Rapid     extends TooSupport("Rapid")
  case None      extends TooSupport("None")
