// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model

import lucuma.core.util.Enumerated

enum ResourceUsage(val tag: String) derives Enumerated:
  case Science     extends ResourceUsage("SCIENCE")
  case Engineering extends ResourceUsage("ENGINEERING")
  case Unavailable extends ResourceUsage("UNAVAILABLE")
