// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model

import lucuma.core.util.Enumerated

enum TelescopeModeType(val tag: String) derives Enumerated:
  case Queue           extends TelescopeModeType("QUEUE")
  case Classical       extends TelescopeModeType("CLASSICAL")
  case PriorityVisitor extends TelescopeModeType("PRIORITY_VISITOR")
  case Engineering     extends TelescopeModeType("ENGINEERING")
  case Commissioning   extends TelescopeModeType("COMMISSIONING")
