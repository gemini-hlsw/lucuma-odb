// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model

import lucuma.core.util.Enumerated

enum TelescopeModeType(val tag: String) derives Enumerated:
  case Queue           extends TelescopeModeType("Queue")
  case Classical       extends TelescopeModeType("Classical")
  case PriorityVisitor extends TelescopeModeType("PriorityVisitor")
  case Engineering     extends TelescopeModeType("Engineering")
  case Commissioning   extends TelescopeModeType("Commissioning")
