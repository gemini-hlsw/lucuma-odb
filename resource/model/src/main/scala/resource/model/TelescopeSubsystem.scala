// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model

import lucuma.core.util.Enumerated

enum TelescopeSubsystem(val tag: String) derives Enumerated:
  case Pwfs1         extends TelescopeSubsystem("PWFS1")
  case Pwfs2         extends TelescopeSubsystem("PWFS2")
  case Altair        extends TelescopeSubsystem("ALTAIR")
  case Canopus       extends TelescopeSubsystem("CANOPUS")
  case Lgs           extends TelescopeSubsystem("LGS")
  case Gpol          extends TelescopeSubsystem("GPOL")
  case DomeShutter   extends TelescopeSubsystem("DOME_SHUTTER")
  case DomeVentGates extends TelescopeSubsystem("DOME_VENT_GATES")
