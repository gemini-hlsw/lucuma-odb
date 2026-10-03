// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model

import lucuma.core.util.Enumerated

enum InstrumentPlace(val tag: String) derives Enumerated:
  case Port    extends InstrumentPlace("PORT")
  case Floor   extends InstrumentPlace("FLOOR")
  case Lab     extends InstrumentPlace("LAB")
  case Base    extends InstrumentPlace("BASE")
  case Unknown extends InstrumentPlace("UNKNOWN")
