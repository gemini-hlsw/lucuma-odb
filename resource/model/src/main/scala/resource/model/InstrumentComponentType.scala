// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model

import lucuma.core.util.Enumerated

enum InstrumentComponentType(val tag: String) derives Enumerated:
  case Filter    extends InstrumentComponentType("FILTER")
  case Disperser extends InstrumentComponentType("DISPERSER")
  case Fpu       extends InstrumentComponentType("FPU")
  case Wfs       extends InstrumentComponentType("WFS")
  case Other     extends InstrumentComponentType("OTHER")
