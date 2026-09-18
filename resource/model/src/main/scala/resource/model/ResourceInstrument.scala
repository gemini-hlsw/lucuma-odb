// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model

import lucuma.core.util.Enumerated

enum ResourceInstrument(val tag: String) derives Enumerated:
  case AcqCam      extends ResourceInstrument("ACQ_CAM")
  case Alopeke     extends ResourceInstrument("ALOPEKE")
  case Altair      extends ResourceInstrument("ALTAIR")
  case CalZorro    extends ResourceInstrument("CAL_ZORRO")
  case Canopus     extends ResourceInstrument("CANOPUS")
  case Engineering extends ResourceInstrument("ENGINEERING")
  case F2          extends ResourceInstrument("F2")
  case Gcal        extends ResourceInstrument("GCAL")
  case Ghost       extends ResourceInstrument("GHOST")
  case Gmos        extends ResourceInstrument("GMOS")
  case Gnirs       extends ResourceInstrument("GNIRS")
  case Gpi         extends ResourceInstrument("GPI")
  case Gsaoi       extends ResourceInstrument("GSAOI")
  case Igrins2     extends ResourceInstrument("IGRINS2")
  case Iqueye      extends ResourceInstrument("IQUEYE")
  case MaroonX     extends ResourceInstrument("MAROON_X")
  case Niri        extends ResourceInstrument("NIRI")
  case Scorpio     extends ResourceInstrument("SCORPIO")
  case Unknown     extends ResourceInstrument("UNKNOWN")
