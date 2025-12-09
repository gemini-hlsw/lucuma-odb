// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos.imaging

import lucuma.core.util.Enumerated

enum VariantType(val tag: String, val display: String) derives Enumerated:
  case Grouped     extends VariantType("grouped", "Grouped")
  case Interleaved extends VariantType("interleaved", "Interleaved")
  case PreImaging  extends VariantType("pre_imaging", "Pre-Imaging")