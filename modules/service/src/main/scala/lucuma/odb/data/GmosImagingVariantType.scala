// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

enum GmosImagingVariantType(val tag: String) derives Enumerated:
  case Grouped     extends GmosImagingVariantType("grouped")
  case Interleaved extends GmosImagingVariantType("interleaved")
  case PreImaging  extends GmosImagingVariantType("pre_imaging")