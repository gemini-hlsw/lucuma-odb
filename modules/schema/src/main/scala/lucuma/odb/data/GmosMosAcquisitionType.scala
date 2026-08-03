// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

/**
 * Whether a GMOS MOS acquisition image is taken with the mask in the beam
 * (imaging the alignment holes cut into the mask) or out of the beam.
 *
 * TODO: move to lucuma core
 */
enum GmosMosAcquisitionType(val tag: String) derives Enumerated:
  case MaskIn  extends GmosMosAcquisitionType("MaskIn")
  case MaskOut extends GmosMosAcquisitionType("MaskOut")
