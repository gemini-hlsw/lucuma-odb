// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import lucuma.core.util.Enumerated

// TODO: Delete me?

/**
 * Indicates whether a calibration comes before or after its corresponding
 * science step.
 */
enum CalLocation(val tag: String) derives Enumerated:
  case Before extends CalLocation("before")
  case After  extends CalLocation("after")