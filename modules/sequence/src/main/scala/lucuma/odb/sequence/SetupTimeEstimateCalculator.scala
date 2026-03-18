// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.model.sequence.SetupTime
import lucuma.core.util.TimeSpan

/**
 * Estimates the cost of setup.
 */
trait SetupTimeEstimateCalculator:

  /**
   * Provides a rough estimate of the setup time, which includes acquisition.
   */
  def estimateSetupTime: SetupTime

  /**
   * Estimates the number of setups that will be required to execute an
   * observation of the given `scienceTime` duration.
   */
  def estimateSetupCount(scienceTime: TimeSpan): NonNegInt

  /**
   * Total setup time estimate for the observation as a whole.
   */
  def totalSetupTime(scienceTime: TimeSpan): TimeSpan =
    estimateSetupTime.full *| estimateSetupCount(scienceTime).value