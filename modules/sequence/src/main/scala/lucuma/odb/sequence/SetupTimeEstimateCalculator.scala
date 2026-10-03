// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
   * Estimates the number of reacquisitions (recenterings on the target without
   * a full setup) that will be required to execute an observation of the given
   * `scienceTime` duration.
   */
  def estimateReacquisitionCount(scienceTime: TimeSpan): NonNegInt

  /**
   * Total setup time estimate for the observation as a whole.
   */
  def totalSetupTime(scienceTime: TimeSpan): TimeSpan =
    (estimateSetupTime.full          *| estimateSetupCount(scienceTime).value) +|
    (estimateSetupTime.reacquisition *| estimateReacquisitionCount(scienceTime).value)

object SetupTimeEstimateCalculator:

  private val OneHour: Long  = TimeSpan.fromHours(1).get.toMicroseconds
  private val TwoHours: Long = 2 * OneHour

  /**
   * Spectroscopy guided by a PWFS, which does not correct for flexure, must be
   * recentered on the target every hour.  A full setup is charged every two
   * hours and a reacquisition at each hour in between:
   *
   *   setups         = floor(t / 2h) + 1
   *   reacquisitions = floor((t + 1h) / 2h)
   *
   * where `t` is the science time.
   */
  def pwfsSpectroscopy(setup: SetupTime): SetupTimeEstimateCalculator =
    new SetupTimeEstimateCalculator:
      override def estimateSetupTime: SetupTime =
        setup

      override def estimateSetupCount(scienceTime: TimeSpan): NonNegInt =
        if scienceTime.isZero then NonNegInt.MinValue
        else NonNegInt.unsafeFrom((scienceTime.toMicroseconds / TwoHours).toInt + 1)

      override def estimateReacquisitionCount(scienceTime: TimeSpan): NonNegInt =
        if scienceTime.isZero then NonNegInt.MinValue
        else NonNegInt.unsafeFrom(((scienceTime.toMicroseconds + OneHour) / TwoHours).toInt)
