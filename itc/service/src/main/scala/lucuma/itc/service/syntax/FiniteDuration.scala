// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.syntax

import scala.concurrent.duration.*

trait FiniteDurationSyntax:
  extension (self: FiniteDuration)

    /** Returns this `FiniteDuration` in double-precision seconds. */
    def toDoubleSeconds: Double =
      self.toNanos / 1000000000.0

    /** Second-precision ceiling. */
    def secondsCeil: FiniteDuration =
      toDoubleSeconds.ceil.seconds

    /** Second-precision ceiling, if `b` otherwise return this value unchanged. */
    def secondsCeilIf(b: Boolean): FiniteDuration =
      if (b) secondsCeil else self

object finiteduration extends FiniteDurationSyntax
