// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.syntax

import cats.syntax.monoid.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.exposure.ExposureTimeViolation
import lucuma.core.model.sequence.exposure.PendingExposureRules

trait ToSequenceDigestOps:

  extension (self: SequenceDigest)

    /**
     * Adds an atom to the digest, checking the exposure times of its steps
     * (see `ExposureTimeViolation.check`) as it goes.
     */
    def add[D: PendingExposureRules](a: Atom[D], ctx: PendingExposureRules.Context): SequenceDigest =
      SequenceDigest(
        observeClass           = self.observeClass |+| a.observeClass,
        timeEstimate           = self.timeEstimate |+| a.timeEstimate,
        telescopeConfigs       = self.telescopeConfigs ++ a.steps.toList.map(_.telescopeConfig),
        atomCount              = NonNegInt.unsafeFrom(self.atomCount.value + 1),
        gcalSets               =
          if a.steps.exists(_.stepConfig.usesGcalUnit) then NonNegInt.unsafeFrom(self.gcalSets.value + 1)
          else self.gcalSets,
        steps                  = a.steps.toList.foldLeft(self.steps)(_.add(_)),
        executionState         = self.executionState,
        exposureTimeViolations =
          self.exposureTimeViolations ++ a.steps.toList.flatMap(ExposureTimeViolation.check(_, ctx))
      )

object sequencedigest extends ToSequenceDigestOps
