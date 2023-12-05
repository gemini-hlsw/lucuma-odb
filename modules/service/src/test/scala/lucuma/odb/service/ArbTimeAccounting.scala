// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.core.enums.ChargeClass
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.arb.ArbCategorizedTime
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbGid
import lucuma.core.util.arb.ArbTimeSpan
import lucuma.core.util.arb.ArbTimestamp
import lucuma.core.util.arb.ArbUid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbTimeAccounting {

  import ArbCategorizedTime.given
  import ArbEnumerated.*
  import ArbGid.*
  import ArbTimestamp.given
  import ArbTimeSpan.given
  import ArbUid.*

  import TimeAccounting.*

  given Arbitrary[Charge] =
    Arbitrary {
      for {
        c <- arbitrary[CategorizedTime]
        u <- arbitrary[TimeSpan]
      } yield Charge(c, u)
    }

  given Cogen[Charge] =
    Cogen[(
      CategorizedTime,
      TimeSpan
    )].contramap { a => (
      a.categorized,
      a.uncategorized
    )}

  given Arbitrary[StepContext] =
    Arbitrary {
      for {
        a <- arbitrary[Atom.Id]
        s <- arbitrary[Step.Id]
        c <- arbitrary[ChargeClass]
      } yield StepContext(a, s, c)
    }

  given Cogen[StepContext] =
    Cogen[(
      Atom.Id,
      Step.Id,
      ChargeClass
    )].contramap { a => (
      a.atomId,
      a.stepId,
      a.chargeClass
    )}

  given Arbitrary[Context] =
    Arbitrary {
      for {
        v <- arbitrary[Visit.Id]
        s <- arbitrary[Option[StepContext]]
      } yield Context(v, s)
    }

  given Cogen[Context] =
    Cogen[(
      Visit.Id,
      Option[StepContext]
    )].contramap { a => (
      a.visitId,
      a.step
    )}

  given Arbitrary[Event] =
    Arbitrary {
      for {
        t <- arbitrary[Timestamp]
        c <- arbitrary[Context]
      } yield Event(t, c)
    }

  given Cogen[Event] =
    Cogen[(
      Timestamp,
      Context
    )].contramap { a => (
      a.timestamp,
      a.context
    )}

}

object ArbTimeAccounting extends ArbTimeAccounting
