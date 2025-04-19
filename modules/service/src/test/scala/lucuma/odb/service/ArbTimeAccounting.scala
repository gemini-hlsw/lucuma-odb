// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.core.enums.ChargeClass
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbGid
import lucuma.core.util.arb.ArbTimestamp
import lucuma.core.util.arb.ArbUid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbTimeAccounting {

  import ArbEnumerated.given
  import ArbGid.given
  import ArbTimestamp.given
  import ArbUid.given

  import TimeAccounting.*

  given Arbitrary[AtomContext] =
    Arbitrary {
      for {
        a <- arbitrary[Atom.Id]
        s <- arbitrary[Option[Step.Id]]
      } yield AtomContext(a, s)
    }

  given Cogen[AtomContext] =
    Cogen[(
      Atom.Id,
      Option[Step.Id],
    )].contramap { a => (
      a.atomId,
      a.stepId,
    )}

  given Arbitrary[Context] =
    Arbitrary {
      for {
        v <- arbitrary[Visit.Id]
        c <- arbitrary[ChargeClass]
        a <- arbitrary[Option[AtomContext]]
      } yield Context(v, c, a)
    }

  given Cogen[Context] =
    Cogen[(
      Visit.Id,
      ChargeClass,
      Option[AtomContext]
    )].contramap { a => (
      a.visitId,
      a.chargeClass,
      a.atom
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
