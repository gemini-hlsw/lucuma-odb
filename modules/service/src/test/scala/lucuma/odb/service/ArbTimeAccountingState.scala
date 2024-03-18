// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.option.*
import cats.syntax.traverse.*
import lucuma.core.enums.ChargeClass
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbGid
import lucuma.core.util.arb.ArbTimestampInterval
import lucuma.core.util.arb.ArbUid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import org.scalacheck.cats.implicits.*

trait ArbTimeAccountingState {

  import ArbEnumerated.given
  import ArbGid.given
  import ArbTimeAccounting.given
  import ArbTimestampInterval.given
  import ArbUid.given

  import TimeAccounting.*

  val genAtom: Gen[List[Option[StepContext]]] =
    for {
      a  <- arbitrary[Atom.Id]
      n  <- Gen.chooseNum(0, 10)
      s0 <- Gen.listOfN(n, arbitrary[StepContext].map(StepContext.atomId.replace(a)))
      s1 <- s0.flatTraverse { sc =>
              Gen.chooseNum(0, 10).flatMap { n =>
                Gen.listOfN(n, Gen.frequency(19 -> sc.some, 1 -> none[StepContext]))
              }
            }
    } yield s1

  def genVisit(min: Timestamp, vid: Visit.Id): Gen[List[Event]] =
    for {
      c  <- arbitrary[ChargeClass]
      n  <- Gen.chooseNum(0, 10)
      ss <- Gen.listOfN(n, genAtom).map(_.flatten)
      es <- Gen.listOfN(ss.size, Gen.chooseNum(0L, 1000L))
               .map(_.scanLeft(min)((t, inc) => t.plusMillisOption(inc).get))
               .map(_.drop(1).zip(ss).map { case (t, s) => Event(t, Context(vid, c, s)) })
    } yield es

  given Arbitrary[TimeAccountingState] =
    Arbitrary {
      for {
        v <- arbitrary[Visit.Id]
        c <- arbitrary[ChargeClass]
        t <- genVisit(Timestamp.Min, v).map(events => TimeAccountingState.unsafeFromEvents(c, v, events))
      } yield t
    }

  given Cogen[TimeAccountingState] =
    Cogen[List[(TimestampInterval, Context)]].contramap(_.toList)
}

object ArbTimeAccountingState extends ArbTimeAccountingState
