// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order.catsKernelOrderingForOrder
import cats.syntax.order.*
import lucuma.core.enums.ChargeClass
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*

import scala.collection.immutable.SortedSet

final class TimeAccountingStateSuite extends ScalaCheckSuite {

  import ArbTimeAccountingState.given

  test("unsorted fails") {
    val v = Visit.Id.fromLong(1L).get
    val c = TimeAccounting.Context(v, ChargeClass.Program, None)
    val s = List(
      TimeAccounting.Event(Timestamp.ofEpochMilli(100).get, c),
      TimeAccounting.Event(Timestamp.ofEpochMilli(50).get,  c)
    )

    intercept[RuntimeException] {
      TimeAccountingState.unsafeFromEvents(ChargeClass.Program, v, s)
    }
  }

  case class TestData(
    state:    TimeAccountingState,
    interval: TimestampInterval,
    atoms:    Set[Atom.Id]
  ) {
    def timestamp: Timestamp = interval.start
  }

  given Arbitrary[TestData] =
    Arbitrary {
      for {
        t <- arbitrary[TimeAccountingState]
        entries = t.toList
        min     = entries.headOption.flatMap((interval, _) => interval.start.plusMillisOption(-1000L)).getOrElse(Timestamp.Min)
        max     = entries.lastOption.flatMap((interval, _) => interval.end.plusMillisOption(1000L)).getOrElse(Timestamp.Max)
        s <- Gen.choose(min.toEpochMilli, max.toEpochMilli).map(ms => Timestamp.ofEpochMilli(ms).get)
        e <- Gen.choose(s.toEpochMilli, max.toEpochMilli).map(ms => Timestamp.ofEpochMilli(ms).get)
        atoms   = t.allAtoms.toList
        n <- Gen.choose(0, atoms.length)
        a <- Gen.pick(n, atoms).map(_.toSet)
      } yield TestData(t, TimestampInterval.between(s, e), a)
    }

  extension (t: Timestamp) {
    def prev: Option[Timestamp] =
      t.plusMicrosOption(-1)

    def next: Option[Timestamp] =
      t.plusMicrosOption(1)
  }

  test("`until` ends before or at timestamp") {
    forAll { (d: TestData) =>
      d.state.until(d.timestamp).toMap.lastOption.forall { (interval, _) =>
        interval.end <= d.timestamp
      }
    }
  }

  test("`until` Timestamp.Min === Empty") {
    forAll { (tas: TimeAccountingState) =>
      assertEquals(tas.until(Timestamp.Min), TimeAccountingState.Empty)
    }
  }

  test("`until` Timestamp.Max === state") {
    forAll { (tas: TimeAccountingState) =>
      assertEquals(tas.until(Timestamp.Max), tas)
    }
  }

  test("`until` keeps context when last interval is split") {
    forAll { (d: TestData) =>
      d.timestamp.prev.foreach { p =>
        // The last interval may be split, so just check that the
        // context is the same at the previous instant.
        assertEquals(
          d.state.contextAt(p),
          d.state.until(d.timestamp).contextAt(p)
        )
      }
    }
  }

  test("`from` begins after or at timestamp") {
    forAll { (d: TestData) =>
      d.state.from(d.timestamp).toMap.headOption.forall { (interval, _) =>
        interval.start >= d.timestamp
      }
    }
  }

  test("`from` Timestamp.Min === state") {
    forAll { (tas: TimeAccountingState) =>
      assertEquals(tas.from(Timestamp.Min), tas)
    }
  }

  test("`from` Timestamp.Max === Empty") {
    forAll { (tas: TimeAccountingState) =>
      assertEquals(tas.from(Timestamp.Max), TimeAccountingState.Empty)
    }
  }

  test("`from` keeps context when first interval is split") {
    forAll { (d: TestData) =>
      d.timestamp.next.foreach { n =>
        // The first interval may be split, so just check that the
        // context is the same at the next instant.
        assertEquals(
          d.state.contextAt(n),
          d.state.from(d.timestamp).contextAt(n)
        )
      }
    }
  }

  test("`until` + `from` = total") {
    forAll { (d: TestData) =>
      assertEquals(
        d.state.until(d.timestamp).charge +| d.state.from(d.timestamp).charge,
        d.state.charge
      )
    }
  }

  test("`between` TimestampInterval.All is identity") {
    forAll { (tas: TimeAccountingState) =>
      assertEquals(tas.between(TimestampInterval.All), tas)
    }
  }

  test("`between` TimestampInterval.empty is Empty") {
    forAll { (d: TestData) =>
      assertEquals(d.state.between(TimestampInterval.empty(d.timestamp)), TimeAccountingState.Empty)
    }
  }

  test("`excluding` TimestampInterval.All is empty") {
    forAll { (tas: TimeAccountingState) =>
      assertEquals(tas.excluding(TimestampInterval.All), TimeAccountingState.Empty)
    }
  }

  test("`excluding` TimestampInterval.empty is identity") {
    forAll { (d: TestData) =>
      assertEquals(d.state.excluding(TimestampInterval.empty(d.timestamp)), d.state)
    }
  }

  // The intervals may be split so compare the resultinug charges.
  test("`between` + `excluding` = total") {
    forAll { (d: TestData) =>
      assertEquals(
        d.state.between(d.interval).charge +| d.state.excluding(d.interval).charge,
        d.state.charge
      )
    }
  }

  test("atomsIntersecting") {
    forAll { (d: TestData) =>
      assertEquals(
        d.state.atomsIntersecting(d.interval),
        d.state.toList.foldLeft(SortedSet.empty[Atom.Id]) { case (s, (interval, ctx)) =>
          ctx.atom.fold(s) { atom =>
            if (interval.intersects(d.interval)) s + atom.atomId else s
          }
        }
      )
    }
  }

  test("entryAt contains timestamp") {
    forAll { (d: TestData) =>
      val s = d.state
      val t = d.timestamp
      assertEquals(
        s.entryAt(t).exists(_._1.contains(t)),
        s.contains(t)
      )
    }
  }

  test("intervalContaining is superset") {
    forAll { (d: TestData) =>
      val s  = d.state
      val i  = d.interval
      val as = s.atomsIntersecting(i)
      val iʹ = s.intervalContaining(as).getOrElse(i)
      as.subsetOf(s.between(iʹ).allAtoms)
    }
  }

  test("partitionOnInterval is (between, excluding)") {
    forAll { (d: TestData) =>
      val (in, out) = d.state.partitionOnInterval(d.interval)

      assertEquals(in,  d.state.between(d.interval))
      assertEquals(out, d.state.excluding(d.interval))
    }

  }

  test("partitionOnAtomBoundary separates on atom boundaries") {
    forAll { (d: TestData) =>
      val (in, out) = d.state.partitionOnAtomBoundary(d.interval)

      val inAtoms  = in.allAtoms
      val outAtoms = out.allAtoms
      assert(inAtoms.intersect(outAtoms).isEmpty)
    }
  }

  test("partitionOnAtomBoundary is complete") {
    forAll { (d: TestData) =>
      val (in, out) = d.state.partitionOnAtomBoundary(d.interval)

      val inAtoms  = in.allAtoms
      val outAtoms = out.allAtoms
      assertEquals(inAtoms.union(outAtoms), d.state.allAtoms)
    }
  }

  test("partitionOnAtom separates on atom boundaries") {
    forAll { (d: TestData) =>
      val a = d.atoms.headOption
      val (in, out) = a.fold((TimeAccountingState.Empty, d.state))(d.state.partitionOnAtom)

      val inAtoms = in.allAtoms
      val outAtoms = out.allAtoms
      assert(inAtoms.intersect(outAtoms).isEmpty)
    }
  }

  test("partitionAtoms is complete") {
    forAll { (d: TestData) =>
      val a = d.atoms.headOption
      val (in, out) = a.fold((TimeAccountingState.Empty, d.state))(d.state.partitionOnAtom)

      val inAtoms  = in.allAtoms
      val outAtoms = out.allAtoms
      assertEquals(inAtoms.union(outAtoms), d.state.allAtoms)
    }
  }
}