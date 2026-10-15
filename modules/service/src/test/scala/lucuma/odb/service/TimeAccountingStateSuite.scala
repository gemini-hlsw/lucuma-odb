// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order.catsKernelOrderingForOrder
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.order.*
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SetupKind
import lucuma.core.enums.SlewStage
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.SetupTimeBreakdown
import lucuma.core.model.sequence.Step
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*

import java.util.UUID
import scala.collection.immutable.SortedSet

final class TimeAccountingStateSuite extends ScalaCheckSuite {

  import ArbTimeAccountingState.given

  test("unsorted fails") {
    val v = Visit.Id.fromLong(1L).get
    val c = TimeAccounting.Context(v, ChargeClass.Program, None, slewing = false)
    val s = List(
      TimeAccounting.Event(Timestamp.ofEpochMilli(100).get, c, None),
      TimeAccounting.Event(Timestamp.ofEpochMilli(50).get,  c, None)
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

  // Setup tests.  Times are in seconds from the epoch and all time is charged
  // to the program.

  private val vid: Visit.Id = Visit.Id.fromLong(1L).get

  private def ts(sec: Int): Timestamp =
    Timestamp.ofEpochMilli(sec * 1000L).get

  private def interval(start: Int, end: Int): TimestampInterval =
    TimestampInterval.between(ts(start), ts(end))

  private def secs(n: Int): CategorizedTime =
    CategorizedTime(ChargeClass.Program -> TimeSpan.unsafeFromMicroseconds(n * 1_000_000L))

  private def uuid(n: Int): UUID =
    UUID.fromString(f"00000000-0000-0000-0000-$n%012d")

  private def sequenceEvent(sec: Int): TimeAccounting.Event =
    TimeAccounting.Event(ts(sec), TimeAccounting.Context(vid, ChargeClass.Program, None, slewing = false), None)

  private def slewEvent(sec: Int, stage: SlewStage): TimeAccounting.Event =
    TimeAccounting.Event(ts(sec), TimeAccounting.Context(vid, ChargeClass.Program, None, slewing = false), stage.some)

  // A one-step atom executing from `start` to `end`.
  private def atom(n: Int, sequenceType: SequenceType, start: Int, end: Int): List[TimeAccounting.Event] = {
    val ac  = TimeAccounting.AtomContext(Atom.Id.fromUuid(uuid(n)), Step.Id.fromUuid(uuid(n)).some, sequenceType)
    val ctx = TimeAccounting.Context(vid, ChargeClass.Program, ac.some, slewing = false)
    List(TimeAccounting.Event(ts(start), ctx, None), TimeAccounting.Event(ts(end), ctx, None))
  }

  private def acq(n: Int, start: Int, end: Int): List[TimeAccounting.Event] =
    atom(n, SequenceType.Acquisition, start, end)

  private def sci(n: Int, start: Int, end: Int): List[TimeAccounting.Event] =
    atom(n, SequenceType.Science, start, end)

  private def state(events: List[TimeAccounting.Event]): TimeAccountingState =
    TimeAccountingState.unsafeFromEvents(ChargeClass.Program, vid, events)

  private def setups(events: List[TimeAccounting.Event]): List[TimeAccounting.Setup] = {
    val s = state(events)
    s.setups(s)
  }

  private def setup(kind: SetupKind, start: Int, end: Int, slew: Int, acquisition: Int, other: Int): TimeAccounting.Setup =
    TimeAccounting.Setup(kind, interval(start, end), SetupTimeBreakdown(secs(slew), secs(acquisition), secs(other)))

  // slew, acquisition, science, reacquisition, science
  private val typical: List[TimeAccounting.Event] =
    List(slewEvent(0, SlewStage.StartSlew), slewEvent(10, SlewStage.EndSlew)) ++
      acq(1, 15, 25) ++
      sci(2, 40, 60) ++
      acq(3, 70, 80) ++
      sci(4, 90, 100)

  test("setups: slew, acquisition, science, reacquisition") {
    assertEquals(
      setups(typical),
      List(
        setup(SetupKind.Full,          0, 40, 10, 10, 20),
        setup(SetupKind.Reacquisition, 60, 90,  0, 10, 20)
      )
    )
  }

  test("setups: tracking slews does not change the charge") {
    assertEquals(state(typical).charge, secs(100))
    assertEquals(state(typical).charge, state(typical.map(_.copy(slewStage = None))).charge)
  }

  test("setups: a slew lasts until the next atom event when there is no slew end") {
    assertEquals(
      setups(slewEvent(0, SlewStage.StartSlew) :: acq(1, 15, 25) ++ sci(2, 40, 60)),
      List(setup(SetupKind.Full, 0, 40, 15, 10, 15))
    )
  }

  test("setups: sequence events do not end a slew") {
    assertEquals(
      setups(List(slewEvent(0, SlewStage.StartSlew), sequenceEvent(5), slewEvent(10, SlewStage.EndSlew)) ++ acq(1, 15, 25) ++ sci(2, 40, 60)),
      List(setup(SetupKind.Full, 0, 40, 10, 10, 20))
    )
  }

  test("setups: an acquisition with no slew that precedes science is a full setup") {
    assertEquals(
      setups(acq(1, 0, 10) ++ sci(2, 20, 30)),
      List(setup(SetupKind.Full, 0, 20, 0, 10, 10))
    )
  }

  test("setups: a slew after science starts a new full setup") {
    assertEquals(
      setups(sci(1, 0, 10) ++ List(slewEvent(20, SlewStage.StartSlew), slewEvent(30, SlewStage.EndSlew)) ++ acq(2, 35, 45) ++ sci(3, 50, 60)),
      List(setup(SetupKind.Full, 10, 50, 10, 10, 20))
    )
  }

  test("setups: slewing without an acquisition (e.g., imaging) is a setup") {
    assertEquals(
      setups(List(slewEvent(0, SlewStage.StartSlew), slewEvent(10, SlewStage.EndSlew)) ++ sci(1, 20, 30)),
      List(setup(SetupKind.Full, 0, 20, 10, 0, 10))
    )
  }

  test("setups: idle time without a slew or acquisition is not a setup") {
    assertEquals(setups(sequenceEvent(0) :: sci(1, 10, 20) ++ sci(2, 30, 40)), Nil)
  }

  test("setups: an acquisition at the end of a visit is a setup") {
    assertEquals(
      setups(List(slewEvent(0, SlewStage.StartSlew), slewEvent(10, SlewStage.EndSlew)) ++ acq(1, 15, 25)),
      List(setup(SetupKind.Full, 0, 25, 10, 10, 5))
    )
  }

  test("setups: are charged according to the discounted state") {
    val s = state(typical)
    // e.g., the first 5 seconds of the slew fall before twilight
    assertEquals(
      s.setups(s.from(ts(5))),
      List(
        setup(SetupKind.Full,          0, 40, 5, 10, 20),
        setup(SetupKind.Reacquisition, 60, 90, 0, 10, 20)
      )
    )
  }

  test("setups: never charge more than the state") {
    forAll { (s: TimeAccountingState) =>
      val total = s.setups(s).foldMap(_.charge.total)
      assert(total.sum <= s.charge.sum)
    }
  }

  test("setups: are ordered and disjoint") {
    forAll { (s: TimeAccountingState) =>
      val is = s.setups(s).map(_.interval)
      is.zip(is.drop(1)).foreach { (a, b) => assert(a.end <= b.start) }
    }
  }
}
