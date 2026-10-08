// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.syntax

import cats.data.NonEmptyList
import cats.syntax.all.*
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.StepType
import lucuma.core.model.ConstraintSet
import lucuma.core.model.arb.ArbConstraintSet.given
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepDigest
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.arb.ArbAtom.given
import lucuma.core.model.sequence.arb.ArbStep.given
import lucuma.core.model.sequence.exposure.ExposureTimeViolation
import lucuma.core.model.sequence.exposure.PendingExposureRules
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.model.sequence.gmos.arb.ArbDynamicConfig.given
import lucuma.core.model.sequence.gmos.arb.ArbGmosGratingConfig.given
import lucuma.core.util.TimeSpan
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

import scala.collection.immutable.SortedSet

import sequencedigest.*

// The parts of the digest that don't depend on exposure rules, moved here
// from lucuma-core along with `add`, followed by the exposure time
// violations.  The rules themselves are tested in lucuma-core.
class SequenceDigestOpsSuite extends ScalaCheckSuite:

  private def sample[A](using a: Arbitrary[A]): A =
    Iterator.continually(a.arbitrary.sample).flatten.next()

  private val ctx: PendingExposureRules.Context =
    PendingExposureRules.Context(sample[ConstraintSet])

  // Steps without an instrument have no exposure rules.
  given PendingExposureRules[Unit] = (_, _) => Nil

  property("preserves ordering of atom steps"):
    forAll: (a: Atom[Unit]) =>
      val sd     = SequenceDigest.Zero.add(a, ctx)
      val result = a.steps.toList.map(s => TelescopeConfig(s.telescopeConfig.offset, s.telescopeConfig.guiding))
      sd.telescopeConfigs === SortedSet.from(result)

  private def matches(sd: StepDigest, steps: List[Step[Unit]]): Boolean =
    (sd.count.value === steps.size) && (sd.time === steps.foldMap(_.timeEstimate))

  property("buckets steps by type"):
    forAll: (a: Atom[Unit]) =>
      val sd     = SequenceDigest.Zero.add(a, ctx)
      val steps  = a.steps.toList
      val biases = steps.filter(_.stepConfig.stepType === StepType.Bias)
      val darks  = steps.filter(_.stepConfig.stepType === StepType.Dark)
      val arcs   = steps.filter(_.stepConfig.isArc)
      val flats  = steps.filter(s => s.stepConfig.usesGcalUnit && !s.stepConfig.isArc)
      val other  = steps.filter(_.stepConfig.stepType === StepType.Science)
      matches(sd.steps.biases, biases) &&
      matches(sd.steps.darks, darks) &&
      matches(sd.steps.arcs, arcs) &&
      matches(sd.steps.flats, flats) &&
      matches(sd.steps.observing, other)

  property("buckets sum to the time estimate"):
    forAll: (a: Atom[Unit]) =>
      val sd = SequenceDigest.Zero.add(a, ctx)
      sd.steps.time === sd.timeEstimate

  property("counts an atom as one gcal set when it has any GCAL step"):
    forAll: (as: List[Atom[Unit]]) =>
      val sd       = as.foldLeft(SequenceDigest.Zero)(_.add(_, ctx))
      val expected = as.count(_.steps.exists(_.stepConfig.usesGcalUnit))
      sd.gcalSets.value === expected

  // A GMOS North long slit step, so only the 1 s minimum, whole seconds and
  // the 1200 s cosmic ray warning apply.
  private def step(seconds: BigDecimal, observeClass: ObserveClass = ObserveClass.Science): Step[DynamicConfig.GmosNorth] =
    val d = sample[DynamicConfig.GmosNorth].copy(
      exposure      = TimeSpan.unsafeFromMicroseconds((seconds * 1_000_000).toLongExact),
      gratingConfig = sample[GmosGratingConfig.North].some
    )
    sample[Step[DynamicConfig.GmosNorth]].copy(instrumentConfig = d, stepConfig = StepConfig.Science, observeClass = observeClass)

  private def atom(steps: Step[DynamicConfig.GmosNorth]*): Atom[DynamicConfig.GmosNorth] =
    sample[Atom[DynamicConfig.GmosNorth]].copy(steps = NonEmptyList.fromListUnsafe(steps.toList))

  private val BelowMin   = "Exposure times for GMOS North must be at least 1 s."
  private val Fractional = "Exposure times for GMOS North must be a whole number of seconds."
  private val CosmicRays = "Exposure times above 1200 s are not recommended for GMOS North due to cosmic ray contamination."

  private def error(d: String): ExposureTimeViolation   = ExposureTimeViolation(ExposureTimeViolation.Severity.Error, d)
  private def warning(d: String): ExposureTimeViolation = ExposureTimeViolation(ExposureTimeViolation.Severity.Warning, d)

  test("a rule violated by many steps is reported once, errors first"):
    val sd =
      SequenceDigest.Zero
        .add(atom(step(1300), step(20), step(1500)), ctx)
        .add(atom(step(0.5), step(0.25)), ctx)
    assertEquals(sd.exposureTimeViolations.toList, List(error(Fractional), error(BelowMin), warning(CosmicRays)))

  test("acquisition steps are checked for errors alone"):
    val sd = SequenceDigest.Zero.add(atom(step(0, ObserveClass.Acquisition), step(1500, ObserveClass.Acquisition)), ctx)
    assertEquals(sd.exposureTimeViolations.toList, List(error(BelowMin)))

  test("the digest's violations are core's for its steps"):
    val sd = SequenceDigest.Zero.add(atom(step(1500)), ctx)
    assertEquals(sd.exposureTimeViolations.toList, ExposureTimeViolation.check(step(1500), ctx))
