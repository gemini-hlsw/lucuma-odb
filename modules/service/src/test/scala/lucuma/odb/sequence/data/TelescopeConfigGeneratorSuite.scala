// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.foldable.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.odb.sequence.data.TelescopeConfigGenerator
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.effect.PropF
import org.scalacheck.effect.PropF.forAllF
import lucuma.core.geom.OffsetGenerator

class TelescopeConfigGeneratorSuite extends CatsEffectSuite with ScalaCheckEffectSuite:

  extension (g: TelescopeConfigGenerator)
    def generate(
      c: Int,
      s: Long = 0L,
      d: StepGuideState = StepGuideState.Enabled
    ): IO[List[TelescopeConfig]] =
      g.generate[IO](NonNegInt.unsafeFrom(c), s, d)

  private def offset(
    p: Int,
    q: Int
  ): Offset =
    Offset.signedMicroarcseconds.reverseGet((p * 1_000_000, q * 1_000_000))

  private def telescopeConfig(
    p: Int,
    q: Int,
    g: StepGuideState = StepGuideState.Enabled
  ): TelescopeConfig =
    TelescopeConfig(offset(p, q), g)

  def runTests(ts: (IO[List[TelescopeConfig]], List[TelescopeConfig])*): IO[Unit] =
    ts.toList.traverse_ { case (result, expected) =>
      assertIO(result, expected)
    }

  test("none"):
    val expected = List.empty[TelescopeConfig]
    runTests(
      TelescopeConfigGenerator.NoGenerator.generate(0) -> expected,
      TelescopeConfigGenerator.NoGenerator.generate(1) -> expected,
      TelescopeConfigGenerator.NoGenerator.generate(2) -> expected
    )

  test("enumerated"):
    val gen = TelescopeConfigGenerator.Enumerated(
      NonEmptyList.of(
        telescopeConfig(10, 11),
        telescopeConfig(12, 13, StepGuideState.Disabled)
      )
    )

    runTests(
      gen.generate(0) -> List.empty[TelescopeConfig],
      gen.generate(1) -> List(telescopeConfig(10, 11)),
      gen.generate(2) -> List(telescopeConfig(10, 11), telescopeConfig(12, 13, StepGuideState.Disabled)),
      gen.generate(3) -> List(telescopeConfig(10, 11), telescopeConfig(12, 13, StepGuideState.Disabled), telescopeConfig(10, 11))
    )

  test("uniform"):
    def row(q: Int)(ps: Int*): List[TelescopeConfig] =
      ps.toList.map: p =>
        TelescopeConfig(Offset.signedMicroarcseconds.reverseGet(p, q), StepGuideState.Enabled)

    // 2:1 aspect ratio
    val gen = TelescopeConfigGenerator.FromOffsetGenerator(OffsetGenerator.Uniform(offset(10, 10), offset(30, 20)))

    // 4 x 2
    val expected8: List[TelescopeConfig] =
      row(20_000_000)(30_000_000, 23_333_334, 16_666_667, 10_000_000) ++
      row(10_000_000)(30_000_000, 23_333_334, 16_666_667, 10_000_000)

    // 6 x 3
    val expected18: List[TelescopeConfig] =
      row(20_000_000)(30_000_000, 26_000_000, 22_000_000, 18_000_000, 14_000_000, 10_000_000) ++
      row(15_000_000)(30_000_000, 26_000_000, 22_000_000, 18_000_000, 14_000_000, 10_000_000) ++
      row(10_000_000)(30_000_000, 26_000_000, 22_000_000, 18_000_000, 14_000_000, 10_000_000)

    runTests(
      gen.generate( 8) -> expected8,
      gen.generate(18) -> expected18
    )

  case class CountAndSeed(
    count: NonNegInt,
    seed:  Long
  )

  given Arbitrary[CountAndSeed] =
    Arbitrary:
      for
        c <- Gen.choose(0, 100)
        s <- arbitrary[Long]
      yield CountAndSeed(NonNegInt.unsafeFrom(c), s)

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(20)     // default = 100
      .withMaxDiscardRatio(5)

  private def randomGeneratorTest(
    gen:            TelescopeConfigGenerator,
    sizeArcseconds: Int
  ): PropF[IO] =
    forAllF: (cs: CountAndSeed) =>
      assertIOBoolean:
        gen.generate[IO](cs.count, cs.seed).map: lst =>
          val sizeCorrect     = lst.sizeIs == (cs.count.value)
          val withinSizeLimit = lst.forall(_.offset.distance(Offset.Zero).toMicroarcseconds < (sizeArcseconds * 1_000_000))
          val notRepeating    = lst == lst.distinctBy(_.offset)
          sizeCorrect && withinSizeLimit && notRepeating

  test("random"):
    randomGeneratorTest(
      TelescopeConfigGenerator.FromOffsetGenerator(OffsetGenerator.Random(Angle.signedMicroarcseconds.reverseGet(20_000_000), Offset.Zero)),
      20
    )


  test("spiral"):
    randomGeneratorTest(
      TelescopeConfigGenerator.FromOffsetGenerator(OffsetGenerator.Spiral(Angle.signedMicroarcseconds.reverseGet(20_000_000), Offset.Zero)),
      20
    )