// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import io.circe.syntax.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.odb.graphql.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.effect.PropF
import org.scalacheck.effect.PropF.forAllF

class OffsetGeneratorServiceSuite extends OdbSuite with munit.ScalaCheckSuite:

  val pi: StandardUser     = TestUsers.Standard.pi(nextId, nextId)
  val service: ServiceUser = TestUsers.service(nextId)

  lazy val validUsers: List[User] =
    List(pi, service)

  def setup(
    obj: String,
    sky: String
  ): IO[Observation.Id] =
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        query(
          pi,
          s"""
            mutation {
              createObservation(input: {
                programId: ${pid.asJson}
                SET: {
                  targetEnvironment: {
                    asterism: [${tid.asJson}]
                  }
                  scienceRequirements: {
                    exposureTimeMode: {
                      signalToNoise: {
                        value: 10.0
                        at: { nanometers: 500.0 }
                      }
                    }
                    imaging: {
                      minimumFov: { arcseconds: 100 }
                      narrowFilters: false
                      broadFilters: false
                      combinedFilters: true
                    }
                  }
                  observingMode: {
                    gmosSouthImaging: {
                      filters: [
                        { filter: G_PRIME },
                        { filter: R_PRIME }
                      ]
                      objectOffsetGenerator: {
                        $obj
                      }
                      skyOffsetGenerator: {
                        $sky
                      }
                    }
                  }
                }
              }) { observation { id } }
            }
          """
        ).map: json =>
          json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]

  private def calcObjectOffsets(
    obj:    String,
    counts: Map[GmosNorthFilter, (Int, Long)]
  ): IO[Map[GmosNorthFilter, List[TelescopeConfig]]] =
   setup(obj, "").flatMap: oid =>
     withServices(pi): services =>
       services
         .offsetGeneratorService
         .generateGmosNorthImagingObject(oid, counts)

  private def telescopeConfig(
    p: Int,
    q: Int,
    g: StepGuideState = StepGuideState.Enabled
  ): TelescopeConfig =
    TelescopeConfig(
      Offset.signedMicroarcseconds.reverseGet((p, q)),
      g
    )

  private def row(q: Int)(ps: Int*): List[TelescopeConfig] =
    ps.toList.map(p => telescopeConfig(p, q))

  test("none"):
    val counts = Map(
      GmosNorthFilter.GPrime -> (3, 0L),
      GmosNorthFilter.RPrime -> (1, 0L)
    )

    val result = calcObjectOffsets("", counts)

    val expected = Map(GmosNorthFilter.GPrime -> Nil, GmosNorthFilter.RPrime -> Nil)

    assertIO(result, expected)

  test("enumerated"):
    val counts = Map(
      GmosNorthFilter.GPrime -> (3, 0L),
      GmosNorthFilter.RPrime -> (1, 0L)
    )

    val result = calcObjectOffsets(
      s"""
        enumerated: {
          values: [
            {
              offset: {
                p: { arcseconds: 10.0 }
                q: { arcseconds: 11.0 }
              }
              guiding: ENABLED
            },
            {
              offset: {
                p: { arcseconds: 12.0 }
                q: { arcseconds: 13.0 }
              }
              guiding: ENABLED
            }
          ]
        }
      """,
      counts
    )

    val expected = Map(
      GmosNorthFilter.GPrime -> List(
        telescopeConfig(10_000_000, 11_000_000),
        telescopeConfig(12_000_000, 13_000_000),
        telescopeConfig(10_000_000, 11_000_000) // repeat
      ),
      GmosNorthFilter.RPrime -> List(
        telescopeConfig(10_000_000, 11_000_000)
      )
    )

    assertIO(result, expected)

  test("grid"):
    val counts = Map(
      GmosNorthFilter.GPrime -> ( 8, 0L), // sqrt( 8 * 2) = 4 columns at 2:1 aspect ratio
      GmosNorthFilter.RPrime -> (18, 0L)  // sqrt(18 * 2) = 6 columns at 2:1 aspect ratio
    )

    // 2:1 aspect ratio
    val result = calcObjectOffsets(
      s"""
        grid: {
          cornerA: {
            p: { arcseconds: 10.0 }
            q: { arcseconds: 10.0 }
          }
          cornerB: {
            p: { arcseconds: 30.0 }
            q: { arcseconds: 20.0 }
          }
        }
      """,
      counts
    )

    val expected = Map(
      GmosNorthFilter.GPrime -> ( // 4 x 2
        row(20_000_000)(10_000_000, 16_666_666, 23_333_333, 30_000_000) ++
        row(10_000_000)(10_000_000, 16_666_666, 23_333_333, 30_000_000)
      ),
      GmosNorthFilter.RPrime -> ( // 6 x 3
        row(20_000_000)(10_000_000, 14_000_000, 18_000_000, 22_000_000, 26_000_000, 30_000_000) ++
        row(15_000_000)(10_000_000, 14_000_000, 18_000_000, 22_000_000, 26_000_000, 30_000_000) ++
        row(10_000_000)(10_000_000, 14_000_000, 18_000_000, 22_000_000, 26_000_000, 30_000_000)
      )
    )

    assertIO(result, expected)

  case class CountAndSeed(
    count: Int,
    seed:  Long
  )

  given Arbitrary[CountAndSeed] =
    Arbitrary:
      for
        c <- Gen.choose(0, 10)
        s <- arbitrary[Long]
      yield CountAndSeed(c, s)

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(20)     // default = 100
      .withMaxDiscardRatio(5)

  private def randomGeneratorTest(
    input:               String,
    sizeMicroarcseconds: Int
  ): PropF[IO] =
    forAllF: (g: CountAndSeed, r: CountAndSeed) =>
      val counts = Map(
        GmosNorthFilter.GPrime -> (g.count -> g.seed),
        GmosNorthFilter.RPrime -> (r.count -> r.seed)
      )

      calcObjectOffsets(input, counts).map: r =>
        val sizeCorrect =
          r.forall: (f, os) =>
            os.sizeIs == counts(f)._1

        val withinSizeLimit =
          r.values
           .flatten
           .map(_.offset.distance(Offset.Zero).toMicroarcseconds)
           .forall(_ > sizeMicroarcseconds)

        val notRepeating =
          r.values.forall: os =>
            os == os.distinct

        assert:
          sizeCorrect && withinSizeLimit && notRepeating

  test("random"):
    randomGeneratorTest(
      s"""
        random: {
          size: { arcseconds: 20.0 }
        }
      """,
      20_000_000
    )

  test("spiral"):
    randomGeneratorTest(
      s"""
        spiral: {
          size: { arcseconds: 20.0 }
        }
      """,
      20_000_000
    )