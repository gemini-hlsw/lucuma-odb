// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.Site
import lucuma.core.math.Wavelength
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.InstrumentMode

class executionSciGmosNorthImaging extends ExecutionTestSupportForGmos:

  val Time120x06 = IntegrationTime(120.secondTimeSpan, PosInt.unsafeFrom( 6))
  val Time060x12 = IntegrationTime( 60.secondTimeSpan, PosInt.unsafeFrom(12))
  val Time030x30 = IntegrationTime( 30.secondTimeSpan, PosInt.unsafeFrom(30))

  // 120s x 6 in g, 60s x 12 in i, and 30s x 30 in Y
  override def fakeItcImagingResultFor(input: ImagingInput): Option[IntegrationTime] =
    input.mode match
      case InstrumentMode.GmosNorthImaging(f, _) =>
        f match
          case GmosNorthFilter.GPrime => Time120x06.some
          case GmosNorthFilter.IPrime => Time060x12.some
          case GmosNorthFilter.Y      => Time030x30.some
          case _                      => none

      case InstrumentMode.GmosSouthImaging(f, _) =>
        f match
          case GmosSouthFilter.GPrime => Time120x06.some
          case GmosSouthFilter.IPrime => Time060x12.some
          case GmosSouthFilter.Y      => Time030x30.some
          case _                      => none

      case _                                     =>
        none

  private def createObservation(
    pid:  Program.Id,
    tid:  Target.Id,
    site: Site
  )(
    mode: String
  ): IO[Observation.Id] =
    val req = site match
      case Site.GN => scienceRequirementsObject(ObservingModeType.GmosNorthImaging)
      case Site.GS => scienceRequirementsObject(ObservingModeType.GmosSouthImaging)

    query(
      user  = pi,
      query =
        s"""
          mutation {
            createObservation(input: {
            programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: ${List(tid).asJson}
                }
                scienceRequirements: $req
                observingMode: $mode
                constraintSet: {
                  imageQuality: ${ImageQuality.Preset.PointEight.tag.toScreamingSnakeCase}
                }
              }
            }) {
              observation {
                id
              }
            }
          }
        """
    ).map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])

  def enumerated(os: (Int, Int)*): String =
    val values = os.map: (p, q) =>
      s"""
        {
          offset: {
            p: { arcseconds: $p }
            q: { arcseconds: $q }
          }
        }
      """
    s"""
      enumerated: {
        values: ${values.mkString("[", ",", "]")}
      }
    """

  case class Step[L: Enumerated](
    filter:  L,
    time:    IntegrationTime,
    offset:  (Int, Int),
    obsClass: ObserveClass
  ):
    def withOffset(p: Int, q: Int): Step[L] =
      copy(offset = (p, q))

    def withClass(c: ObserveClass): Step[L] =
      copy(obsClass = c)

    def toJson(wavelength: L => Wavelength): Json =
      json"""
        {
          "instrumentConfig" : {
            "exposure" : {
              "seconds" : ${Json.fromBigDecimal(time.exposureTime.toSeconds)}
            },
            "readout" : {
              "xBin" : "TWO",
              "yBin" : "TWO"
            },
            "roi" : "FULL_FRAME",
            "gratingConfig" : null,
            "filter" : ${Enumerated[L].tag(filter).toScreamingSnakeCase.asJson},
            "fpu" : null,
            "centralWavelength" : {
              "nanometers" : ${Json.fromBigDecimal(wavelength(filter).toNanometers.value.value)}
            }
          },
          "stepConfig" : {
            "stepType" : "SCIENCE"
          },
          "telescopeConfig" : {
            "offset" : {
              "p" : {
                "arcseconds" : ${Json.fromBigDecimal(BigDecimal(s"${offset._1}.000000"))}
              },
              "q" : {
                "arcseconds" : ${Json.fromBigDecimal(BigDecimal(s"${offset._1}.000000"))}
              }
            },
            "guiding" : "ENABLED"
          },
          "observeClass" : ${obsClass.tag.toScreamingSnakeCase},
          "breakpoint" : "DISABLED"
        }
      """
  
  object Step:
    def apply[L: Enumerated](filter: L, time: IntegrationTime): Step[L] =
      Step(filter, time, (0, 0), ObserveClass.Science)

  def atom[L](steps: Step[L]*)(w: L => Wavelength): Json =
    json"""{
      "description" : null,
      "observeClass" : "SCIENCE",
      "steps" : ${steps.map(_.toJson(w)).asJson}
    }"""

  def gnAtom(steps: Step[GmosNorthFilter]*): Json =
    atom[GmosNorthFilter](steps*)(_.wavelength)

  def expectedResult(
    atoms: List[Json]
  ): Json =
    Json.obj(
      "executionConfig" -> Json.obj(
        "gmosNorth" -> Json.obj(
          "science" -> Json.obj(
            "nextAtom" -> atoms.head,
            "possibleFuture" -> atoms.tail.asJson,
            "hasMore" -> false.asJson
          )
        )
      )
    )

  test("grouped, no offsets, no sky (INCREASING)"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservation(p, t, Site.GN):
          s"""{
            gmosNorthImaging: {
              variant: {
                grouped: { skyCount: 0 }
              }
              filters: [
                { filter: G_PRIME },
                { filter: I_PRIME },
                { filter: Y       }
              ]
            }
          }"""
      yield o

    val json: List[Json] = List(
      List.fill( 6)(gnAtom(Step(GmosNorthFilter.GPrime, Time120x06))) ++
      List.fill(12)(gnAtom(Step(GmosNorthFilter.IPrime, Time060x12))) ++
      List.fill(30)(gnAtom(Step(GmosNorthFilter.Y,      Time030x30)))
    ).flatten

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid, 100.some),
        expected = expectedResult(json).asRight
      )

  test("grouped, no offsets, no sky (DECREASING)"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservation(p, t, Site.GN):
          s"""{
            gmosNorthImaging: {
              variant: {
                grouped: {
                  order: DECREASING
                  skyCount: 0
                }
              }
              filters: [
                { filter: G_PRIME },
                { filter: I_PRIME },
                { filter: Y       }
              ]
            }
          }"""
      yield o

    val json: List[Json] = List(
      List.fill(30)(gnAtom(Step(GmosNorthFilter.Y,      Time030x30))) ++
      List.fill(12)(gnAtom(Step(GmosNorthFilter.IPrime, Time060x12))) ++
      List.fill( 6)(gnAtom(Step(GmosNorthFilter.GPrime, Time120x06)))
    ).flatten

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid, 100.some),
        expected = expectedResult(json).asRight
      )

  test("grouped, offsets, no sky"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservation(p, t, Site.GN):
          s"""{
            gmosNorthImaging: {
              variant: {
                grouped: {
                  offsets: {
                    ${enumerated((0 until 12).toList.fproduct(identity) *)}
                  }
                  skyCount: 0
                }
              }
              filters: [
                { filter: G_PRIME },
                { filter: I_PRIME },
                { filter: Y       }
              ]
            }
          }"""
      yield o

    val g = (0 until  6).toList.map: n =>
      gnAtom(Step(GmosNorthFilter.GPrime, Time120x06).withOffset(n, n))
    val i = (0 until 12).toList.map: n =>
      gnAtom(Step(GmosNorthFilter.IPrime, Time060x12).withOffset(n, n))
    val y = (0 until 12).toList.map: n =>
      gnAtom(Step(GmosNorthFilter.Y,      Time030x30).withOffset(n, n))

    val json: List[Json] = g ++ i ++ y ++ y ++ y.take(6)

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid, 100.some),
        expected = expectedResult(json).asRight
      )

  test("grouped, no offsets, sky"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservation(p, t, Site.GN):
          s"""{
            gmosNorthImaging: {
              variant: {
                grouped: {
                  skyCount: 2
                  skyOffsets: {
                    ${enumerated((0 until 2).toList.fproduct(identity) *)}
                  }
                }
              }
              filters: [
                { filter: G_PRIME },
                { filter: I_PRIME },
                { filter: Y       }
              ]
            }
          }"""
      yield o

    def mkAtom(f: GmosNorthFilter, t: IntegrationTime): Json =
      gnAtom(
        (
          Step(f, t).withOffset(0, 0).withClass(ObserveClass.NightCal) ::
          Step(f, t).withOffset(1, 1).withClass(ObserveClass.NightCal) ::
          List.fill(t.exposureCount.value)(Step(f, t)).appendedAll:
            List(
              Step(f, t).withOffset(0, 0).withClass(ObserveClass.NightCal),
              Step(f, t).withOffset(1, 1).withClass(ObserveClass.NightCal)
            )
        )*
      )

    val g = mkAtom(GmosNorthFilter.GPrime, Time120x06)
    val i = mkAtom(GmosNorthFilter.IPrime, Time060x12)
    val y = mkAtom(GmosNorthFilter.Y,      Time030x30)

    val json: List[Json] = List(g, i, y)

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid, 100.some),
        expected = expectedResult(json).asRight
      )

  test("interleaved, no offsets, no sky"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservation(p, t, Site.GN):
          s"""{
            gmosNorthImaging: {
              variant: {
                interleaved: { skyCount: 0 }
              }
              filters: [
                { filter: G_PRIME },
                { filter: I_PRIME },
                { filter: Y       }
              ]
            }
          }"""
      yield o

    val oneGroup: List[Json] = List(
      gnAtom(Step(GmosNorthFilter.GPrime, Time120x06)),
      gnAtom(Step(GmosNorthFilter.IPrime, Time060x12)),
      gnAtom(Step(GmosNorthFilter.IPrime, Time060x12)),
      gnAtom(Step(GmosNorthFilter.Y,      Time030x30)),
      gnAtom(Step(GmosNorthFilter.Y,      Time030x30)),
      gnAtom(Step(GmosNorthFilter.Y,      Time030x30)),
      gnAtom(Step(GmosNorthFilter.Y,      Time030x30)),
      gnAtom(Step(GmosNorthFilter.Y,      Time030x30))
    )

    val json: List[Json] = List.fill(6)(oneGroup).flatten

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid, 100.some),
        expected = expectedResult(json).asRight
      )

  test("interleaved, offsets, no sky"):
    val mode =
      s"""{
        gmosNorthImaging: {
          variant: {
            interleaved: {
              offsets: {
                ${enumerated((0 until 48).toList.fproduct(identity) *)}
              }
              skyCount: 0
            }
          }
          filters: [
            { filter: G_PRIME },
            { filter: I_PRIME },
            { filter: Y       }
          ]
        }
      }"""

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservation(p, t, Site.GN)(mode)
      yield o

    def oneGroup(i: Int): List[Json] = List(
      gnAtom(Step(GmosNorthFilter.GPrime, Time120x06).withOffset(i+0, i+0)),
      gnAtom(Step(GmosNorthFilter.IPrime, Time060x12).withOffset(i+1, i+1)),
      gnAtom(Step(GmosNorthFilter.IPrime, Time060x12).withOffset(i+2, i+2)),
      gnAtom(Step(GmosNorthFilter.Y,      Time030x30).withOffset(i+3, i+3)),
      gnAtom(Step(GmosNorthFilter.Y,      Time030x30).withOffset(i+4, i+4)),
      gnAtom(Step(GmosNorthFilter.Y,      Time030x30).withOffset(i+5, i+5)),
      gnAtom(Step(GmosNorthFilter.Y,      Time030x30).withOffset(i+6, i+6)),
      gnAtom(Step(GmosNorthFilter.Y,      Time030x30).withOffset(i+7, i+7))
    )

    val json: List[Json] = (0 until 6).toList.flatMap: i =>
      oneGroup(i * 8)

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid, 100.some),
        expected = expectedResult(json).asRight
      )

  test("interleaved, no offsets, sky"):
    val mode =
      s"""{
        gmosNorthImaging: {
          variant: {
            interleaved: {
              skyCount: 2
              skyOffsets: {
                ${enumerated((100, 100), (200, 200))}
              }
            }
          }
          filters: [
            { filter: G_PRIME },
            { filter: I_PRIME }
          ]
        }
      }"""

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservation(p, t, Site.GN)(mode)
      yield o

    val oneGroup: Json =
      gnAtom(
        Step(GmosNorthFilter.GPrime, Time120x06).withOffset(100, 100).withClass(ObserveClass.NightCal),
        Step(GmosNorthFilter.GPrime, Time120x06).withOffset(200, 200).withClass(ObserveClass.NightCal),
        Step(GmosNorthFilter.IPrime, Time060x12).withOffset(100, 100).withClass(ObserveClass.NightCal),
        Step(GmosNorthFilter.IPrime, Time060x12).withOffset(200, 200).withClass(ObserveClass.NightCal),
        Step(GmosNorthFilter.GPrime, Time120x06),
        Step(GmosNorthFilter.IPrime, Time060x12),
        Step(GmosNorthFilter.IPrime, Time060x12),
        Step(GmosNorthFilter.IPrime, Time060x12).withOffset(200, 200).withClass(ObserveClass.NightCal),
        Step(GmosNorthFilter.IPrime, Time060x12).withOffset(100, 100).withClass(ObserveClass.NightCal),
        Step(GmosNorthFilter.GPrime, Time120x06).withOffset(200, 200).withClass(ObserveClass.NightCal),
        Step(GmosNorthFilter.GPrime, Time120x06).withOffset(100, 100).withClass(ObserveClass.NightCal),
      )

    val json: List[Json] = List.fill(6)(oneGroup)

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid, 100.some),
        expected = expectedResult(json).asRight
      )

  test("preImaging"):
    val mode =
      s"""{
        gmosNorthImaging: {
          variant: {
            preImaging: {
              offset1: {
                p: { arcseconds: 1 }
                q: { arcseconds: 1 }
              }
              offset2: {
                p: { arcseconds: 2 }
                q: { arcseconds: 2 }
              }
              offset3: {
                p: { arcseconds: 3 }
                q: { arcseconds: 3 }
              }
              offset4: {
                p: { arcseconds: 4 }
                q: { arcseconds: 4 }
              }
            }
          }
          filters: [
            { filter: G_PRIME },
            { filter: I_PRIME }
          ]
        }
      }"""

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservation(p, t, Site.GN)(mode)
      yield o

    // Pre-imaging would usually be done with a TimeAndCount mode with exactly
    // 4 exposures, but it should work with the other modes anyway.

    val g: List[Json] = List(
      gnAtom(Step(GmosNorthFilter.GPrime, Time120x06).withOffset(1, 1)),
      gnAtom(Step(GmosNorthFilter.GPrime, Time120x06).withOffset(2, 2)),
      gnAtom(Step(GmosNorthFilter.GPrime, Time120x06).withOffset(3, 3)),
      gnAtom(Step(GmosNorthFilter.GPrime, Time120x06).withOffset(4, 4)),
      gnAtom(Step(GmosNorthFilter.GPrime, Time120x06).withOffset(1, 1)),
      gnAtom(Step(GmosNorthFilter.GPrime, Time120x06).withOffset(2, 2))
    )

    val i: List[Json] = List(
      gnAtom(Step(GmosNorthFilter.IPrime, Time060x12).withOffset(1, 1)),
      gnAtom(Step(GmosNorthFilter.IPrime, Time060x12).withOffset(2, 2)),
      gnAtom(Step(GmosNorthFilter.IPrime, Time060x12).withOffset(3, 3)),
      gnAtom(Step(GmosNorthFilter.IPrime, Time060x12).withOffset(4, 4)),
      gnAtom(Step(GmosNorthFilter.IPrime, Time060x12).withOffset(1, 1)),
      gnAtom(Step(GmosNorthFilter.IPrime, Time060x12).withOffset(2, 2)),
      gnAtom(Step(GmosNorthFilter.IPrime, Time060x12).withOffset(3, 3)),
      gnAtom(Step(GmosNorthFilter.IPrime, Time060x12).withOffset(4, 4)),
      gnAtom(Step(GmosNorthFilter.IPrime, Time060x12).withOffset(1, 1)),
      gnAtom(Step(GmosNorthFilter.IPrime, Time060x12).withOffset(2, 2)),
      gnAtom(Step(GmosNorthFilter.IPrime, Time060x12).withOffset(3, 3)),
      gnAtom(Step(GmosNorthFilter.IPrime, Time060x12).withOffset(4, 4))
    )

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid, 100.some),
        expected = expectedResult(g ++ i).asRight
      )

  test("digest"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservation(p, t, Site.GN):
          s"""{
            gmosNorthImaging: {
              variant: {
                grouped: {
                  offsets: {
                    ${enumerated((0 until 4).toList.fproduct(identity) *)}
                  }
                  skyCount: 0
                }
              }
              filters: [
                { filter: G_PRIME },
                { filter: I_PRIME },
                { filter: Y       }
              ]
            }
          }"""
        _ <- runObscalcUpdate(p, o)
      yield o

    val expected: Json =
      json"""
        {
          "observation" : {
            "execution" : {
              "digest" : {
                "state" : "READY",
                "value" : {
                  "science" : {
                    "atomCount" : 48,
                    "telescopeConfigs" : [
                      {
                        "guiding" : "ENABLED",
                        "offset" : {
                          "p" : { "microarcseconds" : 0 },
                          "q" : { "microarcseconds" : 0 }
                        }
                      },
                      {
                        "guiding" : "ENABLED",
                        "offset" : {
                          "p" : { "microarcseconds" : 1000000 },
                          "q" : { "microarcseconds" : 1000000 }
                        }
                      },
                      {
                        "guiding" : "ENABLED",
                        "offset" : {
                          "p" : { "microarcseconds" : 2000000 },
                          "q" : { "microarcseconds" : 2000000 }
                        }
                      },
                      {
                        "guiding" : "ENABLED",
                        "offset" : {
                          "p" : { "microarcseconds" : 3000000 },
                          "q" : { "microarcseconds" : 3000000 }
                        }
                      }
                    ]
                  }
                }
              }
            }
          }
        }
      """

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = s"""
          query {
            observation(observationId: "$oid") {
              execution {
                digest {
                  state
                  value {
                    science {
                      atomCount
                      telescopeConfigs {
                        guiding
                        offset {
                          p { microarcseconds }
                          q { microarcseconds }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        """,
        expected = expected.asRight
      )