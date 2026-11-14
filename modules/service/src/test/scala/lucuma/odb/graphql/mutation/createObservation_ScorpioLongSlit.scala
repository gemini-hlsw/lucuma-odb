// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.ObservingModeType.*
import lucuma.core.enums.ScorpioFpu
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.core.syntax.timespan.*
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.scorpio.longslit.Config

class createObservation_ScorpioLongSlit extends OdbSuite:

  val pi: StandardUser = TestUsers.Standard.pi(nextId, nextId)
  val serviceUser      = TestUsers.service(nextId)

  lazy val validUsers: List[User] =
    List(pi, serviceUser)

  private def timeAndCount(seconds: Int, count: Int, nm: Int): String =
    s"{ timeAndCount: { time: { seconds: $seconds }, count: $count, at: { nanometers: $nm } } }"

  private val ChannelQuery: String =
    "{ timeAndCount { time { seconds } count at { nanometers } } signalToNoise { value } }"

  private def channelJson(seconds: Int, count: Int, nm: Int): Json =
    json"""
      {
        "timeAndCount": {
          "time": { "seconds": ${BigDecimal(seconds).setScale(6)} },
          "count": $count,
          "at": { "nanometers": ${BigDecimal(nm).setScale(3)} }
        },
        "signalToNoise": null
      }
    """

  private val TelescopeConfigsQuery: String =
    "{ offsetMode alongSlit { q { arcseconds } guiding } toSky { guiding } }"

  private def alongSlitJson(qs: Int*): Json =
    Json.obj(
      "offsetMode" -> Json.fromString("NOD_ALONG_SLIT"),
      "alongSlit"  -> Json.fromValues(qs.map: q =>
        json"""{ "q": { "arcseconds": ${BigDecimal(q).setScale(6)} }, "guiding": "ENABLED" }"""
      ),
      "toSky"      -> Json.Null
    )

  private val DefaultTelescopeConfigs: Json =
    alongSlitJson(0, -15, 15, 0)

  // GHOST requirements give a time and count mode, Flamingos 2 a signal to noise one.
  private def createMutation(
    pid:          Program.Id,
    requirements: String,
    scorpio:      String,
    selection:    String
  ): String =
    s"""
      mutation {
        createObservation(input: {
          programId: "$pid"
          SET: {
            scienceRequirements: $requirements
            observingMode: {
              scorpioLongSlit: { $scorpio }
            }
          }
        }) {
          observation { $selection }
        }
      }
    """

  private val ModeSelection: String =
    s"""
      observingMode {
        instrument
        mode
        scorpioLongSlit {
          fpu
          g $ChannelQuery
          r $ChannelQuery
          i $ChannelQuery
          z $ChannelQuery
          telescopeConfigs $TelescopeConfigsQuery
          defaultTelescopeConfigs $TelescopeConfigsQuery
          explicitTelescopeConfigs $TelescopeConfigsQuery
        }
      }
    """

  private def expectRejected(requirements: String, scorpio: String, message: String): IO[Unit] =
    createProgramAs(pi).flatMap: pid =>
      expect(
        user     = pi,
        query    = createMutation(pid, requirements, scorpio, "id"),
        expected = List(message).asLeft
      )

  test("create SCORPIO long slit"):
    createProgramAs(pi).flatMap: pid =>
      expect(
        user  = pi,
        query = createMutation(
          pid,
          scienceRequirementsObject(GhostIfu),
          s"""
            fpu: LONG_SLIT_0_54
            g: ${timeAndCount(10, 2, 469)}
            r: ${timeAndCount(10, 2, 622)}
            i: ${timeAndCount(20, 1, 755)}
            z: ${timeAndCount(20, 1, 889)}
          """,
          ModeSelection
        ),
        expected =
          json"""
            {
              "createObservation": {
                "observation": {
                  "observingMode": {
                    "instrument": "SCORPIO",
                    "mode": "SCORPIO_LONG_SLIT",
                    "scorpioLongSlit": {
                      "fpu": "LONG_SLIT_0_54",
                      "g": ${channelJson(10, 2, 469)},
                      "r": ${channelJson(10, 2, 622)},
                      "i": ${channelJson(20, 1, 755)},
                      "z": ${channelJson(20, 1, 889)},
                      "telescopeConfigs": $DefaultTelescopeConfigs,
                      "defaultTelescopeConfigs": $DefaultTelescopeConfigs,
                      "explicitTelescopeConfigs": null
                    }
                  }
                }
              }
            }
          """.asRight
      )

  test("channels default to the science requirements"):
    createProgramAs(pi).flatMap: pid =>
      expect(
        user  = pi,
        query = createMutation(
          pid,
          scienceRequirementsObject(GhostIfu),
          "fpu: LONG_SLIT_1_08",
          s"observingMode { scorpioLongSlit { fpu g $ChannelQuery r $ChannelQuery i $ChannelQuery z $ChannelQuery } }"
        ),
        expected =
          json"""
            {
              "createObservation": {
                "observation": {
                  "observingMode": {
                    "scorpioLongSlit": {
                      "fpu": "LONG_SLIT_1_08",
                      "g": ${channelJson(1, 5, 500)},
                      "r": ${channelJson(1, 5, 500)},
                      "i": ${channelJson(1, 5, 500)},
                      "z": ${channelJson(1, 5, 500)}
                    }
                  }
                }
              }
            }
          """.asRight
      )

  test("explicit telescope configs override the default"):
    createProgramAs(pi).flatMap: pid =>
      expect(
        user  = pi,
        query = createMutation(
          pid,
          scienceRequirementsObject(GhostIfu),
          """
            fpu: LONG_SLIT_0_36
            explicitTelescopeConfigs: {
              alongSlit: [
                { q: { arcseconds: -5 }, guiding: ENABLED },
                { q: { arcseconds:  5 }, guiding: ENABLED }
              ]
            }
          """,
          s"""
            observingMode {
              scorpioLongSlit {
                telescopeConfigs $TelescopeConfigsQuery
                defaultTelescopeConfigs $TelescopeConfigsQuery
                explicitTelescopeConfigs $TelescopeConfigsQuery
              }
            }
          """
        ),
        expected =
          json"""
            {
              "createObservation": {
                "observation": {
                  "observingMode": {
                    "scorpioLongSlit": {
                      "telescopeConfigs": ${alongSlitJson(-5, 5)},
                      "defaultTelescopeConfigs": $DefaultTelescopeConfigs,
                      "explicitTelescopeConfigs": ${alongSlitJson(-5, 5)}
                    }
                  }
                }
              }
            }
          """.asRight
      )

  test("the stored mode reads back as a sequence config"):
    val g = ExposureTimeMode.TimeAndCountMode(10.secondTimeSpan, PosInt.unsafeFrom(2), Wavelength.unsafeFromIntPicometers(469_000))
    val r = g.copy(at = Wavelength.unsafeFromIntPicometers(622_000))
    val i = ExposureTimeMode.TimeAndCountMode(20.secondTimeSpan, PosInt.unsafeFrom(1), Wavelength.unsafeFromIntPicometers(755_000))
    val z = i.copy(at = Wavelength.unsafeFromIntPicometers(889_000))

    for
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid, Some(ScorpioLongSlit))
      cfg <- withServices(serviceUser): services =>
               services.transactionally:
                 services.scorpioLongSlitService.select(List(oid)).map(_.get(oid))
    yield assertEquals(cfg, Some(Config(ScorpioFpu.LongSlit_0_54, g, r, i, z, Config.DefaultTelescopeConfigs)))

  test("cannot create without a slit"):
    expectRejected(
      scienceRequirementsObject(GhostIfu),
      s"g: ${timeAndCount(10, 2, 469)}",
      "Argument 'input.SET.observingMode.scorpioLongSlit' is invalid: A slit (fpu) must be supplied for SCORPIO Long Slit observations"
    )

  test("cannot create from signal to noise requirements alone"):
    expectRejected(
      scienceRequirementsObject(Flamingos2LongSlit),
      "fpu: LONG_SLIT_0_54",
      "SCORPIO Long Slit requires time and count exposure time modes."
    )

  test("cannot create with a signal to noise channel"):
    expectRejected(
      scienceRequirementsObject(GhostIfu),
      """
        fpu: LONG_SLIT_0_54
        i: { signalToNoise: { value: 100.0, at: { nanometers: 755 } } }
        z: { signalToNoise: { value: 100.0, at: { nanometers: 889 } } }
      """,
      "SCORPIO Long Slit requires time and count exposure time modes."
    )

  test("cannot create with different g and r exposures"):
    expectRejected(
      scienceRequirementsObject(GhostIfu),
      s"""
        fpu: LONG_SLIT_0_54
        g: ${timeAndCount(10, 2, 469)}
        r: ${timeAndCount(11, 2, 622)}
      """,
      "The SCORPIO g and r channels share a detector controller and must have the same exposure time and count."
    )

  test("cannot create with different i and z exposures"):
    expectRejected(
      scienceRequirementsObject(GhostIfu),
      s"""
        fpu: LONG_SLIT_0_54
        i: ${timeAndCount(20, 1, 755)}
        z: ${timeAndCount(20, 3, 889)}
      """,
      "The SCORPIO i and z channels share a detector controller and must have the same exposure time and count."
    )

  test("a SCORPIO observation is found among other modes"):
    for
      p <- createProgramAs(pi)
      _ <- createObservationAs(pi, p, Some(ScorpioLongSlit))
      t <- createTargetAs(pi, p)
      _ <- createGmosNorthImagingObservationAs(pi, p, t)
      _ <- expect(
             user  = pi,
             query = s"""
               query {
                 observations(WHERE: { program: { id: { EQ: "$p" } } }) {
                   matches {
                     observingMode {
                       mode
                       scorpioLongSlit { fpu g { timeAndCount { count } } z { timeAndCount { count } } }
                     }
                   }
                 }
               }
             """,
             expected = json"""
               {
                 "observations": {
                   "matches": [
                     {
                       "observingMode": {
                         "mode": "SCORPIO_LONG_SLIT",
                         "scorpioLongSlit": {
                           "fpu": "LONG_SLIT_0_54",
                           "g": { "timeAndCount": { "count": 2 } },
                           "z": { "timeAndCount": { "count": 1 } }
                         }
                       }
                     },
                     {
                       "observingMode": {
                         "mode": "GMOS_NORTH_IMAGING",
                         "scorpioLongSlit": null
                       }
                     }
                   ]
                 }
               }
             """.asRight
           )
    yield ()

  test("the sequence is reported as unavailable"):
    for
      p <- createProgramAs(pi)
      t <- createTargetAs(pi, p)
      o <- createObservationAs(pi, p, Some(ScorpioLongSlit), t)
      _ <- expectOdbError(
             user     = pi,
             query    = s"""query { executionConfig(observationId: "$o") { instrument } }""",
             expected = {
               case OdbError.SequenceUnavailable(`o`, Some(m)) if m.contains("Sequences are not supported for SCORPIO") => // ok
             }
           )
    yield ()
