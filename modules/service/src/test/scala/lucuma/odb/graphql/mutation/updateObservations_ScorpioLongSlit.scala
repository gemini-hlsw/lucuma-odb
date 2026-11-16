// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.User

class updateObservations_ScorpioLongSlit extends query.ExecutionTestSupportForGmos with UpdateObservationsOps:

  private def timeAndCount(seconds: Int, count: Int, nm: Int): String =
    s"{ timeAndCount: { time: { seconds: $seconds }, count: $count, at: { nanometers: $nm } } }"

  private val ScorpioInput: String =
    s"""
      scorpioLongSlit: {
        fpu: LONG_SLIT_0_54
        g: ${timeAndCount(10, 2, 469)}
        r: ${timeAndCount(10, 2, 622)}
        i: ${timeAndCount(20, 1, 755)}
        z: ${timeAndCount(20, 1, 889)}
      }
    """

  private val ChannelQuery: String =
    "{ timeAndCount { time { seconds } count at { nanometers } } }"

  private def channelJson(seconds: Int, count: Int, nm: Int): Json =
    json"""
      {
        "timeAndCount": {
          "time": { "seconds": ${BigDecimal(seconds).setScale(6)} },
          "count": $count,
          "at": { "nanometers": ${BigDecimal(nm).setScale(3)} }
        }
      }
    """

  private val TelescopeConfigsQuery: String =
    "{ alongSlit { q { arcseconds } } }"

  private def alongSlitJson(qs: Int*): Json =
    Json.obj(
      "alongSlit" -> Json.fromValues(qs.map: q =>
        json"""{ "q": { "arcseconds": ${BigDecimal(q).setScale(6)} } }"""
      )
    )

  private val ModeQuery: String =
    s"""
      observations {
        observingMode {
          mode
          scorpioLongSlit {
            fpu
            g $ChannelQuery
            r $ChannelQuery
            i $ChannelQuery
            z $ChannelQuery
            telescopeConfigs $TelescopeConfigsQuery
            explicitTelescopeConfigs $TelescopeConfigsQuery
          }
        }
      }
    """

  private def expectedMode(
    fpu:      String,
    g:        Json,
    r:        Json,
    i:        Json       = channelJson(20, 1, 755),
    z:        Json       = channelJson(20, 1, 889),
    configs:  Json       = alongSlitJson(0, -15, 15, 0),
    explicit: Json       = Json.Null
  ): Either[String, Json] =
    json"""
      {
        "updateObservations": {
          "observations": [
            {
              "observingMode": {
                "mode": "SCORPIO_LONG_SLIT",
                "scorpioLongSlit": {
                  "fpu": $fpu,
                  "g": $g,
                  "r": $r,
                  "i": $i,
                  "z": $z,
                  "telescopeConfigs": $configs,
                  "explicitTelescopeConfigs": $explicit
                }
              }
            }
          ]
        }
      }
    """.asRight

  private val createScorpio: IO[Observation.Id] =
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), ScorpioInput)
    yield o

  private def edit(scorpio: String): String =
    s"observingMode: { scorpioLongSlit: { $scorpio } }"

  test("update the slit"):
    createScorpio.flatMap: o =>
      updateObservation(
        pi, o,
        edit("fpu: LONG_SLIT_1_08"),
        ModeQuery,
        expectedMode("LONG_SLIT_1_08", channelJson(10, 2, 469), channelJson(10, 2, 622))
      )

  test("update both channels of a pair"):
    createScorpio.flatMap: o =>
      updateObservation(
        pi, o,
        edit(s"g: ${timeAndCount(30, 3, 470)}, r: ${timeAndCount(30, 3, 620)}"),
        ModeQuery,
        expectedMode("LONG_SLIT_0_54", channelJson(30, 3, 470), channelJson(30, 3, 620))
      )

  test("a channel updated alone takes its controller partner along"):
    createScorpio.flatMap: o =>
      updateObservation(
        pi, o,
        edit(s"g: ${timeAndCount(30, 3, 470)}, z: ${timeAndCount(40, 4, 890)}"),
        ModeQuery,
        expectedMode(
          "LONG_SLIT_0_54",
          g = channelJson(30, 3, 470),
          r = channelJson(30, 3, 622),
          i = channelJson(40, 4, 755),
          z = channelJson(40, 4, 890)
        )
      )

  test("cannot switch a channel to signal to noise"):
    createScorpio.flatMap: o =>
      updateObservation(
        pi, o,
        edit("g: { signalToNoise: { value: 100.0, at: { nanometers: 469 } } }"),
        ModeQuery,
        "SCORPIO Long Slit requires time and count exposure time modes.".asLeft
      )

  test("cannot update a pair to different exposures"):
    createScorpio.flatMap: o =>
      updateObservation(
        pi, o,
        edit(s"i: ${timeAndCount(20, 1, 755)}, z: ${timeAndCount(20, 3, 889)}"),
        ModeQuery,
        "The SCORPIO i and z channels share a detector controller and must have the same exposure time and count.".asLeft
      )

  test("set and clear explicit telescope configs"):
    val g = channelJson(10, 2, 469)
    val r = channelJson(10, 2, 622)

    for
      o <- createScorpio
      _ <- updateObservation(
             pi, o,
             edit("explicitTelescopeConfigs: { alongSlit: [ { q: { arcseconds: -5 }, guiding: ENABLED }, { q: { arcseconds: 5 }, guiding: ENABLED } ] }"),
             ModeQuery,
             expectedMode("LONG_SLIT_0_54", g, r, configs = alongSlitJson(-5, 5), explicit = alongSlitJson(-5, 5))
           )
      _ <- updateObservation(
             pi, o,
             edit("explicitTelescopeConfigs: null"),
             ModeQuery,
             expectedMode("LONG_SLIT_0_54", g, r)
           )
    yield ()

  test("switch an observation from another mode to SCORPIO"):
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _ <- updateObservation(
             pi, o,
             s"observingMode: { $ScorpioInput }",
             ModeQuery,
             expectedMode("LONG_SLIT_0_54", channelJson(10, 2, 469), channelJson(10, 2, 622))
           )
    yield ()

  test("cannot switch to SCORPIO without a slit"):
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _ <- updateObservation(
             pi, o,
             edit(s"g: ${timeAndCount(10, 2, 469)}"),
             ModeQuery,
             "A slit (fpu) must be supplied for SCORPIO Long Slit observations".asLeft
           )
    yield ()

  // SCORPIO has no sequence yet, so it cannot be ongoing itself.  An ongoing
  // observation of another mode pins the rule: no SCORPIO edit once executing.
  private val ongoingObservation: IO[Observation.Id] =
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v <- recordVisitAs(serviceUser, o)
      s <- firstAcquisitionStepId(serviceUser, o)
      _ <- addEndStepEvent(s, v)
    yield o

  List[(String, User, String)](
    ("pi",    pi,    "Completed"),
    ("staff", staff, "Suspended/Completed")
  ).foreach: (name, user, transitions) =>
    test(s"$name cannot set a SCORPIO mode on an ongoing observation"):
      ongoingObservation.flatMap: o =>
        updateObservation(
          user, o,
          s"observingMode: { $ScorpioInput }",
          ModeQuery,
          s"Observation $o is ineligible for this operation due to its workflow state (Ongoing with allowed transition to $transitions).".asLeft
        )
