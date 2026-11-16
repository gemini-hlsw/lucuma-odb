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
import lucuma.odb.graphql.query.ObservingModeSetupOperations

class cloneObservation_ScorpioLongSlit extends OdbSuite with UpdateObservationsOps with ObservingModeSetupOperations:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)
  override lazy val validUsers: List[User] = List(pi)

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
        explicitTelescopeConfigs: {
          alongSlit: [
            { q: { arcseconds: -5 }, guiding: ENABLED },
            { q: { arcseconds:  5 }, guiding: ENABLED }
          ]
        }
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

  private val ModeQuery: String =
    s"""
      observingMode {
        mode
        scorpioLongSlit {
          fpu
          g $ChannelQuery
          r $ChannelQuery
          i $ChannelQuery
          z $ChannelQuery
          explicitTelescopeConfigs { alongSlit { q { arcseconds } } }
        }
      }
    """

  private def modeJson(fpu: String, g: Json, r: Json): Json =
    json"""
      {
        "observingMode": {
          "mode": "SCORPIO_LONG_SLIT",
          "scorpioLongSlit": {
            "fpu": $fpu,
            "g": $g,
            "r": $r,
            "i": ${channelJson(20, 1, 755)},
            "z": ${channelJson(20, 1, 889)},
            "explicitTelescopeConfigs": {
              "alongSlit": [
                { "q": { "arcseconds": -5.000000 } },
                { "q": { "arcseconds": 5.000000 } }
              ]
            }
          }
        }
      }
    """

  private val OriginalMode: Json =
    modeJson("LONG_SLIT_0_54", channelJson(10, 2, 469), channelJson(10, 2, 622))

  private def expectMode(oid: Observation.Id, mode: Json): IO[Unit] =
    expect(
      user     = pi,
      query    = s"""query { observation(observationId: "$oid") { $ModeQuery } }""",
      expected = Json.obj("observation" -> mode).asRight
    )

  private val createAndClone: IO[(Observation.Id, Observation.Id)] =
    for
      p  <- createProgramAs(pi)
      t  <- createTargetWithProfileAs(pi, p)
      o0 <- createObservationWithModeAs(pi, p, List(t), ScorpioInput)
      o1 <- cloneObservationAs(pi, o0)
    yield (o0, o1)

  test("a clone has the same slit, channels and explicit telescope configs"):
    createAndClone.flatMap: (_, clone) =>
      expectMode(clone, OriginalMode)

  test("editing a clone leaves the original unchanged"):
    val edited = modeJson("LONG_SLIT_1_08", channelJson(30, 3, 469), channelJson(30, 3, 622))

    for
      (orig, clone) <- createAndClone
      _ <- updateObservation(
             pi, clone,
             s"observingMode: { scorpioLongSlit: { fpu: LONG_SLIT_1_08, g: ${timeAndCount(30, 3, 469)} } }",
             s"observations { $ModeQuery }",
             Json.obj("updateObservations" -> Json.obj("observations" -> Json.arr(edited))).asRight
           )
      _ <- expectMode(orig, OriginalMode)
    yield ()
