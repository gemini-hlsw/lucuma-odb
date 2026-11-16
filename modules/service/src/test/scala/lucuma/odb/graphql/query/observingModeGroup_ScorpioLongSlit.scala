// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target

class observingModeGroup_ScorpioLongSlit extends OdbSuite with ObservingModeSetupOperations:

  val pi         = TestUsers.Standard.pi(nextId, nextId)
  val validUsers = List(pi)

  private def scorpio(fpu: String, seconds: Int): String =
    def etm(nm: Int): String =
      s"{ timeAndCount: { time: { seconds: $seconds }, count: 2, at: { nanometers: $nm } } }"

    s"""
      scorpioLongSlit: {
        fpu: $fpu
        g: ${etm(469)}
        r: ${etm(622)}
        i: ${etm(755)}
        z: ${etm(889)}
      }
    """

  private def create(pid: Program.Id, tid: Target.Id, fpu: String, seconds: Int): IO[Observation.Id] =
    createObservationWithModeAs(pi, pid, List(tid), scorpio(fpu, seconds))

  private def groupQuery(pid: Program.Id): String =
    s"""
      query {
        observingModeGroup(programId: ${pid.asJson}) {
          matches {
            observingMode {
              scorpioLongSlit { fpu }
            }
            observations {
              matches { id }
            }
          }
        }
      }
    """

  private def group(fpu: String, oids: Observation.Id*): Json =
    json"""
      {
        "observingMode": { "scorpioLongSlit": { "fpu": $fpu } },
        "observations": { "matches": ${oids.toList.map(id => Json.obj("id" -> id.asJson))} }
      }
    """

  private def expectGroups(pid: Program.Id, groups: Json*): IO[Unit] =
    expect(
      user     = pi,
      query    = groupQuery(pid),
      expected = Json.obj("observingModeGroup" -> Json.obj("matches" -> groups.toList.asJson)).asRight
    )

  test("the same slit with different exposures is one group"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid, "Biff")
      o1  <- create(pid, tid, "LONG_SLIT_0_54", 10)
      o2  <- create(pid, tid, "LONG_SLIT_0_54", 30)
      _   <- expectGroups(pid, group("LONG_SLIT_0_54", o1, o2))
    yield ()

  test("different slits form different groups"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid, "Biff")
      o1  <- create(pid, tid, "LONG_SLIT_0_54", 10)
      o2  <- create(pid, tid, "LONG_SLIT_1_08", 10)
      _   <- expectGroups(pid, group("LONG_SLIT_0_54", o1), group("LONG_SLIT_1_08", o2))
    yield ()
