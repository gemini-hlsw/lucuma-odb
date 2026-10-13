// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User

class guideTargetNameGMOS extends ExecutionTestSupportForGmos with GuideTargetNameSuite:

  override def createObservationAs(user: User, pid: Program.Id, tids: List[Target.Id]): IO[Observation.Id] =
    createGmosNorthLongSlitObservationAs(user, pid, tids)

  private def updateLongSlit(oid: Observation.Id, set: String): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: { observingMode: { gmosNorthLongSlit: { $set } } }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  private def nameAfter(edit: Observation.Id => IO[Unit]): IO[Observation.Id] =
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationAs(pi, p, List(t))
      _ <- setObservationTimeAndDuration(pi, o, ObsTime.some, none)
      _ <- setGuideTargetName(pi, o, targetName1.some)
      _ <- edit(o)
    yield o

  test("changing the exposure time mode keeps name"):
    nameAfter(updateLongSlit(_, "exposureTimeMode: { signalToNoise: { value: 50.0, at: { nanometers: 500 } } }"))
      .flatMap: oid =>
        expect(pi, guideTargetNameQuery(oid), expected = guideTargetNameResult(targetName1.some))

  test("changing the focal plane unit invalidates name"):
    nameAfter(updateLongSlit(_, "fpu: LONG_SLIT_1_00"))
      .flatMap: oid =>
        expect(pi, guideTargetNameQuery(oid), expected = guideTargetNameResult(none))
