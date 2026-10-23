// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime

// Groups of GMOS observations that may be executed, for the group progress and
// group workflow tests.
trait GroupTestSupport extends ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(2)
    )

  def newObservation(pid: Program.Id): IO[Observation.Id] =
    createTargetWithProfileAs(pi, pid).flatMap(t => createGmosNorthLongSlitObservationAs(pi, pid, List(t)))

  def completeObservation(oid: Observation.Id): IO[Unit] =
    for
      ss <- scienceStepIds(serviceUser, oid)
      v  <- recordVisitAs(serviceUser, oid)
      _  <- ss.traverse(sid => addEndStepEvent(sid, v))
    yield ()

  def group(
    pid:      Program.Id,
    oids:     List[Observation.Id],
    min:      Option[Int]      = None,
    minI:     Option[TimeSpan] = None,
    maxI:     Option[TimeSpan] = None,
    parent:   Option[Group.Id] = None
  ): IO[Group.Id] =
    createGroupAs(
      pi,
      pid,
      parentGroupId   = parent,
      minRequired     = min.map(m => NonNegShort.unsafeFrom(m.toShort)),
      minimumInterval = minI,
      maximumInterval = maxI,
      initialContents = Some(oids.map(_.asRight))
    )

  def startObservation(oid: Observation.Id): IO[Unit] =
    for
      v  <- recordVisitAs(serviceUser, oid)
      ss <- firstScienceAtomStepIds(serviceUser, oid)
      _  <- ss.take(1).traverse(sid => addEndStepEvent(sid, v))
    yield ()
