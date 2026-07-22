// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import lucuma.core.model.Observation
import lucuma.itc.ItcVersions
import lucuma.itc.client.ClientCalculationResult
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyInput
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsInput
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsResult
import lucuma.odb.data.OdbError

import java.io.IOException
import java.util.concurrent.atomic.AtomicBoolean

// resetAcquisition re-derives the acquisition ITC afresh (bypassing the frozen
// snapshot).  When that lookup cannot be completed the mutation must fail with
// nothing written, leaving the already-materialized acquisition intact.
class resetAcquisitionItcFailure extends ExecutionTestSupportForGmos:

  // When set, the acquisition (imaging) ITC call fails as if the service were
  // unavailable.  The science (spectroscopy) call is unaffected.
  private val acquisitionItcDown: AtomicBoolean = new AtomicBoolean(false)

  override protected def itcClient: ItcClient[IO] =
    val delegate = super.itcClient
    new ItcClient[IO]:
      override def imaging(input: ImagingInput, useCache: Boolean): IO[ClientCalculationResult] =
        if acquisitionItcDown.get then IO.raiseError(new IOException("ITC unavailable"))
        else delegate.imaging(input, useCache)

      override def spectroscopy(input: SpectroscopyInput, useCache: Boolean): IO[ClientCalculationResult] =
        delegate.spectroscopy(input, useCache)

      override def spectroscopyIntegrationTimeAndGraphs(
        input:    SpectroscopyIntegrationTimeAndGraphsInput,
        useCache: Boolean
      ): IO[SpectroscopyIntegrationTimeAndGraphsResult] =
        delegate.spectroscopyIntegrationTimeAndGraphs(input, useCache)

      override def versions: IO[ItcVersions] =
        delegate.versions

  private def resetAcquisitionMutation(oid: Observation.Id): String =
    s"""
      mutation {
        resetAcquisition(input: { observationId: "$oid" }) {
          observation { id }
        }
      }
    """

  test("resetAcquisition fails (and preserves the sequence) when the acquisition ITC is unavailable"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

      // Begin execution: the ITC result is frozen and the acquisition materialized.
      v <- recordVisitAs(serviceUser, o)
      s <- firstAcquisitionStepId(serviceUser, o)
      _ <- addEndStepEvent(s, v)

      // The acquisition as materialized, for comparison after a failed reset.
      before <- query(pi, gmosNorthAcquisitionQuery(o))

      // Simulate an ITC outage: the reset must fail...
      _ <- IO(acquisitionItcDown.set(true))
      _ <- expectOdbError(
             user     = serviceUser,
             query    = resetAcquisitionMutation(o),
             expected = { case OdbError.RemoteServiceCallError(_) => () }
           )

      // ...with nothing written, so the materialized acquisition is unchanged.
      after <- query(pi, gmosNorthAcquisitionQuery(o))
      _      = assertEquals(after, before)

      // With the ITC restored, the reset succeeds again.
      _ <- IO(acquisitionItcDown.set(false))
      _ <- resetAcquisitionAs(serviceUser, o)
    yield ()
