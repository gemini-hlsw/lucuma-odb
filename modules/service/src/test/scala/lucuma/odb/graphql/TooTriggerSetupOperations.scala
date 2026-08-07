// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import cats.syntax.option.*
import io.circe.syntax.*
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.query.ObservingModeSetupOperations

/**
 * Builds an observation that `requestTooTrigger` will actually accept: it needs
 * a ToO activation other than NONE, no opportunity placeholder in its asterism,
 * and a cached workflow state of exactly `Defined`.  That last one is why this
 * is more than a `createObservationAs` -- the observation has to be genuinely
 * valid (real target, observing mode, ITC results) and obscalc has to have run.
 */
trait TooTriggerSetupOperations extends ObservingModeSetupOperations { this: OdbSuite =>

  /** The service user used to drive obscalc; not a GraphQL caller. */
  val tooObscalcUser = TestUsers.service(97)

  // temporary, until this is doable via graphql
  private def approveConfigurationRequestHack(req: ConfigurationRequest.Id): IO[Unit] =
    import skunk.syntax.all.*
    import lucuma.odb.util.Codecs.configuration_request_id
    session.use: s =>
      s.prepareR(sql"update t_configuration_request set c_status = 'approved' where c_configuration_request_id = $configuration_request_id".command).use: ps =>
        ps.execute(req).void

  def setTooActivationAs(user: User, oid: Observation.Id, activation: String): IO[Unit] =
    query(
      user,
      s"""
        mutation {
          updateObservations(input: {
            SET: { schedulingConstraints: { tooActivation: $activation } }
            WHERE: { id: { EQ: ${oid.asJson} } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  /**
   * A program plus one triggerable ToO observation, with obscalc already run so
   * the request precondition sees `Defined`.  `activation` is set on the only
   * observation, so the derived proposal ceiling matches it and the ceiling
   * check passes without an explicit ceiling.
   */
  def createTooObservationAs(
    pi:         User,
    staff:      User,
    activation: String = "RAPID"
  ): IO[(Program.Id, Observation.Id)] =
    for
      cfp <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramAs(pi, "ToO")
      _   <- addProposal(pi, pid, cfp.some, None)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack)
      _   <- computeItcResultAs(pi, oid)
      _   <- setTooActivationAs(pi, oid, activation)
      _   <- runObscalcUpdateAs(tooObscalcUser, pid, oid)
    yield (pid, oid)

}
