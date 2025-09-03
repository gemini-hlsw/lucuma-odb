// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.syntax.string.*
import lucuma.core.util.Enumerated
import lucuma.odb.graphql.mutation.UpdateConstraintSetOps

class observation_configurationRequests 
  extends OdbSuite 
     with ObservingModeSetupOperations 
     with UpdateConstraintSetOps {

  val pi       = TestUsers.Standard.pi(1, 30)
  val admin    = TestUsers.Standard.admin(2, 31)
  val validUsers = List(pi, admin)

  /** Mutations that are relevant to configuration requests. */
  object Mutation {

    def forGmosNorthLongSlit(user: User, oid: Observation.Id, grating: GmosNorthGrating): IO[Unit] =
      updateObservationAs(user, oid):
        s"""
          observingMode: {
            gmosNorthLongSlit: {
              grating: ${grating.tag.toScreamingSnakeCase}
            }
          }
        """

    def forGmosSouthLongSlit(user: User, oid: Observation.Id, grating: GmosSouthGrating): IO[Unit] =
      updateObservationAs(user, oid):
        s"""
          observingMode: {
            gmosSouthLongSlit: {
              grating: ${grating.tag.toScreamingSnakeCase}
            }
          }
        """

    def forGmosNorthImaging(user: User, oid: Observation.Id, filters: List[GmosNorthFilter]): IO[Unit] =
      updateObservationAs(user, oid):
        s"""
          observingMode: {
            gmosNorthImaging: {
              filters: ${filters.map(_.tag.toScreamingSnakeCase).mkString("[", " ", "]")}
            }
          }
        """

    def forGmosSouthImaging(user: User, oid: Observation.Id, filters: List[GmosSouthFilter]): IO[Unit] =
      updateObservationAs(user, oid):
        s"""
          observingMode: {
            gmosSouthImaging: {
              filters:  ${filters.map(_.tag.toScreamingSnakeCase).mkString("[", " ", "]")}
            }
          }
        """

    def forFlamingos2LongSlit(user: User, oid: Observation.Id, disperser: Flamingos2Disperser): IO[Unit] =
      updateObservationAs(user, oid):
        s"""
          observingMode: {
            flamingos2LongSlit: {
              disperser: ${disperser.tag.toScreamingSnakeCase}
            }
          }
        """

  }

  def baseMutation(user: User, oid: Observation.Id, mode: ObservingModeType): IO[Unit] =
    mode match
      case ObservingModeType.GmosNorthLongSlit  => Mutation.forGmosNorthLongSlit(user, oid, GmosNorthGrating.B480_G5309)
      case ObservingModeType.GmosSouthLongSlit  => Mutation.forGmosSouthLongSlit(user, oid, GmosSouthGrating.B480_G5327)
      case ObservingModeType.Flamingos2LongSlit => Mutation.forFlamingos2LongSlit(user, oid, Flamingos2Disperser.R1200HK)
      case ObservingModeType.GmosNorthImaging   => Mutation.forGmosNorthImaging(user, oid, List(GmosNorthFilter.CaT, GmosNorthFilter.DS920))
      case ObservingModeType.GmosSouthImaging   => Mutation.forGmosSouthImaging(user, oid, List(GmosSouthFilter.CaT, GmosSouthFilter.GG455))
    
  def compatibleMutation(user: User, oid: Observation.Id, mode: ObservingModeType): IO[Unit] =
    mode match
      case ObservingModeType.GmosNorthLongSlit  => IO.unit // no changes are compatible
      case ObservingModeType.GmosSouthLongSlit  => IO.unit // no changes are compatible
      case ObservingModeType.Flamingos2LongSlit => IO.unit // no changes are compatible
      case ObservingModeType.GmosNorthImaging   => Mutation.forGmosNorthImaging(user, oid, List(GmosNorthFilter.DS920)) // subset of original, ok
      case ObservingModeType.GmosSouthImaging   => Mutation.forGmosSouthImaging(user, oid, List(GmosSouthFilter.GG455)) // subset of original, ok

  def incompatibleMutation(user: User, oid: Observation.Id, mode: ObservingModeType): IO[Unit] =
    mode match
      case ObservingModeType.GmosNorthLongSlit  => Mutation.forGmosNorthLongSlit(user, oid, GmosNorthGrating.B1200_G5301)
      case ObservingModeType.GmosSouthLongSlit  => Mutation.forGmosSouthLongSlit(user, oid, GmosSouthGrating.R600_G5324)
      case ObservingModeType.Flamingos2LongSlit => Mutation.forFlamingos2LongSlit(user, oid, Flamingos2Disperser.R1200JH)
      case ObservingModeType.GmosNorthImaging   => Mutation.forGmosNorthImaging(user, oid, List(GmosNorthFilter.GG455, GmosNorthFilter.GPrime_GG455))
      case ObservingModeType.GmosSouthImaging   => Mutation.forGmosSouthImaging(user, oid, List(GmosSouthFilter.GG455, GmosSouthFilter.GPrime_GG455))

  private def updateObservationAs(user: User, oid: Observation.Id)(update: String): IO[Unit] =
    updateObservation(user, oid, update,
      query = """
        observations {
          id
        }
      """,
      expected = Right(json"""
        {
          "updateObservations": {
            "observations": [
              {
                "id": $oid
              }
            ]
          }
        }
      """)
    )

  private def setExplicitBaseAs(user: User, oid: Observation.Id, hms: String, dms: String): IO[Unit] =
    updateObservationAs(user, oid):
      s"""
        targetEnvironment: {
          explicitBase: {
            ra: { hms: "$hms"},
            dec: { dms: "$dms"}
          }
        }
      """

  private def updateCloudExtinction(user: User, oid: Observation.Id, cloudExtinction: CloudExtinction.Preset): IO[Unit] =
    updateObservationAs(user, oid):
      s"""
        constraintSet: {
          cloudExtinction: ${cloudExtinction.tag.toUpperCase()}
        }
      """

  private def expectRequests(user: User, oid: Observation.Id, ids: List[ConfigurationRequest.Id]): IO[Unit] =
    expect(
      user = user,
      query = s"""
        query {
          observation(observationId: "$oid") {
            configurationRequests {
              id
            }
          }
        }
      """,
      expected = Right(json"""                
        {
          "observation" : {
            "configurationRequests" : ${ids.map(id => Json.obj("id" -> id.asJson))}
          }
        }                
      """)
    )

  // set up cfp, program, and fullt configured observation
  private def setup(too: Boolean, mode: ObservingModeType): IO[Observation.Id] =
    for
      cfpid <- createCallForProposalsAs(admin)
      pid   <- createProgramAs(pi, "Foo")
      _     <- addProposal(pi, pid, Some(cfpid), None)
      tid   <- if !too then createTargetWithProfileAs(pi, pid) else createOpportunityTargetAs(pi, pid)
      oid   <- 
        mode match
          case ObservingModeType.GmosNorthLongSlit  => createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
          case ObservingModeType.GmosSouthLongSlit  => createGmosSouthLongSlitObservationAs(pi, pid, List(tid))
          case ObservingModeType.Flamingos2LongSlit => createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
          case ObservingModeType.GmosNorthImaging   => createGmosNorthImagingObservationAs(pi, pid, tid)
          case ObservingModeType.GmosSouthImaging   => createGmosSouthImagingObservationAs(pi, pid, tid)
    yield oid

  Enumerated[ObservingModeType].all.foreach { mode =>
    List(false, true).foreach { too =>

      val prefix = s"[$mode, ${if too then "opportunity" else "sidereal"}]"

      test(s"$prefix create and select some configuration requests, expect deduplication"):
        for
          oid   <- setup(too, mode)
          ids   <- createConfigurationRequestAs(pi, oid).replicateA(2)
          _     <- expectRequests(pi, oid, ids.distinct)
        yield ()

      test(s"$prefix request should apply to identical obs"):
        for
          oid   <- setup(too, mode)
          oid2  <- cloneObservationAs(pi, oid)
          mid   <- createConfigurationRequestAs(pi, oid)
          _     <- expectRequests(pi, oid2, List(mid))
        yield ()

      test(s"$prefix request should apply for nearby base position"):
        for
          oid  <- setup(too, mode)
          _    <- setExplicitBaseAs(pi, oid, "1:00:00", "2:00:00")
          mid  <- createConfigurationRequestAs(pi, oid)
          _    <- setExplicitBaseAs(pi, oid, "1:00:00.01", "2:00:00.01")
          _    <- expectRequests(pi, oid, List(mid))
        yield ()

      test(s"$prefix request should not apply for faraway base position"):
        for
          oid  <- setup(too, mode)
          _    <- setExplicitBaseAs(pi, oid, "1:00:00", "2:00:00")
          mid  <- createConfigurationRequestAs(pi, oid)
          _    <- setExplicitBaseAs(pi, oid, "3:00:00", "4:00:00")
          _    <- expectRequests(pi, oid, Nil)
        yield ()

      test(s"$prefix request should apply when base position is moved back"):
        for
          oid  <- setup(too, mode)
          _    <- setExplicitBaseAs(pi, oid, "1:00:00", "2:00:00")
          mid  <- createConfigurationRequestAs(pi, oid)
          _    <- setExplicitBaseAs(pi, oid, "3:00:00", "4:00:00")
          _    <- expectRequests(pi, oid, Nil) // sanity check
          _    <- setExplicitBaseAs(pi, oid, "1:00:00", "2:00:00")
          _    <- expectRequests(pi, oid, List(mid))
        yield ()

      test(s"$prefix request should apply after compatible mutation"):
        for
          oid  <- setup(too, mode)
          _    <- baseMutation(pi, oid, mode)
          mid  <- createConfigurationRequestAs(pi, oid)
          _    <- compatibleMutation(pi, oid, mode)
          _    <- expectRequests(pi, oid, List(mid))
        yield ()

      test(s"$prefix request should apply after compatible mutation"):
        for
          oid  <- setup(too, mode)
          _    <- baseMutation(pi, oid, mode)
          mid  <- createConfigurationRequestAs(pi, oid)
          _    <- incompatibleMutation(pi, oid, mode)
          _    <- expectRequests(pi, oid, Nil)
        yield ()

      test(s"$prefix request should not apply for narrower conditions"):
        for
          oid  <- setup(too, mode)
          _    <- updateCloudExtinction(pi, oid, CloudExtinction.Preset.PointFive)
          mid  <- createConfigurationRequestAs(pi, oid)
          _    <- updateCloudExtinction(pi, oid, CloudExtinction.Preset.PointThree) // can't ask for better conditions
          _    <- expectRequests(pi, oid, Nil)
        yield ()

      test(s"$prefix request should apply for wider conditions"):
        for
          oid  <- setup(too, mode)
          _    <- updateCloudExtinction(pi, oid, CloudExtinction.Preset.PointThree)
          mid  <- createConfigurationRequestAs(pi, oid)
          _    <- updateCloudExtinction(pi, oid, CloudExtinction.Preset.PointFive) // ok to ask for worse conditions
          _    <- expectRequests(pi, oid, List(mid))
        yield ()

    }
  }

}





  