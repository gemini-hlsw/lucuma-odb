// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.effect.IO
import cats.syntax.show.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User

import scala.concurrent.duration.*


trait SubscriptionUtils { self: OdbSuite =>

  // try to behave nicely on weak CI machines
  private val sleep: IO[Unit] =
    IO.sleep(500.millis)

  def createProgram(user: User, name: String): IO[Program.Id] =
    sleep >>
      query(
        user  = user,
        query =
          s"""
          mutation {
            createProgram(input: { SET: { name: "$name" } }) {
              program {
                id
              }
            }
          }
        """
      ).map { json =>
        json.hcursor.downFields("createProgram", "program", "id").require[Program.Id]
      }

  def createObservation(user: User, subtitle: String, pid: Program.Id): IO[Observation.Id] =
    sleep >>
      query(
        user  = user,
        query =
          s"""
            mutation {
              createObservation(input: { programId: "${pid.show}", SET: { subtitle: "$subtitle" }}) {
                observation {
                  id
                }
              }
            }
          """
      ).map { json =>
        json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
      }

  def updateObservation(user: User, subtitle: String, oid: Observation.Id): IO[Unit] =
    sleep >>
      query(
        user    = user,
        query   =
          s"""
            mutation {
              updateObservations(input: {
                SET: {
                  subtitle: "${subtitle}"
                },
                WHERE: {
                  id: { EQ: "${oid.show}" }
                }
              }) {
                observations {
                  id
                }
              }
            }
          """
      ).void
}