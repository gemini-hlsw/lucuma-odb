// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.effect.IO
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.Gid

import scala.concurrent.duration._


trait SubscriptionUtils { self: OdbSuite =>

  def createProgram(user: User, name: String): IO[Program.Id] =
    IO.sleep(500.millis) >> // try to behave nicely on weak CI machines
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
    IO.sleep(500.millis) >>
      query(
        user  = user,
        query =
          s"""
            mutation {
              createObservation(input: { programId: "${Gid[Program.Id].show(pid)}", SET: { subtitle: "$subtitle" }}) {
                observation {
                  id
                }
              }
            }
          """
      ).map { json =>
        json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
      }
}