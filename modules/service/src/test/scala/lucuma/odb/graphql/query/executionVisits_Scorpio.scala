// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.either.*
import io.circe.literal.*
import lucuma.core.model.User

class executionVisits_Scorpio extends OdbSuite with ObservingModeSetupOperations:

  val pi = TestUsers.Standard.pi(nextId, nextId)

  lazy val validUsers: List[User] =
    List(pi)

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
    "{ exposureTime { seconds } exposureCount readoutMode }"

  // No SCORPIO visit can be recorded yet, so this only checks that the static
  // and dynamic mappings select columns that exist.
  test("the SCORPIO static and dynamic configs can be selected"):
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), ScorpioInput)
      _ <- expect(
             user  = pi,
             query = s"""
               query {
                 observation(observationId: "$o") {
                   execution {
                     visits {
                       matches {
                         scorpio { channelSelection }
                         atomRecords {
                           matches {
                             steps {
                               matches {
                                 scorpio {
                                   g $ChannelQuery
                                   r $ChannelQuery
                                   i $ChannelQuery
                                   z $ChannelQuery
                                   fpu
                                   disperser
                                 }
                               }
                             }
                           }
                         }
                       }
                     }
                   }
                 }
               }
             """,
             expected = json"""
               {
                 "observation": {
                   "execution": {
                     "visits": {
                       "matches": []
                     }
                   }
                 }
               }
             """.asRight
           )
    yield ()
