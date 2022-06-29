package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all._
import io.circe.literal._
import io.circe.syntax._
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.OdbSuite

class createTarget extends OdbSuite with CreateProgramOps with LinkUserOps with SetAllocationOps {

  val pi       = TestUsers.Standard.pi(nextId, nextId)
  val pi2      = TestUsers.Standard.pi(nextId, nextId)
  val pi3      = TestUsers.Standard.pi(nextId, nextId)
  val ngo      = TestUsers.Standard.ngo(nextId, nextId, Partner.Ca)
  val staff    = TestUsers.Standard.staff(nextId, nextId)
  val admin    = TestUsers.Standard.admin(nextId, nextId)
  val guest    = TestUsers.guest(nextId)
  val service  = TestUsers.service(nextId)

  lazy val validUsers = List(pi, pi2, pi3, ngo, staff, admin, guest, service)

  def createUsers(users: User*): IO[Unit] =
    users.toList.traverse_(createProgramAs) // TODO: something cheaper

  test("[general] create a target") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createTarget(
              programId: ${pid.asJson}
              input: {
                name: "Crunchy Target"
                sidereal: {
                  ra: { degrees: "12.345" }
                  dec: { degrees: "45.678" }
                  epoch: "J2000.000"
                }
                sourceProfile: {
                }
              }) {

            id
            existence
            name
            program {
              id
            }
            sourceProfile {

              point {
                bandNormalized {
                  # brightness
                  sed {
                    stellarLibrary
                    # coolStar
                    # galaxy
                    # planet
                    # quasar
                    # hiiRegion
                    # planetaryNebula
                    # powerLaw
                    # blackBodyTempK
                    # fluxDensities
                  }
                }
                # emissionLines
              }

            }
            sidereal {
              ra {
                hms
                hours
                degrees
                microarcseconds
              }
              dec {
                dms
                degrees
                microarcseconds
              }
              epoch
            }
            nonsidereal {
              des
            }

          }
        }
        """).flatMap { js =>

        println(js)

        val get = js.hcursor
          .downField("createTarget")
          .downField("program")
          .downField("id")
          .as[Program.Id]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, pid)
      }
    }
  }

}