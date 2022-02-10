package lucuma.odb

import io.circe.{ ACursor, Decoder }

package object graphql {

  implicit class ACursorOps(hc: ACursor) {
    def downFields(fields: String*) = fields.foldLeft(hc)(_ downField _)
    def require[A: Decoder]: A = hc.as[A].fold(e => sys.error(e.message), identity)
  }

}
