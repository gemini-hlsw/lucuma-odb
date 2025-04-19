// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import io.circe.ACursor
import io.circe.Decoder

package object graphql {

  implicit class ACursorOps(hc: ACursor) {
    def downFields(fields: String*): ACursor = fields.foldLeft(hc)(_ downField _)
    def require[A: Decoder]: A = hc.as[A].fold(e => sys.error(e.message), identity)
  }

}
