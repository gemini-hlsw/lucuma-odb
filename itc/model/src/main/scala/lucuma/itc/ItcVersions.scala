// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Eq
import cats.derived.*
import io.circe.Decoder
import io.circe.Encoder

// ITC versions reported by the server.  You cannot infer ordering from the
// information but they can be used for equality comparisons.
case class ItcVersions(serverVersion: String, dataVersion: Option[String])
    derives Eq,
      Encoder.AsObject,
      Decoder
