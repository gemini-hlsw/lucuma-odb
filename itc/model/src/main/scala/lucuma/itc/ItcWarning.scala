// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import io.circe.Decoder
import io.circe.Encoder

case class ItcWarning(msg: String) derives Decoder, Encoder.AsObject
