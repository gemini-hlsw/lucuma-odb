// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model

import io.circe.Encoder

final case class TelescopeMode(`type`: TelescopeModeType, programReference: Option[String])
    derives Encoder.AsObject
