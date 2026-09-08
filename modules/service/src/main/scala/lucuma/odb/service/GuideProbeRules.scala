// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.probes
import lucuma.core.syntax.string.*
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*

// ODB-facing wrapper over lucuma-core's probe rules
object GuideProbeRules:

  def notAllowedMessage(mode: ObservingModeType, probe: GuideProbe): String =
    s"Guide probe ${probe.tag.toScreamingSnakeCase} cannot be used with observing mode ${mode.tag.toScreamingSnakeCase}."

  def check(mode: ObservingModeType, probe: GuideProbe, prefix: String = ""): Result[Unit] =
    if probes.isProbeAllowed(mode, probe) then Result.unit
    else OdbError.InvalidArgument(s"$prefix${notAllowedMessage(mode, probe)}".some).asFailure
