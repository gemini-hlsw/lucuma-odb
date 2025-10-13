// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User

class guideTargetNameGMOS extends ExecutionTestSupportForGmos with GuideTargetNameSuite:

  override def createObservationAs(user: User, pid: Program.Id, tids: List[Target.Id]): IO[Observation.Id] =
    createGmosNorthLongSlitObservationAs(user, pid, tids)
