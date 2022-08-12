// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.odb.graphql.util.Bindings._

def gidBinding[A: Gid](name: String): Matcher[A] =
  StringBinding.emap { s =>
    Gid[A].fromString.getOption(s).toRight(s"'$s' is not a valid $name id")
  }

val ObservationIdBinding = gidBinding[Observation.Id]("observation")
val ProgramIdBinding     = gidBinding[Program.Id]("program")
val TargetIdBinding      = gidBinding[Target.Id]("target")
val UserIdBinding        = gidBinding[User.Id]("user")
