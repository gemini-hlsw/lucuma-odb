// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import cats.syntax.all._
import io.circe.Json
import lucuma.core.enums.Band
import lucuma.core.util.Enumerated
import lucuma.odb.data.Existence
import lucuma.odb.data.ObsActiveStatus
import lucuma.odb.data.ObsStatus
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.ProgramUserSupportType
import lucuma.odb.graphql.util.Bindings._
import lucuma.core.enums.ToOActivation

def enumeratedBinding[A](implicit ev: Enumerated[A]) =
  TypedEnumBinding.map(b => Json.fromString(b.name)).emap { j =>
    ev.decodeJson(j).leftMap(_.message)
  }

val ExistenceBinding                  = enumeratedBinding[Existence]
val ProgramUserRoleBinding            = enumeratedBinding[ProgramUserRole]
val ProgramUserSupportRoleTypeBinding = enumeratedBinding[ProgramUserSupportType]
val ObsStatusBinding                  = enumeratedBinding[ObsStatus]
val ObsActiveStatusBinding            = enumeratedBinding[ObsActiveStatus]
val BandBinding                       = enumeratedBinding[Band]
val ToOActivationBinding              = enumeratedBinding[ToOActivation]

