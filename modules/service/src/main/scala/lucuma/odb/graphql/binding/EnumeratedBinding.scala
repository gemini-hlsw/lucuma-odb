// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import cats.syntax.all._
import io.circe.Json
import lucuma.core.util.Enumerated

def enumeratedBinding[A](implicit ev: Enumerated[A]): Matcher[A] =
  TypedEnumBinding.map(b => Json.fromString(b.name)).emap { j =>
    ev.decodeJson(j).leftMap(_.message)
  }
