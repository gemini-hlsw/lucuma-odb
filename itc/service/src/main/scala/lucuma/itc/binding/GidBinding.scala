// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.core.util.Gid

def gidBinding[A: Gid](name: String): Matcher[A] =
  StringBinding.emap { s =>
    Gid[A].fromString.getOption(s).toRight(s"'$s' is not a valid $name id")
  }
