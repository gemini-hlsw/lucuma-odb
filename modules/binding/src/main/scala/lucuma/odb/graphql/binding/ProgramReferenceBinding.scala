// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.core.model.ProgramReference

val ProgramReferenceBinding: Matcher[ProgramReference] =
  StringBinding.emap { s =>
    ProgramReference
      .fromString
      .getOption(s)
      .toRight(s"'$s' cannot be parsed as a valid ProgramReference.")
  }
