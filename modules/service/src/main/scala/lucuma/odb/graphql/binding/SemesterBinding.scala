// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.core.model.Semester
import lucuma.odb.data.ProgramReference

val SemesterBinding: Matcher[Semester] =
  StringBinding.emap { s =>
  ProgramReference.parse.semester.parseAll(s).toOption
    .orElse(ProgramReference.parse.shortSemester.parseAll(s).toOption)
    .toRight(s"'$s' cannot be parsed as a valid Semester.")
  }
