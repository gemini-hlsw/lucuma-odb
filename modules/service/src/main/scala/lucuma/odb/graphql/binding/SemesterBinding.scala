// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.core.model.Semester
import lucuma.odb.data.ProgramReference.parse.semester
import lucuma.odb.data.ProgramReference.parse.shortSemester

val SemesterBinding: Matcher[Semester] =
  StringBinding.emap { s =>
    semester.parseAll(s).toOption
      .orElse(shortSemester.parseAll(s).toOption)
      .toRight(s"'$s' is not a valid semester.")
  }