// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.core.model.sequence.Dataset

val DatasetFilenameBinding: Matcher[Dataset.Filename] =
  StringBinding.emap { s =>
    Dataset.Filename.FromString.getOption(s).toRight(s"Invalid dataset filename: $s")
  }
