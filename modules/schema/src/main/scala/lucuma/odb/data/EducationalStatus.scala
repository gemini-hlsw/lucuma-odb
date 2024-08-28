// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

/**
 * Indicates whether a calibration comes before or after its corresponding
 * science step.
 */
enum EducationalStatus(val tag: String) derives Enumerated:
  case PhD              extends EducationalStatus("phd")
  case GradStudent      extends EducationalStatus("grad_student")
  case UndergradStudent extends EducationalStatus("undergrad_student")
  case Other            extends EducationalStatus("other")
