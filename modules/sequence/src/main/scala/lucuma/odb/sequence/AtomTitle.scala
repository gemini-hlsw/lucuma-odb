// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import eu.timepit.refined.types.string.NonEmptyString

/** Atom titles shared across the instrument sequence generators. */
object AtomTitle:

  val NighttimeCal: NonEmptyString =
    NonEmptyString.unsafeFrom("Nighttime Calibrations")

  /** `base` alone without a suffix, `base (suffix)` otherwise. */
  def apply(base: NonEmptyString, suffix: Option[String]): NonEmptyString =
    suffix.fold(base)(s => NonEmptyString.unsafeFrom(s"${base.value} ($s)"))
