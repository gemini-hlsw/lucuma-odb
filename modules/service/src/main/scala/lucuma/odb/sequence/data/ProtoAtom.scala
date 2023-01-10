// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.data.NonEmptyList

/**
 * An atom (list of steps) without an id.
 */
final case class ProtoAtom[D](
  steps: NonEmptyList[ProtoStep[D]]
)


