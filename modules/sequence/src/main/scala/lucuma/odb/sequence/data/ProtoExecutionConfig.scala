// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.syntax.apply.*
import fs2.Pure
import fs2.Stream

/**
 * A precursor to an InstrumentConfig, which contains a concrete list of
 * steps.  The ProtoExecutionConfig provides just a `Stream` of steps of a
 * generic type because there are multiple uses for a sequence. For example to
 * calculate the sequence digest we never need to hold the whole sequence in
 * memory but instead can fold over the stream.
 *
 * @tparam S static execution config
 * @tparam A step type
 */
case class ProtoExecutionConfig[S, A](
  static:      S,
  acquisition: Stream[Pure, A],
  science:     Stream[Pure, A]
)