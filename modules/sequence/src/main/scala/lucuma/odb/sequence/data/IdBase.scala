// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import lucuma.core.util.NewType

/**
 * The IdBase is an integer which is folded into the generation of atom (and
 * therefore) step ids.  When the visit or (in the case of acquisition)
 * sequence type changes, the id base is incremented and different ids are
 * produced.
 */
object IdBase:
  object Acq extends NewType[Int]:
    val zero: Acq = apply(0)
  type Acq = Acq.Type

  object Sci extends NewType[Int]:
    val zero: Sci = apply(0)
  type Sci = Sci.Type