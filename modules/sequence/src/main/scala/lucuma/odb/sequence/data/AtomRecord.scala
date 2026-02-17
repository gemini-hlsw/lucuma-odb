// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.Eq
import cats.derived.*
import cats.syntax.eq.*
import lucuma.core.enums.SequenceType
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.util.Timestamp
import lucuma.odb.data.AtomExecutionState

case class AtomRecord(
  id:               Atom.Id,
  visitId:          Visit.Id,
  sequenceType:     SequenceType,
  created:          Timestamp,
  executionState:   AtomExecutionState,
  generatedId:      Option[Atom.Id]
) derives Eq:

  def isAcquisitionSequence: Boolean =
    sequenceType === SequenceType.Acquisition

  def isScienceSequence: Boolean =
    sequenceType === SequenceType.Science