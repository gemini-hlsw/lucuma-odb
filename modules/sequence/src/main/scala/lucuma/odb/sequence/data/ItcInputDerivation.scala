// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.Eq
import cats.derived.*
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.HashBytes

/**
 * The outcome of deriving an observation's ITC input from its parameters.  Three
 * genuinely distinct states, kept explicit so that "this mode has no ITC" is
 * never conflated with "the ITC input could not be built":
 *
 *   - [[ItcInputDerivation.NotApplicable]] — the observing mode has no ITC
 *     concept at all (exchange and visitor modes).
 *   - [[ItcInputDerivation.Incomplete]] — the mode uses the ITC, but the
 *     observation is missing parameters required to build the input.
 *   - [[ItcInputDerivation.Ready]] — the ITC input was built successfully.
 */
enum ItcInputDerivation derives Eq:
  case NotApplicable
  case Incomplete(missing: MissingParamSet)
  case Ready(input: ItcInput)

  /** The successfully derived ITC input, if any. */
  def toOption: Option[ItcInput] =
    this match
      case Ready(input) => Some(input)
      case _            => None

  /** True when the ITC input was derived successfully. */
  def isReady: Boolean =
    this match
      case Ready(_) => true
      case _        => false

object ItcInputDerivation:

  /**
   * Lifts the applicable-mode result — a successfully built input, or the missing
   * parameters that prevented it — into a derivation.  (Not for the modes that
   * have no ITC concept; those are [[NotApplicable]].)
   */
  def fromEither(e: Either[MissingParamSet, ItcInput]): ItcInputDerivation =
    e.fold(Incomplete(_), Ready(_))

  given HashBytes[ItcInputDerivation] with
    def hashBytes(a: ItcInputDerivation): Array[Byte] =
      a match
        case NotApplicable => Array.emptyByteArray
        case Incomplete(m) => m.hashBytes
        case Ready(i)      => i.hashBytes
