// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.util

import cats.Order
import cats.Show
import cats.syntax.eq.*
import cats.syntax.option.*
import lucuma.core.optics.Format

import java.util.HexFormat
import scala.collection.immutable.LazyList
import scala.util.control.Exception.nonFatalCatch

// TODO: this probably belongs in core

/**
 * A git commit hash value.
 */
opaque type CommitHash = Array[Byte]

object CommitHash {

  private val hexFormat: HexFormat =
    HexFormat.of

  def parse(commitHash: String): Option[CommitHash] =
    nonFatalCatch.opt {
      hexFormat.parseHex(commitHash)
    }.filter(_.length === 20)

  def unsafeParse(commitHash: String): CommitHash =
    parse(commitHash).getOrElse(sys.error(s"`$commitHash` is not a valid commit hash"))

  val Zero: CommitHash =
    unsafeParse("0000000000000000000000000000000000000000")

  extension (c: CommitHash) {

    def toByteArray: Array[Byte] =
      c

    def format: String =
      hexFormat.formatHex(c)

    def toString: String =
      format

  }

  given Show[CommitHash] =
    Show.show(hexFormat.formatHex)

  given Order[CommitHash] =
    Order.from { (h0, h1) =>
      LazyList.from(h0).zip(h1).find {
        case (a0, a1) => a0 =!= a1
      }.map {
        case (a0, a1) => a0 - a1
      }.getOrElse(0)
    }

  val FromString: Format[String, CommitHash] =
    Format(parse, _.format)

}
