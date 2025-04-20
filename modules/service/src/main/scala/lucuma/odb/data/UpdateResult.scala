// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

/** Possible results from an attempted database update. */
sealed trait UpdateResult[+A]

object UpdateResult {
  case object NothingToBeDone      extends UpdateResult[Nothing]
  case object NoSuchObject         extends UpdateResult[Nothing]
  case class  Success[A](value: A) extends UpdateResult[A]
}
