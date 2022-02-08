package lucuma.odb.data

/** Possible results from an attempted database update. */
sealed trait UpdateResult[+A]

object UpdateResult {
  case object NothingToBeDone      extends UpdateResult[Nothing]
  case object NoSuchObject         extends UpdateResult[Nothing]
  case class  Success[A](value: A) extends UpdateResult[A]
}
