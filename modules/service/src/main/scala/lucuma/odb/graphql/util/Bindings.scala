package lucuma.odb.graphql.util

import edu.gemini.grackle.Query.Binding
import edu.gemini.grackle.Value._
import edu.gemini.grackle.EnumValue
import edu.gemini.grackle.Value
import edu.gemini.grackle.Result
import cats.syntax.all._

object Bindings {

  trait Matcher[A] { outer =>

    def validate(v: Value): Either[String, A]

    final def validate(b: Binding): Result[A] =
      validate(b.value) match {
        case Left(error)  => Result.failure(s"Argument '${b.name}' is invalid: $error")
        case Right(value) => Result(value)
      }

    final def map[B](f: A => B): Matcher[B] = v =>
      outer.validate(v).map(f)

    final def emap[B](f: A => Either[String, B]): Matcher[B] = v =>
      outer.validate(v).flatMap(f)

    final def unapply(b: Binding): Some[(String, Result[A])] =
      Some((b.name, validate(b)))

    final def unapply(kv: (String, Value)): Some[(String, Result[A])] =
      unapply(Binding(kv._1, kv._2))

    lazy val Nullable: Matcher[Option[A]] = {
      case NullValue => Right(None)
      case other     => outer.validate(other).map(Some(_))
    }

    lazy val Optional: Matcher[Option[A]] = {
      case AbsentValue => Right(None)
      case other       => outer.validate(other).map(Some(_))
    }

    lazy val NullableOptional: Matcher[Option[A]] = {
      case NullValue | AbsentValue => Right(None)
      case other                   => outer.validate(other).map(Some(_))
    }

    lazy val OptionalNullable: Matcher[Option[Option[A]]] = {
      case AbsentValue => Right(None)
      case NullValue   => Right(Some(None))
      case other       => outer.validate(other).map(a => Some(Some(a)))
    }

    lazy val List: Matcher[List[A]] =
      ListBinding.emap { vs =>
        // This fast-fails on the first invalid one, which is the best we can do
        vs.zipWithIndex.traverse { case (v, n) => validate(v).leftMap(s => s"at index $n: $s") }
      }

  }

  val IntBinding:             Matcher[Int]         = v => v match { case IntValue(value)            => Right(value) ; case other => Left(s"expected Int, found $other.") }
  val FloatBinding:           Matcher[Double]      = v => v match { case FloatValue(value)          => Right(value) ; case other => Left(s"expected Float, found $other.") }
  val StringBinding:          Matcher[String]      = v => v match { case StringValue(value)         => Right(value) ; case other => Left(s"expected String, found $other.") }
  val BooleanBinding:         Matcher[Boolean]     = v => v match { case BooleanValue(value)        => Right(value) ; case other => Left(s"expected Boolean, found $other.") }
  val IDBinding:              Matcher[String]      = v => v match { case IDValue(value)             => Right(value) ; case other => Left(s"expected ID, found $other.") }
  val UntypedEnumBinding:     Matcher[String]      = v => v match { case UntypedEnumValue(name)     => Right(name)  ; case other => Left(s"expected UntypedEnum, found $other.") }
  val TypedEnumBinding:       Matcher[EnumValue]   = v => v match { case TypedEnumValue(value)      => Right(value) ; case other => Left(s"expected TypedEnum, found $other.") }
  val UntypedVariableBinding: Matcher[String]      = v => v match { case UntypedVariableValue(name) => Right(name)  ; case other => Left(s"expected UntypedVariable, found $other.") }
  val ListBinding:            Matcher[List[Value]] = v => v match { case ListValue(elems)           => Right(elems) ; case other => Left(s"expected List, found $other.") }

}