package lucuma.odb.graphql.util

import edu.gemini.grackle.Query.Binding
import edu.gemini.grackle.Value._
import edu.gemini.grackle.EnumValue
import edu.gemini.grackle.Value
import edu.gemini.grackle.Result
import cats.syntax.all._
import lucuma.odb.data
import io.circe.Decoder
import io.circe.parser.parse
import java.time.Duration
import cats.syntax.all._
import java.time.format.DateTimeParseException
import io.circe.Json
import lucuma.core.enum.Band

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

    final def rmap[B](f: PartialFunction[A, Result[B]]): Matcher[B] = v =>
      outer.validate(v).flatMap { a =>
        f.lift(a) match {
          case Some(r) => r.toEither.leftMap(_.head.message)
          case None    => Left(s"rmap: unhandled case; no match for $v") // todo: this sucks
        }
      } // only preserves the first problem, rats

    def unapply(b: Binding): Some[(String, Result[A])] =
      Some((b.name, validate(b)))

    final def unapply(kv: (String, Value)): Some[(String, Result[A])] =
      unapply(Binding(kv._1, kv._2))

    lazy val Nullable: Matcher[data.Nullable[A]] = {
      case NullValue   => Right(data.Nullable.Null)
      case AbsentValue => Right(data.Nullable.Absent)
      case other       => outer.validate(other).map(data.Nullable.NonNull(_))
    }

    /** A matcher that treats `NullValue` and `AbsentValue` as `None` */
    lazy val Option: Matcher[Option[A]] =
      Nullable.map(_.toOption)

    /** A matcher that matches a list of `A` */
    lazy val List: Matcher[List[A]] =
      ListBinding.emap { vs =>
        // This fast-fails on the first invalid one, which is the best we can do
        vs.zipWithIndex.traverse { case (v, n) => validate(v).leftMap(s => s"at index $n: $s") }
      }

  }

  /** A primitive non-nullable binding. */
  def primitiveBinding[A](name: String)(pf: PartialFunction[Value, A]): Matcher[A] = {
    case NullValue   => Left(s"cannot be null")
    case AbsentValue => Left(s"is not optional")
    case other =>
      pf.lift(other) match {
        case Some(value) => Right(value)
        case None        => Left(s"expected $name, found $other")
      }
  }

  val IntBinding:             Matcher[Int]         = primitiveBinding("Int")             { case IntValue(value)            => value }
  val FloatBinding:           Matcher[Double]      = primitiveBinding("Float")           { case FloatValue(value)          => value }
  val StringBinding:          Matcher[String]      = primitiveBinding("String")          { case StringValue(value)         => value }
  val BooleanBinding:         Matcher[Boolean]     = primitiveBinding("Boolean")         { case BooleanValue(value)        => value }
  val IDBinding:              Matcher[String]      = primitiveBinding("ID")              { case IDValue(value)             => value }
  val UntypedEnumBinding:     Matcher[String]      = primitiveBinding("UntypedEnum")     { case UntypedEnumValue(name)     => name }
  val TypedEnumBinding:       Matcher[EnumValue]   = primitiveBinding("TypedEnum")       { case TypedEnumValue(value)      => value }
  val UntypedVariableBinding: Matcher[String]      = primitiveBinding("UntypedVariable") { case UntypedVariableValue(name) => name }
  val ListBinding:            Matcher[List[Value]] = primitiveBinding("List")            { case ListValue(elems)           => elems }
  val ObjectBinding:          Matcher[ObjectValue] = primitiveBinding("Input") { case ov : ObjectValue        => ov }
  val ObjectFieldsBinding = ObjectBinding.map(_.fields)

  val ObjectAsJsonBinding: Matcher[Json] =
    ObjectBinding.emap(ValueAsJson.toJson)

  val DurationBinding: Matcher[Duration] =
    StringBinding.emap(s =>
      Either.catchOnly[DateTimeParseException](Duration.parse(s))
        .leftMap(_ => "Invalid ISO-8601 duration.")
    )

}