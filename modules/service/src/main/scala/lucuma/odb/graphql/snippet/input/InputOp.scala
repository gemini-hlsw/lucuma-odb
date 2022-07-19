package lucuma.odb.graphql.snippet.input

sealed trait InputOp
object InputOp {
  object Create extends InputOp
  object Edit extends InputOp
}