package io.underscore.validation

import scala.language.higherKinds
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class ValidationMacros(val c: Context) {

  import c.universe._

  def field[A, B, F[_], G[_], R[_]](accessor: c.Tree)(validator: c.Tree)(evidence: c.Tree, transformation: c.Tree): c.Tree = {
    val name = accessorName(accessor)
    q"${c.prefix}.field(${name.toString}, _.${name})($validator)($evidence, $transformation)"
  }

  def fieldImplicit[A, B, F[_], G[_], R[_]](accessor: c.Tree)(evidence: c.Tree, validator: c.Tree, transformation: c.Tree): c.Tree = {
    val name = accessorName(accessor)
    q"${c.prefix}.fieldImplicit(${name.toString}, _.${name})($evidence, $validator, $transformation)"
  }

  def fieldWith[A, B, F[_], G[_], R[_]](accessor: c.Tree)(validatorBuilder: c.Tree)(evidence: c.Tree, transformation: c.Tree): c.Tree = {
    val name = accessorName(accessor)
    q"${c.prefix}.fieldWith(${name.toString}, _.${name})($validatorBuilder)($evidence, $transformation)"
  }

  def fieldWithImplicit[A, B, F[_], G[_], R[_]](accessor: c.Tree)(evidence: c.Tree, validatorBuilder: c.Tree, transformation: c.Tree): c.Tree = {
    val name = accessorName(accessor)
    q"${c.prefix}.fieldWithImplicit(${name.toString}, _.${name})($evidence, $validatorBuilder, $transformation)"
  }

  def seqField[A, B, F[_], G[_], R[_]](accessor: c.Tree)(validator: c.Tree)(evidence: c.Tree, transformation: c.Tree): c.Tree = {
    val name = accessorName(accessor)
    q"${c.prefix}.seqField(${name.toString}, _.${name})($validator)($evidence, $transformation)"
  }

  def seqFieldImplicit[A, B, F[_], G[_], R[_]](accessor: c.Tree)(evidence: c.Tree, validator: c.Tree, transformation: c.Tree): c.Tree = {
    val name = accessorName(accessor)
    q"${c.prefix}.seqFieldImplicit(${name.toString}, _.${name})($evidence, $validator, $transformation)"
  }

  def seqFieldWith[A, B, F[_], G[_], R[_]](accessor: c.Tree)(validatorBuilder: c.Tree)(evidence: c.Tree, transformation: c.Tree): c.Tree = {
    val name = accessorName(accessor)
    q"${c.prefix}.seqFieldWith(${name.toString}, _.${name})($validatorBuilder)($evidence, $transformation)"
  }

  def seqFieldWithImplicit[A, B, F[_], G[_], R[_]](accessor: c.Tree)(evidence: c.Tree, validatorBuilder: c.Tree, transformation: c.Tree): c.Tree = {
    val name = accessorName(accessor)
    q"${c.prefix}.seqFieldWithImplicit(${name.toString}, _.${name})($evidence, $validatorBuilder, $transformation)"
  }

  private def accessorName(accessor: c.Tree) = accessor match {
    case q"($param) => $obj.$name" => name
    case other => c.abort(c.enclosingPosition, errorMessage(s"Argument is not an accessor function literal."))
  }

  private def errorMessage(prefix: String) =
    s"""
       |$prefix
       |
     |The argument must be a function literal of the form `fieldName => expression`,
       |where `fieldName` is the name of a field in the object being validated.
       |
     |Alternatively use the `field(fieldName, accessor)(validationFunction)` method,
       |which allows you to specify the field name manually.
     """.stripMargin
}
