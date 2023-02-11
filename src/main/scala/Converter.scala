package plotter

import plotter.*
import plotter.SExp.*

sealed abstract class ParseTree
case class ExprTreeLeaf(value: Double) extends ParseTree
case class ExprTreeNode(op: List[Double] => Double, children: List[ParseTree]) extends ParseTree

val Plus: List[Double] => Double = a => a.sum
val Times: List[Double] => Double = a => a.product

object Convertor:
    def conv(sexp: SExp): ParseTree =
        sexp match
            case Atom(v: Double) => ExprTreeLeaf(v)
            case Atom(v: String) => throw new ArithmeticException(s"$v is not defined") //throws exception when expression contains non-constants
        
            case Operation("+", args) => ExprTreeNode(Plus, args.foldLeft(List[ParseTree]()){(accumulator, elem) => accumulator:+ conv(elem)})
            case Operation("*", args) => ExprTreeNode(Times, args.foldLeft(List[ParseTree]()){(accumulator, elem) => accumulator:+ conv(elem)})
            //throws exception when expression contains non defined operation
            case Operation(v, args) => throw new ArithmeticException(s"$v is not defined")

object ExprTreeNode:
    def apply(op: List[Double] => Double, children: ParseTree*) = new ExprTreeNode(op, children.toList)

object ParseTree:
    val expr = ExprTreeNode(Plus, List(ExprTreeNode(Times, List(ExprTreeLeaf(3), ExprTreeLeaf(8))), ExprTreeLeaf(2)))

    def eval(expr: ParseTree): Double = expr match
        case ExprTreeLeaf(value) => value
        case ExprTreeNode(op, children) => op(children map eval)