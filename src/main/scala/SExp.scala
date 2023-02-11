package plotter

/** An enum representing simple s-expressions
 */
enum SExp {

  /** A s-expression representing an atom.
   *
   * @param value the value, either a [[String]] or a [[Double]]
   */
  case Atom(value: String | Double)

  /** A s-expression representing an operation.
   *
   * @param name the name of the operator
   * @param args the arguments of the operation, as s-expressions
   */
  case Operation(name: String, args: List[SExp])
}
