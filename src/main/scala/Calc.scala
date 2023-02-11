package Plotter

import org.mariuszgromada.math.mxparser._
val isCallSuccessful = License.iConfirmNonCommercialUse("John Doe")

//stuff for calculation
object Calculator:
    def eval(functions: List[String], low: Int, high: Int) =
        var x = new Argument("x=0")
        var e = new Expression("1", x)
        functions.map(fun => {e = new Expression(fun, x)
            var lst = List[(Int,Int)]()
            for (i <- low to high) 
                {x.setArgumentValue(i)
                lst = lst :+ (i,(e.calculate().toInt))}
            lst})
