package plotter

import scalafx.scene.control._
import scalafx.scene.layout.GridPane
import scalafx.geometry.Insets
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import javafx.scene.Group
import net.objecthunter.exp4j.ExpressionBuilder
import scalafx.scene.shape.Polyline
import scalafx.Includes._
import scala.collection.mutable.ArrayBuffer

class Functions(var stageWidth: Int, var centerDragged: (Double,Double), var zoom: Double):
  var functionList = ArrayBuffer[(String, Color, Int)]()

  def popUp = {
    val functionPopUp = new Dialog() {
      title = "Function Input"
    }
  
    val prompt1 = new TextField() {
      promptText = "Input a Function"
      text = functionList(0)(0)
    }
  
    val prompt2 = new TextField() {
      promptText = "Input a Function"
      text = functionList(1)(0)
    }
  
    val grid = new GridPane() {
      hgap = 10
      vgap = 10
      padding = Insets(20, 100, 10, 10)
  
      add(new Label("f1:"), 0, 0)
      add(prompt1, 1, 0)
      add(new Label("f2:"), 0, 1)
      add(prompt2, 1, 1)
    }
  
    prompt1.text.onChange {(_, _, newValue) => functionList(0) = (newValue, Green, 3); MainWindow.update}
    prompt2.text.onChange {(_, _, newValue) => functionList(1) = (newValue, Blue, 2); MainWindow.update}
  
    functionPopUp.dialogPane().content = grid
  }

  def evalFunctions = {
    val graph = new Group()
    functionList.map((fun, c, th) =>
      try
        var e = new ExpressionBuilder(fun).variable("x").build()
        var poly = new Polyline {stroke = c; strokeWidth = th}
        for (i <- 0 to stageWidth)
          e.setVariable("x", ((i-centerDragged(0)) / zoom))
          try
            var temp = -(e.evaluate() * zoom) + centerDragged(1)
            poly.getPoints().addAll(i.toDouble, temp+th/2)
          catch
            case _ => {graph.children.add(poly); poly = new Polyline {stroke = c; strokeWidth = th}}
        graph.children.add(poly)
      catch
        case _ => println("Error")
    )
    graph
  }