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
import scalafx.scene.control.ButtonType
import scalafx.scene.control.ButtonBar.ButtonData

class Functions(var stageWidth: Int, var centerDragged: (Double,Double), var zoom: Double):
  var functionList = ArrayBuffer[(String, Color, Int)]()

  def popUp = {
    val functionPopUp = new Dialog {title = "Function Input"}

    functionPopUp.getDialogPane().getButtonTypes().add(ButtonType("Done", ButtonData.OKDone))
  
    var inputFields = ArrayBuffer[(TextField,Boolean)]()

    def addInputField(function: String, color: Color, thickness: Int, index: Int): Unit =
      val field = new TextField{promptText = "Input a Function"; text = function}
      field.text.onChange{(_, _, newValue) => 
        functionList(index) = (newValue, color, thickness)
        MainWindow.update
        if (inputFields(index)(1) && newValue != "")
          inputFields(index) = (inputFields(index)(0),false)
          addInputField("", Blue, 2, inputFields.length)}
      inputFields += ((field,true))

    functionList.zipWithIndex.foreach{case((function,color,thickness),index) =>
      addInputField(function, color, thickness, index)}
  
    val grid = new GridPane() {
      hgap = 10
      vgap = 10
      padding = Insets(20, 100, 10, 10)
    }

    inputFields.zipWithIndex.foreach{case(field,index) =>
      grid.add(new Label(s"f${index+1}:"), 0, index)
      grid.add(field(0), 1, index)}

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