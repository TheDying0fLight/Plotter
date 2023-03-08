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
import scalafx.scene.layout.VBox
import plotter.MainWindow.stageSizeXY

class Functions(var stageWidth: Int, var centerDragged: (Double,Double), var zoom: Double):
  var functionList = ArrayBuffer[(String, Color, Int)]()

  def popUp = {
    val functionPopUp = new Dialog {title = "Function Input"}

    var inputFields = ArrayBuffer[(TextField,Boolean)]()

    functionPopUp.getDialogPane.getButtonTypes.add(ButtonType("Done", ButtonData.OKDone))
  
    val grid = new GridPane {
      prefHeight = 300
      hgap = 10
      vgap = 10
      padding = Insets(20, 100, 10, 10)
    }

    def addInputField(function: String, color: Color, thickness: Int, index: Int): Unit = {
      if (index == functionList.length) functionList += ((function, color, thickness))

      val functionInput = new TextField{promptText = "Input a Function"; text = function}
      grid.add(new Label(s"f${index+1}:"), 0, index)
      grid.add(functionInput, 1, index)
      functionInput.text.onChange{(_, _, newValue) =>
        functionList(index) = (newValue, functionList(index)(1), functionList(index)(2))
        MainWindow.update
        if (inputFields(index)(1) && newValue != "")
          inputFields(index) = (inputFields(index)(0),false)
          addInputField("", Blue, 3, inputFields.length)}

      val colorInput = new TextField{promptText = "Input a Color"; text = color.toString}
      grid.add(new Label(s"Color ${index+1}:"), 2, index)
      grid.add(colorInput, 3, index)
      colorInput.text.onChange{(_,_,newValue) =>
        try
          functionList(index) = (functionList(index)(0), valueOf(newValue), functionList(index)(2))
          MainWindow.update
        catch
          case _ => println("input is no valid Color")}

      val thicknessInput = new TextField{promptText = "Input a thickness"; text = thickness.toString}
      grid.add(new Label(s"Thickness ${index+1}:"), 4, index)
      grid.add(thicknessInput, 5, index)
      thicknessInput.text.onChange{(_,_,newValue) =>
        if (newValue != "")
          functionList(index) = (functionList(index)(0), functionList(index)(1), newValue.toInt)
          MainWindow.update}

      functionPopUp.dialogPane().content = grid
      if (functionList(index)(0) == "") inputFields += ((functionInput,true))
      else inputFields += ((functionInput,false))
    }

    functionList.zipWithIndex.foreach{case((function,color,thickness),index) =>
      addInputField(function, color, thickness, index)}

    if (inputFields.length == 0) addInputField("", Blue, 3, inputFields.length)
    functionPopUp.show()
  }

  def evalFunctions = {
    val graph = new Group()
    functionList.map((fun, c, th) =>
      if (fun != "")
        try
          var e = new ExpressionBuilder(fun).variable("x").build()
          var poly = new Polyline {stroke = c; strokeWidth = th}
          var prevTemp = 0.0
          var i = 0
          while (i <= stageWidth)
            e.setVariable("x", ((i-centerDragged(0)) / zoom))
            try
              var temp = -(e.evaluate() * zoom) + centerDragged(1)
              if ((prevTemp > stageSizeXY(1) && temp > stageSizeXY(1)))
                while (temp > stageSizeXY(1))
                  i += 1
                  var temp = -(e.evaluate() * zoom) + centerDragged(1)
                poly = new Polyline {stroke = c; strokeWidth = th}
                poly.getPoints().addAll(i.toDouble, prevTemp)
              else if ((prevTemp < 0 && temp < 0) )
                while (temp < 0)
                  i += 1
                  var temp = -(e.evaluate() * zoom) + centerDragged(1)
                poly = new Polyline {stroke = c; strokeWidth = th}
                poly.getPoints().addAll(i.toDouble, prevTemp)
              poly.getPoints().addAll(i.toDouble, temp)
            catch
              case _ => {graph.children.add(poly); poly = new Polyline {stroke = c; strokeWidth = th}}
            i += 1
          graph.children.add(poly)
        catch
          case _ => println("Invalid function")
    )
    graph
  }