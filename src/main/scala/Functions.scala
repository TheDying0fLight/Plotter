package plotter

import scalafx.application.JFXApp3.PrimaryStage
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
import javafx.application.Application

class Functions(stage: PrimaryStage, var stageWidth: Int, var centerDragged: (Double,Double), var zoom: Double):
  var functionList = ArrayBuffer[(String, Color, Int)]()

  val functionPopUp = new Dialog() {
    initOwner(stage)
    title = "Function Input"
    headerText = "Input Functions below"
  }

  val prompt1 = new TextField() {
    promptText = "Input a Function"
  }

  val prompt2 = new TextField() {
    promptText = "Input a Function"
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

  prompt1.text.onChange {(_, _, newValue) => functionList(0) = (newValue, Green, 3)}
  prompt2.text.onChange {(_, _, newValue) => functionList(1) = (newValue, Yellow, 2)}

  functionPopUp.dialogPane().content = grid

  def popUp = functionPopUp.showAndWait()

  def evalFunctions = {
    val graph = new Group()
    functionList.map((fun, c, th) =>
      var e = new ExpressionBuilder(fun).variable("x").build()
      var poly = new Polyline { stroke = c; strokeWidth = th }
      for (i <- 0 to stageWidth)
        e.setVariable("x", ((i - centerDragged(0)) / zoom))
        var temp = -(e.evaluate() * zoom) + centerDragged(1)
        poly.getPoints().addAll(i.toDouble, temp+th/2)
      graph.children.add(poly)
    )
    graph
  }