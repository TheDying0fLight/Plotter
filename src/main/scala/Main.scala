package plotter

//for the scene
import scalafx.application.{JFXApp3, Platform}
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text
import scalafx.beans.property.{IntegerProperty, ObjectProperty}
import scala.concurrent.Future
import scalafx.scene.Group

//for mouse interaction
import scalafx.geometry.Point2D
import scalafx.scene.input.MouseEvent
import scalafx.stage.{WindowEvent, StageStyle}
import scalafx.Includes._

//for the numbers
import scalafx.geometry.VPos._
import scalafx.scene.text.TextAlignment
import scalafx.scene.text.Font
import scalafx.scene.text.FontPosture
import scalafx.scene.text.FontWeight

import scala.concurrent.ExecutionContext.Implicits.global

import org.mariuszgromada.math.mxparser._
val isCallSuccessful = License.iConfirmNonCommercialUse("John Doe")

object Window extends JFXApp3 {
  var anchorPt: (Double, Double) = (0,0)
  var drag: (Double, Double) = (0,0)
  var dragInt = (0,0)
  var center = (0,0)
  var stageV = (0,0)
  val state = IntegerProperty(0)

  //Initalization of programm
  override def start(): Unit = {
    val frame = IntegerProperty(0)
    //Graphical Window of Application
    stage = new JFXApp3.PrimaryStage {
      width = 1280
      height = 720
      scene = new Scene {
        title = "Plotter"
        fill = Color(0.95,0.95,0.95,1)
        state.onChange(Platform.runLater {content = image})
      }
    }
    center = ((stage.width.value/2+drag._1).toInt, (stage.height.value/2+drag._2).toInt)
    stageV = (stage.width.value.toInt, stage.height.value.toInt)
    //temporary fix for changing window size
    state.onChange(Platform.runLater {
      center = ((stage.width.value/2+drag._1).toInt, (stage.height.value/2+drag._2).toInt)
      stageV = (stage.width.value.toInt, stage.height.value.toInt)
      dragInt = (drag._1.toInt, drag._2.toInt)
    })
    initDrag()
    //loop to update image
    state.update(state.value + 1)
    //loop(() => frame.update(frame.value + 1))
  }

  //templates for grid lines along the x and y Axis
  def xAxisRec(yV: Int, color: Color, thickness: Int) = {
    new Rectangle {
    x = 0
    y = yV
    width = stage.width.value
    height = thickness
    fill = color}
  }

  def yAxisRec(xV: Int, color: Color, thickness: Int) = {
    new Rectangle {
    x = xV-thickness/2
    y = 0
    width = thickness
    height = stage.height.value
    fill = color}
  }

  def grid(graph: Group, axis: (Int,Color,Int) => Rectangle, center: Int, border: Int) = {
    var temp = center%50
    if(!(temp==center%100)) {
      graph.children.add(axis(temp,Color(0,0,0,0.2),1))
      temp += 50}
    while (temp <= border) {
      if (!(temp==center)) graph.children.add(axis(temp,Color(0,0,0,0.5),1))
      temp += 50
      graph.children.add(axis(temp,Color(0,0,0,0.2),1))
      temp += 50
    }
    graph
  }

  //templates for numbers on x and y Axis
  def xAxisNum(xV: Int, num: Double) = {
    new Text {
    x = xV-50
    y = center._2+10
    text = "%.2f".format(-num).toDouble.toString //String.format("%2.1e",num)
    textOrigin = Center
    wrappingWidth = 100
    textAlignment = TextAlignment.Center
    font = Font.font("Arial", FontWeight.Bold, FontPosture.Regular, 12)}
  }

  def yAxisNum(yV: Int, num: Double) = {
    new Text {
    x = center._1-105
    y = yV
    text = "%.2f".format(num).toDouble.toString //String.format("%2.1e",num)
    textOrigin = Center
    wrappingWidth = 100
    textAlignment = TextAlignment.Right
    font = Font.font("Arial", FontWeight.Bold, FontPosture.Regular, 12)}
  }

  def numbers(graph: Group, center: Int, border: Int, num: (Int,Double) => Text) = {
    var temp = center%100
    while (temp <= border) {
      if (!(temp==center)) graph.children.add(num(temp, (center-temp)/100))
      temp += 100
    }
    graph
  }

  def point(xV: Double, yV: Double, color: Color, thickness: Int) = {
    new Rectangle {
      x = xV-thickness/2
      y = yV-thickness/2
      fill = color
      width = thickness
      height = thickness}
  }
  
  def evalFun(graph: Group, functions: List[(String,Color,Int)]) = {
    // val raw = Calculator.eval(functions.map((f,_,_) => f), 1, 100)
    // for i <- 0 to functions.length-1
    // do for j <- 0 to raw(i).size-1
    //   do graph.children.add(point(raw(i)(j)._2, center._2-raw(i)(j)._2, functions(i)._2, functions(i)._3))
    // graph
    var x = new Argument("x")
    functions.map((fun,c,th) => 
        var e = new Function("f", fun, "x")
        for (i <- 0 to stageV._1 by th)
            //if (e.calculate()<=drag._2+stageV._2 && e.calculate()>=drag._2-stageV._2) 
            graph.children.add(point(i,e.calculate((i-center._1)/100)*100.toInt+center._2, c, th)))
    graph
  }

  //generating the image
  def image =
    var graph = new Group()
    var xMain = xAxisRec(center._2,Black,2)
    var yMain = yAxisRec(center._1,Black,2)
    var functs = List(("x^2", Blue, 3), ("x", Red, 2))
    graph = grid(graph, xAxisRec, center._2, stageV._2)
    graph = grid(graph, yAxisRec, center._1, stageV._1)
    graph.children.addAll(xMain,yMain)
    graph = evalFun(graph, functs)
    graph = numbers(graph, center._1, stageV._1, xAxisNum)
    graph = numbers(graph, center._2, stageV._2, yAxisNum)
    graph

  //Repeating loop with short wait
  // def loop(update: () => Unit): Unit = {
  //   Future {
  //     update()
  //     Thread.sleep(1000/60)
  //   }.flatMap(_ => Future(loop(update)))
  // }
  
  def initDrag() =
    val scene = stage.getScene()
    scene.onMousePressed = (event: MouseEvent) => anchorPt = (event.screenX, event.screenY)
    scene.onMouseDragged = (event: MouseEvent) => 
      drag = ((event.screenX-anchorPt._1+drag._1), (event.screenY-anchorPt._2+drag._2))
      anchorPt = (event.screenX, event.screenY)
      state.update(state.value + 1)
}