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
import scalafx.scene.input.ScrollEvent
import scalafx.stage.{WindowEvent, StageStyle}
import scalafx.Includes._

//for the numbers
import scalafx.geometry.VPos._
import scalafx.scene.text._

import scala.concurrent.ExecutionContext.Implicits.global
import net.objecthunter.exp4j.ExpressionBuilder
import scala.collection.mutable.ArrayBuffer
import scalafx.scene.shape.Polyline



object Window extends JFXApp3 {
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
        frame.onChange(Platform.runLater {content = image})
      }
    }
    center = ((stage.width.value/2+drag._1).toInt, (stage.height.value/2+drag._2).toInt)
    stageV = (stage.width.value.toInt, stage.height.value.toInt)
    initMouse()
    //loop to update image
    loop(() => frame.update(frame.value + 1))

    stage.height.onChange(update())
    stage.width.onChange(update())
    state.onChange(update())
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
    var d = 8
    y = if(center._2+5-d+5<0) d
      else if (center._2+10>stageV._2) stageV._2-15
      else center._2+10
    text = "%.2f".format(-num).toDouble.toString //String.format("%2.1e",num)
    textOrigin = Center
    wrappingWidth = 100
    textAlignment = TextAlignment.Center
    font = Font.font("Arial", FontWeight.Bold, FontPosture.Regular, 12)}
  }

  def yAxisNum(yV: Int, num: Double) = {
    new Text {
    x = if(center._1-20<0) -80
      else if (center._1>stageV._1) stageV._1-115
      else center._1-105
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
      if (!(temp==center)) graph.children.add(num((temp*(zoom/100)).toInt, (center-temp)/100))
      temp += 100
    }
    graph
  }
  
  def evalFun(graph: Group, functions: List[(String,Color,Int)]) = {
    functions.map((fun,c,th) => 
        var e = new ExpressionBuilder(fun).variable("x").build()
        var poly = new Polyline{stroke = c; strokeWidth = th}
        for (i <- 0 to stageV._1)
          e.setVariable("x", (i-center._1)/zoom)
          var temp = -(e.evaluate()*zoom)+center._2
          poly.getPoints().addAll(i.toDouble, temp)
        graph.children.add(poly))
    graph
  }

  //generating the image
  def image =
    var graph = new Group()
    val xMain = xAxisRec(center._2,Black,2)
    val yMain = yAxisRec(center._1,Black,2)
    var functs = List(("x^2", Blue, 3), ("x", Red, 2))
    graph = grid(graph, xAxisRec, center._2, stageV._2)
    graph = grid(graph, yAxisRec, center._1, stageV._1)
    graph.children.addAll(xMain,yMain)
    graph = evalFun(graph, functs)
    graph = numbers(graph, center._1, stageV._1, xAxisNum)
    graph = numbers(graph, center._2, stageV._2, yAxisNum)
    graph

  //Repeating loop with short wait
  def loop(update: () => Unit): Unit = {
    Future {
      update()
      Thread.sleep(1000/60)
    }.flatMap(_ => Future(loop(update)))
  }

  var anchorPt: (Double, Double) = (0,0)
  var drag: (Double, Double) = (0,0)
  var dragInt = (0,0)
  var zoom: Double = 100
  
  def initMouse() =
    val scene = stage.getScene()
    scene.onMousePressed = (event: MouseEvent) => anchorPt = (event.screenX, event.screenY)
    scene.onMouseDragged = (event: MouseEvent) => 
      drag = (event.screenX-anchorPt._1+drag._1, event.screenY-anchorPt._2+drag._2)
      anchorPt = (event.screenX, event.screenY)
      state.update(state.value+1)
    scene.onScroll = (event: ScrollEvent) => zoom = zoom+(event.deltaY*zoom/100)/10

  def update() = 
    Platform.runLater {
      center = ((stage.width.value/2+drag._1).toInt, (stage.height.value/2+drag._2).toInt)
      stageV = (stage.width.value.toInt, stage.height.value.toInt)
      dragInt = (drag._1.toInt, drag._2.toInt)
    }
}