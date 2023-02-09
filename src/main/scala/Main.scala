package Plotter

import scalafx.application.{JFXApp3, Platform}
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text
import scalafx.beans.property.{IntegerProperty, ObjectProperty}
import scala.concurrent.Future

import scalafx.geometry.Point2D
import scalafx.scene.input.MouseEvent
import scalafx.stage.{WindowEvent, StageStyle}
import scalafx.Includes._

import scala.concurrent.ExecutionContext.Implicits.global
import scalafx.scene.Group
import scalafx.geometry.VPos._
import scalafx.scene.text.TextAlignment
import scalafx.scene.text.Font
import scalafx.scene.text.FontPosture
import scalafx.scene.text.FontWeight

object WindowFx extends JFXApp3 {

  var anchorPt: (Double, Double) = (0,0)
  var drag: (Double, Double) = (0,0)
  var dragInt = (0,0)
  var center = (0,0)
  var stageV = (0,0)

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
    //temporary fix for changing window size
    frame.onChange(Platform.runLater {
      center = ((stage.width.value/2+drag._1).toInt, (stage.height.value/2+drag._2).toInt)
      stageV = (stage.width.value.toInt, stage.height.value.toInt)
      dragInt = (drag._1.toInt, drag._2.toInt)
    })

    initDrag()
    //loop to update image
    loop(() => frame.update(frame.value + 1))
  }

  def xAxisRec(yV: Double, color: Color, thickness: Double) = new Rectangle {
    x = 0
    y = yV
    width = stage.width.value
    height = thickness
    fill = color
  }

  def yAxisRec(xV: Double, color: Color, thickness: Double) = new Rectangle {
    x = xV-thickness/2
    y = 0
    width = thickness
    height = stage.height.value
    fill = color
  }

  def grid(graph: Group, axis: (Double,Color,Double) => Rectangle, center: Double, border: Double) = {
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

  def xAxisNum(xV: Double, num: Double) = new Text {
    x = xV-50
    y = center._2+10
    text = "%.2f".format(num).toDouble.toString //String.format("%2.1e",num.round)
    textOrigin = Center
    wrappingWidth = 100
    textAlignment = TextAlignment.Center
    font = Font.font("Arial", FontWeight.Bold, FontPosture.Regular, 12)
    //fill = Black
    //strokeWidth = 0.7
    //stroke = White
  }

  def yAxisNum(yV: Double, num: Double) = new Text {
    x = center._1-105
    y = yV
    text = "%.2f".format(num).toDouble.toString //String.format("%2.1e",num)
    textOrigin = Center
    wrappingWidth = 100
    textAlignment = TextAlignment.Right
    font = Font.font("Arial", FontWeight.Bold, FontPosture.Regular, 12)
  }

  def numbers(graph: Group, center: Double, border: Double, drag: Double, num: (Double,Double) => Text) = {
    var temp = center%100
    while (temp <= border) {
      if (!(temp==center)) graph.children.add(num(temp, ((center-temp))))
      temp += 100
    }
    graph
  }

  var odx: Double = 0
  var ocx: Double = 0
  var ow: Double = 0

  def image = {
    var b = false
    if (odx != dragInt._1 || center._1 != ocx || stageV._1 != ow) b=true
    odx=dragInt._1
    ocx=center._1
    ow=stageV._1
    if (b) println(s"width: $ow, dragInt.x: $odx, center.x: $ocx")
    var graph = new Group()
    var xMain = xAxisRec(center._2,Black,2)
    var yMain = yAxisRec(center._1,Black,2)
    graph = grid(graph, xAxisRec, center._2, stageV._2)
    graph = grid(graph, yAxisRec, center._1, stageV._1)
    graph.children.addAll(xMain,yMain)
    graph = numbers(graph, center._1, stageV._1, dragInt._1, xAxisNum)
    graph = numbers(graph, center._2, stageV._2, dragInt._2, yAxisNum)
    graph
  }

  //Repeating loop with short wait
  def loop(update: () => Unit): Unit =
    Future {
      update()
      Thread.sleep(1000/60)
    }.flatMap(_ => Future(loop(update)))

  def initDrag() = {
    val scene = stage.getScene()
    scene.onMousePressed = (event: MouseEvent) => anchorPt = (event.screenX, event.screenY)
    scene.onMouseDragged = (event: MouseEvent) => {
      drag = ((event.screenX-anchorPt._1+drag._1), (event.screenY-anchorPt._2+drag._2))
      anchorPt = (event.screenX, event.screenY)
    }
  }
}