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

object WindowFx extends JFXApp3 {

  var anchorPt: Point2D = null
  var drag = new Point2D(0,0)
  var center = new Point2D(0,0)
  var centerRec = new Point2D(0,0)

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
      graph.children.add(axis(temp,SkyBlue,1))
      temp += 50}
    while (temp <= border) {
      if (!(temp==center)) graph.children.add(axis(temp,Blue,1))
      temp += 50
      graph.children.add(axis(temp,SkyBlue,1))
      temp += 50
    }
    graph
  }

  def xAxisNum(yV: Double, num: Double) = new Text (x = stage.width.value/2-5, y = yV, t = num.toString)

  def numbers(graph: Group, center: Double, border: Double, num: (Double,Double) => Text) = {
    var temp = center%50
    if(!(temp==center%100)) {
      graph.children.add(num(temp, temp))
      temp += 50}
    while (temp <= border) {
      if (!(temp==center)) graph.children.add(num(temp, temp))
      temp += 50
      graph.children.add(num(temp, temp))
      temp += 50
    }
    graph
  }

  def image = {
    var graph = new Group()
    var xMain = xAxisRec(center.y,Black,4)
    var yMain = yAxisRec(center.x,Black,4)
    graph = grid(graph, xAxisRec, center.y, stage.height.value)
    graph = grid(graph, yAxisRec, center.x, stage.width.value)
    graph.children.addAll(xMain,yMain)
    graph = numbers(graph, center.y, stage.height.value, xAxisNum)
    graph
  }

  //Repeating loop with short wait
  def loop(update: () => Unit): Unit =
    Future {
      update()
      Thread.sleep(1)
    }.flatMap(_ => Future(loop(update)))

  def initDrag() = {
    val scene = stage.getScene()
    scene.onMousePressed = (event: MouseEvent) => anchorPt = new Point2D(event.screenX, event.screenY)
    scene.onMouseDragged = (event: MouseEvent) => {
      drag = new Point2D(event.screenX - anchorPt.x + drag.x, event.screenY - anchorPt.y + drag.y)
      anchorPt = new Point2D(event.screenX, event.screenY)
      center = new Point2D(stage.width.value/2+drag.x, stage.height.value/2+drag.y)}
  }

  //Initalization of programm
  override def start(): Unit = {
    val frame = IntegerProperty(0)
    //val state = ObjectProperty(State(List()))

    //Graphical Window of Application
    stage = new JFXApp3.PrimaryStage {
      width = 1280
      height = 720
      scene = new Scene {
        title = "Plotter"
        fill = tuple32JfxColor(250,250,250)
        frame.onChange(Platform.runLater {content = image})
      }
    }
    center = new Point2D(stage.width.value/2+drag.x, stage.height.value/2+drag.y)
    //temporary fix for changing window size
    frame.onChange(Platform.runLater {center = new Point2D(stage.width.value/2+drag.x, stage.height.value/2+drag.y)})

    initDrag()
    //loop to update image
    loop(() => frame.update(frame.value + 1))
  }
}