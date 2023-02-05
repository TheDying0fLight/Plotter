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

  def xAxis(yV: Double, color: Color, thickness: Double) = new Rectangle {
    x = 0
    y = yV
    width = stage.width.value
    height = thickness
    fill = color
  }

  def yAxis(xV: Double, color: Color, thickness: Double) = new Rectangle {
    x = xV-thickness/2
    y = 0
    width = thickness
    height = stage.height.value
    fill = color
  }

  def grid = {
    var graph = new Group()
    var xMain = xAxis(center.y,Black,4)
    var yMain = yAxis(center.x,Black,4)
    graph.children.addAll(xMain,yMain)
    var temp = center.y%100
    while (temp <= stage.height.value) {
      if (temp==center.y) temp += 100
      graph.children.addOne(xAxis(temp,Blue,1))
      temp += 100
    }
    temp = center.x%100
    while (temp <= stage.width.value) {
      if (temp==center.x) temp += 100
      graph.children.addOne(yAxis(temp,Blue,1))
      temp += 100
    }
    println(graph)
    graph
  }

  /*case class State(fs: List[String]) {

    //def evalFunction(func: String): List[Rectangle] =
    def image(): List[Rectangle] = List(yAxis, xAxis) 
  }*/

  //Reapeating loop with short wait
  def loop(update: () => Unit): Unit =
    Future {
      update()
      Thread.sleep(1)
    }.flatMap(_ => Future(loop(update)))

  def initDrag() ={
    val scene = stage.getScene()
    scene.onMousePressed = (event: MouseEvent) => anchorPt = new Point2D(event.screenX, event.screenY)
    scene.onMouseDragged = (event: MouseEvent) => {
      drag = new Point2D(event.screenX - anchorPt.x + drag.x, event.screenY - anchorPt.y + drag.y)
      anchorPt = new Point2D(event.screenX, event.screenY)
      center = new Point2D(stage.width.value/2+drag.x, stage.height.value/2+drag.y)}}

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
        fill = LightGrey
        frame.onChange(Platform.runLater {content = grid})
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