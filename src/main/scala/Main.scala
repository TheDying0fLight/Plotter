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

object WindowFx extends JFXApp3 {

  var anchorPt: Point2D = null
  var drag = new Point2D(0,0)
  var center = new Point2D(0,0)
  var centerRec = new Point2D(0,0)
  var thickness = 2

  def xAxis(yV: Double, color: Color) = new Rectangle {
    width = stage.width.value
    height = thickness
    x = 0
    y = yV
    fill = color
  }

  def yAxis(xV: Double, color: Color) = new Rectangle {
    width = thickness
    height = stage.height.value
    x = xV
    y = 0
    fill = color
  }

  def grid = {
    val xMain = xAxis(centerRec.y, Black)
    val yMain = yAxis(centerRec.x, Black)
    var graph = List(xMain,yMain)
    var temp = center.y%100
    while (temp <= stage.height.value) {
      graph :+ xAxis(temp,Blue)
      temp += 100
    }
    graph :+ xAxis(100,Blue)
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
      center = new Point2D(stage.width.value/2+drag.x, stage.height.value/2+drag.y)
      centerRec = new Point2D(stage.width.value/2+drag.x-thickness/2, stage.height.value/2+drag.y-thickness/2)}}

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
    centerRec = new Point2D(stage.width.value/2+drag.x-thickness/2, stage.height.value/2+drag.y-thickness/2)
    initDrag()
    //loop to update image
    loop(() => frame.update(frame.value + 1))
  }
}