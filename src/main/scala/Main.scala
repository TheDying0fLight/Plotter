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

object WindowFx extends JFXApp3 {

  override def start(): Unit = {
    val state = ObjectProperty(State(List()))
    val frame = IntegerProperty(0)

    stage = new JFXApp3.PrimaryStage {
      width = 600
      height = 600
      scene = new Scene {
        fill = White
        frame.onChange(Platform.runLater {
          content = state.value.image
        })
      }
    }
    // Initialize stage to be movable via mouse
    initPan()
    loop(() => frame.update(frame.value + 1))
  }

  import scala.concurrent.ExecutionContext.Implicits.global

  def loop(update: () => Unit): Unit =
    Future {
      update()
      Thread.sleep(1)
    }.flatMap(_ => Future(loop(update)))

  def square(xr: Double, yr: Double, color: Color) = new Rectangle {
    x = xr
    y = yr
    width = 2
    height = 2
    fill = color
  }

  var drag = new Point2D(0, 0)

  def yAxis = new Rectangle {
    x = (drag.x+stage.width.value)/2
    y = 0
    width = 2
    height = stage.height.value
    fill = Black
  }

  def xAxis = new Rectangle {
    x = 0
    y = (drag.y+stage.height.value)/2
    width = stage.width.value
    height = 2
    fill = Black
  }

  case class State(fs: List[String]) {

    //def evalFunction(func: String): List[Rectangle] =

    def image(): List[Rectangle] = List(yAxis, xAxis) 
  }

  /**
   * Initialize the stage to allow the mouse cursor to move the application
   * using dragging.
   */
  var anchorPt: Point2D = null
  def initPan(): Unit = {
    val scene = stage.getScene()
    scene.onMousePressed = (event: MouseEvent) => anchorPt = new Point2D(event.screenX, event.screenY)
    scene.onMouseDragged = (event: MouseEvent) => {
      drag = new Point2D((event.screenX - anchorPt.x)*2 + drag.x, (event.screenY - anchorPt.y)*2 + drag.y)
      anchorPt = new Point2D(event.screenX, event.screenY)}
  }
}