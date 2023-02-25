package plotter

//for the scene
import scalafx.application.{JFXApp3, Platform}
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.beans.property.{IntegerProperty, ObjectProperty}
import scala.concurrent.Future
import scalafx.scene.Group

//for mouse interaction
import scalafx.geometry.Point2D
import scalafx.scene.input.MouseEvent
import scalafx.scene.input.ScrollEvent
import scalafx.stage.{WindowEvent, StageStyle}
import scalafx.Includes._

import scala.concurrent.ExecutionContext.Implicits.global
import net.objecthunter.exp4j.ExpressionBuilder
import scala.collection.mutable.ArrayBuffer
import scalafx.scene.shape.Polyline
import scalafx.scene.text.Text
import java.math.MathContext
import javafx.scene.input.KeyCode

object MainWindow extends JFXApp3 {
  // Initalization of programm
  override def start(): Unit = {
    // Graphical Window of Application
    stage = new JFXApp3.PrimaryStage {
      width = 1280
      height = 720
      scene = new Scene {
        title = "Plotter"
        fill = Color(0.95, 0.95, 0.95, 1)
        frame.onChange(Platform.runLater { content = image })
      }
    }
    update()
    initAction()
    stage.width.onChange(update())
    stage.height.onChange(update())
  }

  var centerDragged: (Double, Double) = (0, 0)
  var stageSizeXY = (0, 0)
  val frame = IntegerProperty(0)

  def update() =
    Platform.runLater {
      centerDragged = (stage.width.value / 2 + drag(0).toDouble, stage.height.value / 2 + drag(1).toDouble)
      stageSizeXY = (stage.width.value.toInt, stage.height.value.toInt)
      frame.update(frame.value + 1)
    }

  val functions = Functions(stage, stageSizeXY(0), centerDragged, zoom)
  val grid = Grid(centerDragged, stageSizeXY, zoom)
  val functs = List(("x^2", Blue, 3), ("x", Red, 2), ("sin(x^2)", Pink, 4))

  // generating the image
  def image =
    val zoomText = new Text {x = 10; y = 15; text = s"Zoom: ${BigDecimal(zoom, new MathContext(3)).toString()} %"}
    grid.centerDraggedXY = centerDragged; grid.stageSizeXY = stageSizeXY; grid.zoom = zoom
    functions.stageWidth = stageSizeXY(0); functions.centerDragged = centerDragged; functions.zoom = zoom
    new Group(functions.evalFunctions, grid.getCoordinateSystem, zoomText)

  var drag: (BigDecimal, BigDecimal) = (0, 0)
  var zoom: Double = 100
  val zoomSpeed = 2

  def initAction() = {
    var anchorPt: (Double, Double) = (0, 0)
    val scene = stage.getScene()
    scene.onMousePressed = (event: MouseEvent) =>
      anchorPt = (event.screenX, event.screenY)
    scene.onMouseDragged = (event: MouseEvent) =>
      drag = (
        event.screenX - anchorPt(0) + drag(0),
        event.screenY - anchorPt(1) + drag(1)
      )
      anchorPt = (event.screenX, event.screenY)
      update()
    scene.onScroll = (event: ScrollEvent) => zoomer(event)
    scene.setOnKeyPressed(e => {e.getCode() match
      case KeyCode.F => functions.popUp
      case _ => println("Button has no function")})
  }

  def zoomer(event: ScrollEvent) = {
    var newZoom = (zoom + zoomSpeed * (event.deltaY * zoom / 100) / 10)
    if (newZoom > 1E10) newZoom = 1E10
    else if (newZoom < 1E-10) newZoom = 1E-7
    val zoomFactor = newZoom / zoom
    val relativeMousePosition =
      List(event.getX() - centerDragged(0), event.getY() - centerDragged(1))
    val newRelativeMousePosition =
      relativeMousePosition.map(x => x * zoomFactor)
    drag = (
      drag(0) + relativeMousePosition(0) - newRelativeMousePosition(0),
      drag(1) + relativeMousePosition(1) - newRelativeMousePosition(1)
    )
    zoom = newZoom
    update()
  }
}
