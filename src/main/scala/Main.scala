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

object MainWindow extends JFXApp3 {
  var centerDragged: (Double,Double) = (0,0)
  var stageSizeXY = (0,0)
  val frame = IntegerProperty(0)

  def update() = 
    Platform.runLater {
      centerDragged = (stage.width.value/2+drag._1, stage.height.value/2+drag._2)
      stageSizeXY = (stage.width.value.toInt, stage.height.value.toInt)
      frame.update(frame.value + 1)
    }

  //Initalization of programm
  override def start(): Unit = {
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
    for(i <- 1 to 100)
      update()
    initMouseAction()
    stage.width.onChange(update())
    stage.height.onChange(update())
    //loop to update image
    //loop(() => {frame.update(frame.value + 1); update()})
  }
  
  def evalFun(functions: List[(String,Color,Int)]) = {
    val graph = new Group()
    functions.map((fun,c,th) => 
        var e = new ExpressionBuilder(fun).variable("x").build()
        var poly = new Polyline{stroke = c; strokeWidth = th}
        for (i <- 0 to stageSizeXY._1)
          e.setVariable("x", (i-centerDragged._1)/zoom)
          var temp = -(e.evaluate()*zoom)+centerDragged._2
          poly.getPoints().addAll(i.toDouble, temp)
        graph.children.add(poly))
    graph
  }
  
  val grid = Grid(centerDragged,stageSizeXY,zoom)
  val functs = List(("x^2", Blue, 3), ("x", Red, 2))
  
  //generating the image
  def image =
    val zoomText = new Text{x = 100; y = 100; text = zoom.toString()}
    grid.centerXY = centerDragged; grid.stageSizeXY = stageSizeXY; grid.zoom = zoom
    new Group(grid.getCoordinateSystem, evalFun(functs), zoomText)

  var drag: (Double, Double) = (0,0)
  var zoom: Double = 100
  val zoomSpeed = 2
  
  def initMouseAction() =
    var anchorPt: (Double, Double) = (0,0)
    val scene = stage.getScene()
    scene.onMousePressed = (event: MouseEvent) => anchorPt = (event.screenX, event.screenY) 
    scene.onMouseDragged = (event: MouseEvent) => 
      drag = (event.screenX-anchorPt._1+drag._1, event.screenY-anchorPt._2+drag._2)
      anchorPt = (event.screenX, event.screenY)
      update()
    scene.onScroll = (event: ScrollEvent) => zoomer(event)
  
  def zoomer(event: ScrollEvent) =
    val newZoom = (zoom + zoomSpeed*(event.deltaY*zoom/100)/10)
    val zoomFactor =  newZoom/zoom
    val relativeMousePosition = List(event.getX()-centerDragged._1, event.getY()-centerDragged._2)
    val newDrag = relativeMousePosition.map(x => x * zoomFactor)
    drag = (drag._1+relativeMousePosition(0)-newDrag(0), drag._2+relativeMousePosition(1)-newDrag(1))
    zoom = newZoom
    update()

  //Repeating loop with short wait
  def loop(update: () => Unit): Unit = {
    Future {
      update()
      Thread.sleep(1000/60)
    }.flatMap(_ => Future(loop(update)))
  }
}