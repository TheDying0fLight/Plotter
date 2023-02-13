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
  
  def evalFun(functions: List[(String,Color,Int)]) = {
    val graph = new Group()
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
  
  val grid = Grid(center,stageV,zoom)
  //generating the image
  def image =
    grid.center = center; grid.stageV = stageV; grid.zoom = zoom
    val functs = List(("x^2", Blue, 3), ("x", Red, 2))
    new Group(grid.getCoordinateSystem, evalFun(functs))

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