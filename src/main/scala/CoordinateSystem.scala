package plotter

import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.Group
import scalafx.geometry.VPos._
import scalafx.scene.text._
import scalafx.scene.paint.PaintIncludes.jfxColor2sfx

class Grid(var center: (Int,Int), var stageV: (Int,Int), var zoom: (Double)):
  var gridColor = Black
  var gridThickness = 2

  def getCoordinateSystem =
    val xMainAxis = xAxisRec(center._2, gridColor, gridThickness)
    val yMainAxis = yAxisRec(center._1, gridColor, gridThickness)
    val xAxisNums = numbers(center._1, stageV._1, xAxisNum)
    val yAxisNums = numbers(center._2, stageV._2, yAxisNum)
    val xSubAxis = grid(xAxisRec, center._2, stageV._2)
    val ySubAxis = grid(yAxisRec, center._1, stageV._1)
    new Group(xMainAxis, yMainAxis, xAxisNums, yAxisNums, xSubAxis, ySubAxis)

  //templates for grid lines along the x and y Axis
  private def xAxisRec(yV: Int, color: Color, thickness: Int) = {
    new Rectangle {
    x = 0
    y = yV
    width = stageV._1
    height = thickness
    fill = color}
  }

  private def yAxisRec(xV: Int, color: Color, thickness: Int) = {
    new Rectangle {
    x = xV-thickness/2
    y = 0
    width = thickness
    height = stageV._2
    fill = color}
  }

  private def grid(axis: (Int,Color,Int) => Rectangle, center: Int, border: Int) = {
    var temp = center%50
    var graph = new Group()
    if(!(temp==center%100)) {
      graph.children.add(axis(temp, gridColor.opacity(0.2), gridThickness/2))
      temp += 50}
    while (temp <= border) {
      if (!(temp==center)) graph.children.add(axis(temp, gridColor.opacity(0.5), gridThickness/2))
      temp += 50
      graph.children.add(axis(temp, gridColor.opacity(0.2), gridThickness/2))
      temp += 50
    }
    graph
  }

  //templates for numbers on x and y Axis
  private def xAxisNum(xV: Int, num: Double) = {
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

  private def yAxisNum(yV: Int, num: Double) = {
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

  private def numbers(center: Int, border: Int, num: (Int,Double) => Text) = {
    var temp = center%100
    var graph = new Group()
    while (temp <= border) {
      if (!(temp==center)) graph.children.add(num((temp*(zoom/100)).toInt, (center-temp)/100))
      temp += 100
    }
    graph
  }