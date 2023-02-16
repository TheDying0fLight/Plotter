package plotter

import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.Group
import scalafx.geometry.VPos._
import scalafx.scene.text._
import scalafx.scene.paint.PaintIncludes.jfxColor2sfx

class Grid(var centerXY: (Int,Int), var stageSizeXY: (Int,Int), var zoom: (Double)):
  var gridColor = Black
  var gridThickness = 2

  def getCoordinateSystem =
    val xMainAxis = xAxisRec(centerXY._2, gridColor, gridThickness)
    val yMainAxis = yAxisRec(centerXY._1, gridColor, gridThickness)
    val xAxisNums = numbers(centerXY._1, stageSizeXY._1, xAxisNum)
    val yAxisNums = numbers(centerXY._2, stageSizeXY._2, yAxisNum)
    val xSubAxis = grid(xAxisRec, centerXY._2, stageSizeXY._2)
    val ySubAxis = grid(yAxisRec, centerXY._1, stageSizeXY._1)
    new Group(xSubAxis, ySubAxis, xMainAxis, yMainAxis, xAxisNums, yAxisNums)

  //templates for grid lines along the x and y Axis
  private def xAxisRec(yValue: Int, color: Color, thickness: Int) = {
    new Rectangle {
    x = 0
    y = yValue
    width = stageSizeXY._1
    height = thickness
    fill = color}
  }

  private def yAxisRec(xValue: Int, color: Color, thickness: Int) = {
    new Rectangle {
    x = xValue-thickness/2
    y = 0
    width = thickness
    height = stageSizeXY._2
    fill = color}
  }

  private def grid(axis: (Int,Color,Int) => Rectangle, centerXY: Int, border: Int) = {
    var temp = centerXY%50
    var thickness = gridThickness/2
    var graph = new Group()
    if(!(temp==centerXY%100)) {
      graph.children.add(axis(temp, gridColor.opacity(0.2), thickness))
      temp += 50}
    while (temp <= border) {
      if (!(temp==centerXY)) graph.children.add(axis(temp, gridColor.opacity(0.5), thickness))
      temp += 50
      graph.children.add(axis(temp, gridColor.opacity(0.2), thickness))
      temp += 50
    }
    graph
  }

  //templates for numbers on x and y Axis
  private def xAxisNum(xValue: Int, num: Double) = {
    new Text {
    x = xValue-50
    var distanceFromBorder = 8
    y = if(centerXY._2+5-distanceFromBorder+5<0) distanceFromBorder
      else if (centerXY._2+10>stageSizeXY._2) stageSizeXY._2-15
      else centerXY._2+10
    text = "%.2f".format(-num).toDouble.toString //String.format("%2.1e",num)
    textOrigin = Center
    wrappingWidth = 100
    textAlignment = TextAlignment.Center
    font = Font.font("Arial", FontWeight.Bold, FontPosture.Regular, 12)}
  }

  private def yAxisNum(yValue: Int, num: Double) = {
    new Text {
    x = if(centerXY._1-20<0) -80
      else if (centerXY._1>stageSizeXY._1) stageSizeXY._1-115
      else centerXY._1-105
    y = yValue
    text = "%.2f".format(num).toDouble.toString //String.format("%2.1e",num)
    textOrigin = Center
    wrappingWidth = 100
    textAlignment = TextAlignment.Right
    font = Font.font("Arial", FontWeight.Bold, FontPosture.Regular, 12)}
  }

  private def numbers(centerXY: Int, border: Int, num: (Int,Double) => Text) = {
    var temp = centerXY%100
    var graph = new Group()
    while (temp <= border) {
      if (!(temp==centerXY)) graph.children.add(num((temp*(zoom/100)).toInt, (centerXY-temp)/100))
      temp += 100
    }
    graph
  }