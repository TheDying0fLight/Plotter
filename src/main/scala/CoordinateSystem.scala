package plotter

import scalafx.scene.shape.Rectangle
import scalafx.scene.Node
import scalafx.scene.text.Text
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.Group
import scalafx.geometry.VPos._
import scalafx.scene.text._
import scalafx.scene.paint.PaintIncludes.jfxColor2sfx


class Grid(var centerXY: (Double,Double), var stageSizeXY: (Int,Int), var zoom: Double):
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
  private def xAxisRec(yValue: Double, color: Color, thickness: Int) = {
    new Rectangle {
    x = 0
    y = yValue
    width = stageSizeXY._1
    height = thickness
    fill = color}
  }

  private def yAxisRec(xValue: Double, color: Color, thickness: Int) = {
    new Rectangle {
    x = xValue-thickness/2
    y = 0
    width = thickness
    height = stageSizeXY._2
    fill = color}
  }

  private def grid(axis: (Int,Color,Int) => Rectangle, center: Double, border: Int) = {
    var thickness = gridThickness/2
    var step = stepFinder/2
    var point = center+step
    var graph = new Group()
    while (stepEval(point, center) <= border) {
      graph.children.add(axis(stepEval(point, center), gridColor.opacity(0.2), thickness))
      point += step
      graph.children.add(axis(stepEval(point, center), gridColor.opacity(0.5), thickness))
      point += step
    }
    point = center-step
    while (stepEval(point, center) >= 0) {
      graph.children.add(axis(stepEval(point, center), gridColor.opacity(0.2), thickness))
      point -= step
      graph.children.add(axis(stepEval(point, center), gridColor.opacity(0.5), thickness))
      point -= step
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

  private def numbers(center: Double, border: Int, num: (Int,Double) => Text) = {
    var step = stepFinder
    var point = center+step
    var graph = new Group()
    while (stepEval(point, center) <= border) {
      graph.children.add(num(stepEval(point, center), (center-point)/100))
      point += step
    }
    point = center-step
    while (stepEval(point, center) >= 0) {
      graph.children.add(num(stepEval(point, center), (center-point)/100))
      point -= step
    }
    graph
  }

  // private def graphing[A](center: Int, border: Int, shape: A => Node) = {
  //   var step = stepFinder
  //   var point = center+step
  //   var graph = new Group()
  //   while (stepEval(point, center) <= border) {
  //     graph.children.add(shape())
  //     point += step
  //   }
  //   point = center-step
  //   while (stepEval(point, center) >= 0) {
  //     graph.children.add(shape())
  //     point -= step
  //   }
  //   graph
  // }

  private def stepEval(point: Double, center: Double) = ((point-center)*(zoom/100)+center).toInt

  private def stepFinder = {
    var step = 100.toDouble
    var tempZoom = zoom
    if (tempZoom < 75) 
      while (tempZoom < 75)
        tempZoom *= 2; step *= 2
        if (tempZoom < 75) {tempZoom *= 2.5; step *= 2.5}

    else if (tempZoom > 150)
      while (tempZoom > 150)
        tempZoom /= 2; step /= 2
        if (tempZoom > 150) {tempZoom /= 2.5; step /= 2.5}

    step
  }