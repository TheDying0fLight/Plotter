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

class Grid(var centerDraggedXY: (Double, Double), var stageSizeXY: (Int, Int), var zoom: Double):
  var gridColor = Black
  var gridThickness = 2
  var textDistanceFromBorder = 8

  def getCoordinateSystem =
    val xMainAxis = xAxisRec(centerDraggedXY(1), gridThickness, gridColor)
    val yMainAxis = yAxisRec(centerDraggedXY(0), gridThickness, gridColor)
    val xAxisNums = graphing(xAxisNum)(centerDraggedXY(0), stageSizeXY(0))
    val yAxisNums = graphing(yAxisNum)(centerDraggedXY(1), stageSizeXY(1))
    val xSubAxis = grid(xAxisRec)(centerDraggedXY(1), stageSizeXY(1))
    val ySubAxis = grid(yAxisRec)(centerDraggedXY(0), stageSizeXY(0))
    new Group(xSubAxis, ySubAxis, xMainAxis, yMainAxis, xAxisNums, yAxisNums)

  // templates for grid lines along the x and y Axis
  private def xAxisRec(yValue: Double, thickness: Double, color: Color) = {
    new Rectangle {
      x = 0
      y = yValue
      width = stageSizeXY(0)
      height = thickness
      fill = color
    }
  }

  private def yAxisRec(xValue: Double, thickness: Double, color: Color) = {
    new Rectangle {
      x = xValue - thickness / 2
      y = 0
      width = thickness
      height = stageSizeXY(1)
      fill = color
    }
  }

  private def grid(shaper: (Double, Double,Color) => Node)(center: Double, border: Double) = {
    var thickness = gridThickness / 2
    var step = stepFinder / 2
    var graph = new Group()
    def addShapes(addSub: (Double, Double) => Double) =
      var point: Double = addSub(center, step) // if (center - step < border * 2) addSub(center, step) else center % border + border
      var value = stepEval(point, center)
      var opacity = 0.2
      def updateOpacity = opacity = if(opacity == 0.2) 0.5 else  0.2
      while (value > border)
        point -= step
        value = stepEval(point, center)
        updateOpacity
      while (value < 0)
        point += step
        value = stepEval(point, center)
        updateOpacity
      while (value <= border && value >= 0) {
        graph.children.add(shaper(stepEval(point, center), thickness, gridColor.opacity(opacity)))
        updateOpacity
        point = addSub(point, step)
        value = stepEval(point, center)
      }
    addShapes((a, b) => a + b)
    addShapes((a, b) => a - b)
    graph
  }

  // templates for numbers on x and y Axis
  private def xAxisNum(xValue: Double, num: Double) = {
    new Text {
      x = xValue - 50
      y =
        if (centerDraggedXY(1) < textDistanceFromBorder - 10) textDistanceFromBorder
        else if (centerDraggedXY(1) > stageSizeXY(1) - textDistanceFromBorder - 45)
          stageSizeXY(1) - 36 - textDistanceFromBorder
        else centerDraggedXY(1) + 10
      text = "%.2f".format(-num).toDouble.toString // String.format("%2.1e",num)
      textOrigin = Center
      wrappingWidth = 100
      textAlignment = TextAlignment.Center
      font = Font.font("Arial", FontWeight.Bold, FontPosture.Regular, 12)
    }
  }

  private def yAxisNum(yValue: Double, num: Double) = {
    new Text {
      x =
        if (centerDraggedXY(0) - 20 < 0) -80
        else if (centerDraggedXY(0) > stageSizeXY(0)) stageSizeXY(0) - 115
        else centerDraggedXY(0) - 105
      y = yValue
      text = "%.2f".format(num).toDouble.toString // String.format("%2.1e",num)
      textOrigin = Center
      wrappingWidth = 100
      textAlignment = TextAlignment.Right
      font = Font.font("Arial", FontWeight.Bold, FontPosture.Regular, 12)
    }
  }

  private def graphing(shaper: (Double, Double) => Node)(center: Double, border: Double) = {
    var step = stepFinder
    var graph = new Group()
    def addShapes(addSub: (Double, Double) => Double) =
      var point: Double = addSub(center, step)
      var value = stepEval(point, center)
      while (value > border)
        point -= step
        value = stepEval(point, center)
      while (value < 0)
        point += step
        value = stepEval(point, center)
      while (value <= border && value >= 0) {
        graph.children.add(shaper(value, (center - point) / 100))
        point = addSub(point, step)
        value = stepEval(point, center)
      }
    addShapes((a, b) => a + b)
    addShapes((a, b) => a - b)
    graph
  }

  private def stepEval(point: Double, center: Double) = ((point - center) * (zoom / 100) + center).toInt

  private def stepFinder = {
    var step = 100.toDouble
    var tempZoom = zoom
    val low = 75
    val high = 150
    def multDivStepZoom(multDiv: (Double, Double) => Double, fac: Double) = {tempZoom = multDiv(tempZoom, fac); step = multDiv(step, fac)}
    def zoomInOut(greaterLess: (Double,Double) => Boolean, lowHigh: Double, multDiv: (Double, Double) => Double) =
      while (greaterLess(tempZoom, lowHigh))
        multDivStepZoom(multDiv, 2)
        if (greaterLess(tempZoom, lowHigh)) {multDivStepZoom(multDiv, 2.5)}
        if (greaterLess(tempZoom, lowHigh)) {multDivStepZoom(multDiv, 2)}
    if (tempZoom < low)
      zoomInOut((a,b) => a < b, low, (a,b) => a * b)
    else if (tempZoom > high)
      zoomInOut((a,b) => a > b, high, (a,b) => a / b)
    step
  }