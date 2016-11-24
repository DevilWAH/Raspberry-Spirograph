package proto

case class Point(x: Double, y: Double) {
  def +(that: Point): Point = Point(x + that.x, y + that.y)
  def -(that: Point): Point = Point(x -that.x, y - that.y)
  def *(m: Double) = Point(x * m, y * m)
  def distToSq(that: Point) = {
    math.pow(that.x - this.x, 2.0) + math.pow(that.y - this.y, 2.0)
  }
}
object Point{
  val origin = Point(0,0)
}

sealed trait Node {
  def render(in: Point, time: Double): Point
  def boundingRadius(innerBounds: Double): Double
}
case class Mirror(radius: Double, speed: Double, child: Node) extends Node{
  def boundingRadius(innerBounds: Double): Double = {
    val myBounds =  innerBounds+ radius
    child.boundingRadius(myBounds)
  }

  def render(input: Point, time: Double): Point = {
    child.render(
      input + Point(
        radius * math.cos(speed * time),
        radius * math.sin(speed * time)
      ),
      time
    )
  }
}
object Node {
  val id = new Node {
    def render(in: Point, time: Double): Point = in
    def boundingRadius(innerBounds: Double) = innerBounds
  }
}



object Spira {
  val mirrors = Mirror(5,1,Mirror(2,9.6, Node.id))

  //println(mirrors.boundingBoxFrom(BoundingBox.origin))
}

case class TimePoint(time: Double, point: Point)


import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html

import scala.annotation.tailrec
import scala.collection.mutable

@JSExport
object Main {
  @JSExport
  def main(canvas: html.Canvas): Unit = {
    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    case class Timed[T](value: T, time: Double)
    val queue = new mutable.Queue[TimePoint]()
    val scaleFactor = {
      val boundingDiameter= 2 * Spira.mirrors.boundingRadius(0)
      println(boundingDiameter)
      300.0 / boundingDiameter
    }
    val offset = Point(300 / 2.0, 300 / 2.0)


    def getSpiraTimePoint(time: Double): TimePoint = {
      def screenScale(raw: Point) = raw * scaleFactor + offset
      TimePoint(time, screenScale(Spira.mirrors.render(Point.origin, time / 70)))
    }
    val initPoints = IndexedSeq(
      getSpiraTimePoint(0),
      getSpiraTimePoint(1000)
    )

    val pixelTolSquared= math.pow(5,2)
    @tailrec
    def recursiveBisection(tps: IndexedSeq[TimePoint], position: Int = 0): IndexedSeq[TimePoint] = {
      if(position == tps.size - 1)
        tps
      else {
        val tp1 = tps(position)
        val tp2 = tps(position + 1)
        val distSq = tp1.point.distToSq(tp2.point)

        if(distSq < pixelTolSquared) {
          recursiveBisection(tps, position + 1)
        }
        else {
          val midTime = (tps(position + 1).time + tps(position).time) / 2
          val updatedTPs = {
            val (front, back) = tps.splitAt(position + 1)
            ( front :+getSpiraTimePoint(midTime)) ++: back
          }
          recursiveBisection(updatedTPs, position)
        }
      }
    }

    val finalPoints = recursiveBisection(initPoints)

    def clear() = {
      ctx.fillStyle = "black"
      ctx.fillRect(0, 0, 300, 300)
    }

    clear()

    val greenPixel = {
      val id = ctx.createImageData(1,1)
      var d = id.data
      d(0) = 0
      d(1) = 255
      d(2) = 0
      d(3) = 200
      id
    }

    val blackPixel = {
      val id = ctx.createImageData(1,1)
      var d = id.data
      d(0) = 0
      d(1) = 0
      d(2) = 0
      d(3) = 255
      id
    }

    def render(): Unit ={
      def plotPt(pt: Point): Unit ={
        ctx.putImageData(greenPixel, pt.x.toInt, pt.y.toInt)
      }

      finalPoints.foreach(tp => plotPt(tp.point))
    }

    render()
  }
}
