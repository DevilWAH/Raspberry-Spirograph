package proto

case class Point(x: Double, y: Double) {
  def +(that: Point): Point = Point(x + that.x, y + that.y)
  def *(m: Double) = Point(x * m, y * m)
}
object Point{
  val origin = Point(0,0)
}

sealed trait Node {
  def render(in: Point, time: Double): Point
}
case class Mirror(radius: Double, speed: Double, child: Node) extends Node{
  def render(in: Point, time: Double): Point = {
    child.render(
      in + Point(
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
  }
}



object Spira {
  val mirrors = Mirror(2,3,Mirror(4,2,Mirror(3,5, Node.id)))

//  val points = {
//
//
//    (1 to 1000).map{t =>
//      mirrors.render(Point.origin,  t/50.0)
//    }
//  }
//
//  points.foreach(println)
}


import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.raw.Window
import org.scalajs.dom.{ImageData, html}

import scala.collection.mutable

@JSExport
object Main {
  @JSExport
  def main(canvas: html.Canvas): Unit = {
    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    case class Timed[T](value: T, time: Double)
    val queue = new mutable.Queue[Timed[Point]]()
    val length = 100

    def clear() = {
      ctx.fillStyle = "black"
      ctx.fillRect(0, 0, 300, 300)
    }

    clear()

    val w = scala.scalajs.js.Dynamic.global.window.asInstanceOf[Window]

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

    def update(): Function1[Double, Unit] = (d: Double) => {
      render()
      w.requestAnimationFrame(update())
    }
    update()(0)

    def render(): Unit ={
      def plotPt(pt: Point, erase: Boolean): Unit ={
        val scaled = pt * 10 + Point(100, 100)
        if(erase) ctx.fillStyle = ctx.putImageData(blackPixel, scaled.x, scaled.y)
        else ctx.fillStyle = ctx.putImageData(greenPixel, scaled.x, scaled.y)
      }

      if(queue.size == 0) {
        val newPt = Spira.mirrors.render(Point.origin, 0)
        queue.enqueue(Timed(newPt, 0))
      }
      else if(queue.size > length){
        val removed = queue.dequeue()
        plotPt(removed.value, true)
      }else{
        val time = queue.last.time + 1
        val newPt = Spira.mirrors.render(Point.origin, time / 30)
        queue.enqueue(Timed(newPt, time))
        plotPt(newPt, false)
      }
    }
  }
}
