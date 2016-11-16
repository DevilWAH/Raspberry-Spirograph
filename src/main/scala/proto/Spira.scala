package proto

case class Point(x: Double, y: Double) {
  def +(that: Point): Point = Point(x + that.x, y + that.y)
  def *(m: Double) = Point(x * m, y * m)
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
  val points = {
    val mirrors = Mirror(2,3,Mirror(4,2,Mirror(3,5, Node.id)))

    (1 to 1000).map{t =>
      mirrors.render(Point(0,0),  t/50.0)
    }
  }

  points.foreach(println)
}


import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html

@JSExport
object Main {
  @JSExport
  def main(canvas: html.Canvas): Unit = {
    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    var count = 0
    var p = Point(0, 0)
    val corners = Seq(Point(800, 800), Point(0, 800), Point(400, 0))

    def clear() = {
      ctx.fillStyle = "black"
      ctx.fillRect(0, 0, 800, 800)
    }

    def run() = {

      if (count % 3000 == 0) clear()
      count += 1

      Spira.points.foreach { pt =>
        println(pt)
        val p = pt * 10+ Point(100,100)
        ctx.fillStyle = s"rgb(255, 10, 10)"
        ctx.fillRect(p.x, p.y, 1, 1)
      }

    }

    println("RUN")
    run()
  }
}
