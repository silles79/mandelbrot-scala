package com.mandelbrot

import java.awt.Color
import java.awt.event.{MouseEvent, MouseListener}
import java.awt.image.BufferedImage
import javafx.scene.SceneAntialiasing

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.core.util.StatusPrinter
import org.slf4j.LoggerFactory
import javax.swing.{ImageIcon, JFrame, JLabel}

import org.apache.commons.math3.util.FastMath._

import scala.collection.parallel.immutable.ParSeq


object MandelbrotSet {

  val logger = LoggerFactory.getLogger(getClass.getName)

  val width = 1920
  val height = 1024

  val black = 0
  val max_iterations = 1000
  val palleta = (0 to max_iterations).map(i => (i / max_iterations.toFloat, 1, i / (i + 8f)))

  var zoom = 4.0
  var speed = 1.2
  var shiftX = 0.0
  var shiftY = 0.0
  var antialiasing = false

  //zoom = 0.06037975301988161
  //shiftX = -0.7517836387458078
  //shiftY = -0.08966622429551523

  //zoom:0.06037975301988161 shiftX:-0.7517836387458078 shiftY-0.08966622429551523 time:1671 m
 // zoom = 4.3953927646869003E-4
 // shiftX = -1.112683222518461
 // shiftY = -0.25177946746614227


  def main(args: Array[String]): Unit = {

    val f = new JFrame()

    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    draw(image, zoom, shiftX, shiftY, false)

    f.getContentPane.add(new JLabel(new ImageIcon(image)))
    f.pack()
    f.repaint()
    f.setVisible(true)

    val t = ( 1 to 10 ).map ( f=>
       timed("Benchmark") { draw(image, 4.0, 0.0, 0.0, false) }
      ).sum

    logger.info(s"${t / 10.0 / 1000/1000}")

    f.addMouseListener(new MouseListener {

      override def mouseExited(e: MouseEvent): Unit = {}

      override def mouseClicked(e: MouseEvent): Unit = {

        e.getButton match {
          case 1 =>
            zoom = zoom / speed
            shiftX = shiftX + speed * zoom * 0.2 * (e.getX - width / 2) / width.toDouble
            shiftY = shiftY + speed * zoom * 0.2 * (e.getY - height / 2) / height.toDouble

          case 2 =>
            val it = mandel( e.getX, e.getY, zoom, shiftX, shiftY )
            logger.info(s"Iterations at ${e.getX}:${e.getY} $it")
            antialiasing = !antialiasing
          case 3 => zoom = zoom * 1.1

        }

        draw(image, zoom, shiftX, shiftY, antialiasing)
        f.repaint()
      }

      override def mouseEntered(e: MouseEvent): Unit = {}

      override def mousePressed(e: MouseEvent): Unit = {}

      override def mouseReleased(e: MouseEvent): Unit = {}
    })

  }

  val log2: Double = log(2)
  val radius = 2 << 16

  private def mandel(col: Int, row: Int, zoom: Double, shiftX: Double, shiftY: Double): Double = {

    val c_re = (col - width / 2) * zoom / width + shiftX
    val c_im = (row - height / 2) * zoom / width + shiftY
    var x = 0.0
    var y = 0.0
    var iterations = 0.0
    var x2 = x*x
    var y2 = y*y
    var x_new = 0.0
    while (x2 + y2 < radius && iterations < max_iterations) {
      x_new = x2 - y2 + c_re
      y = 2 * x * y + c_im
      x = x_new
      x2 = x*x
      y2 = y*y
      iterations = iterations + 1
    }

    if (iterations < max_iterations) {
      val log_zn = log(x2 + y2) / 2
      val nu = log(log_zn / log2) / log2
      iterations = (iterations + 1 - nu).toFloat
    }

    iterations
  }

  private def draw(image: BufferedImage, zoom: Double, shiftX: Double, shiftY: Double, antialiasing: Boolean) {
    val s = System.currentTimeMillis()
    val g = image.createGraphics()
    g.setColor(Color.black)
    g.fillRect(0, 0, width, height)

    //val histogram = Array.ofDim[Int](max_iterations + 1)

      val m = (0 until height).par.foreach(row => {
      (0 until width).foreach(col => {

        val iterations = mandel(col, row, zoom, shiftX, shiftY)
      //  histogram(iterations.toInt) = histogram(iterations.toInt) + 1
        if (iterations < max_iterations) {

          val color1 = palleta(floor(iterations).toInt)
          // val color1 = iToC(iterations)
          val color2 = palleta(floor(iterations + 1).toInt)
          //val color2 = iToC(iterations + 1)
          val h = linearInterpolate(color1._1, color2._1, iterations % 1).toFloat
          val b = linearInterpolate(color1._3, color2._3, iterations % 1).toFloat
          image.setRGB(col, row, Color.HSBtoRGB(h, 1, b))
          //(h,b)
        } //else
          //(0.0f, 0.0f)


      })
    })

    //val total: Double = (1 until max_iterations).foldLeft(0.0d)((a, i) => a + histogram(i))

    //println(s"Total is $total")

    /*val hues = (1 until max_iterations).map(iterations => {

      //var hue = 0.0
      //(0 to iterations).foreach(i => hue += histogram(i) / total) // Must be floating-point division.


      val hue = 102400* (0 until iterations).foldLeft(0.0d)( (a:Double,i:Int) => a + (histogram(i) / total)  )
      Color.HSBtoRGB(hue.toFloat, 1, iterations / (iterations + 8f))
    }) */

//    println(histogram.toList)

   /* (0 until height).foreach(row => {
      (0 until width).foreach(col => {


          if ( row > 0 && row < height-1 && col >0 && col < width-1 && antialiasing ) {


            import Helpers._
            val hb = (
              m(row)(col) +
              m(row - 1)(col) +
              m(row + 1)(col) +
              m(row)(col-1) +
              m(row)(col+1)
              ) / (5.0f, 5.0f)

            image.setRGB(col, row, Color.HSBtoRGB(hb._1, 1, hb._2))
          }
          else {

            val hb = m(row)(col)
            image.setRGB(col, row, Color.HSBtoRGB(hb._1, 1, hb._2))
          }

      })
    })
*/

    palleta.zipWithIndex.foreach( color => {
      g.setColor(new Color(Color.HSBtoRGB(color._1._1, color._1._2.toFloat, color._1._3)))
      g.fillRect(color._2*1, 0, 1, 10)
    }
    )

    val e = System.currentTimeMillis()

    logger.info(s"zoom=$zoom shiftX=$shiftX shiftY=$shiftY time:${e - s} ms")

  }



  private def timed(fname: String)(f: => Unit) = {
    val s = System.nanoTime()
    f
    val e = System.nanoTime()
    val t = e - s

    logger.debug(s"Time for $fname: $t")
    t

  }

  private def iToC(i:Double) = (i / 256f, 1.0f, i / (i + 8f))

  private def linearInterpolate(v0: Double, v1: Double, t: Double) = (1 - t) * v0 + t * v1


}

object Helpers {
  implicit class Tupple2Math(x:(Float, Float)) {
    def +( y: (Float, Float) ) = (x._1 + y._1, x._2 + y._2)
    def /( y: (Float, Float) ) = (x._1 / y._1, x._2 / y._2)
    def *( y: (Float, Float) ) = (x._1 * y._1, x._2 * y._2)
  }
}
