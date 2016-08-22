package com.mandelbrot

import java.awt.Color
import java.awt.event.{MouseEvent, MouseListener}
import java.awt.image.BufferedImage
import javax.swing.{ImageIcon, JFrame, JLabel}

import org.apache.commons.math3.util.FastMath._
import org.slf4j.LoggerFactory


object MandelbrotSet {

  val logger = LoggerFactory.getLogger(getClass.getName)

  val width = 1920
  val height = 1024

  val black = 0
  val max_iterations = 1000
  val palleta = (0 to max_iterations).map(i => ((i) / 256f, 1, i / (i + 8f)))

  var zoom = 4.0
  var speed = 1.2
  var shiftX = 0.0
  var shiftY = 0.0

  //zoom = 0.06037975301988161
  //shiftX = -0.7517836387458078
  //shiftY = -0.08966622429551523

  //zoom:0.06037975301988161 shiftX:-0.7517836387458078 shiftY-0.08966622429551523 time:1671 m
  // zoom = 4.3953927646869003E-4
  // shiftX = -1.112683222518461
  // shiftY = -0.25177946746614227

  //zoom=0.020221057118819667
  //shiftX = -0.7734143194603526
  //shiftY = -0.10801967083280396

  def main(args: Array[String]): Unit = {

    val f = new JFrame()

    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    draw(image, zoom, shiftX, shiftY)

    f.getContentPane.add(new JLabel(new ImageIcon(image)))
    f.pack()
    f.repaint()
    f.setVisible(true)

    val t = (1 to 10).map(f =>
      timed("Benchmark") {
        draw(image, 4.0, 0.0, 0)
      }
    ).sum

    logger.info(s"${t / 10.0 / 1000 / 1000}")

    f.addMouseListener(new MouseListener {

      override def mouseExited(e: MouseEvent): Unit = {}

      override def mouseClicked(e: MouseEvent): Unit = {

        e.getButton match {
          case 1 =>
            zoom = zoom / speed
            shiftX = shiftX + speed * zoom * 0.2 * (e.getX - width / 2) / width.toDouble
            shiftY = shiftY + speed * zoom * 0.2 * (e.getY - height / 2) / height.toDouble

          case 2 =>
            val it = mandel(e.getX, e.getY, zoom, shiftX, shiftY)
            logger.info(s"Iterations at ${e.getX}:${e.getY} $it")
          case 3 => zoom = zoom * 1.1

        }

        draw(image, zoom, shiftX, shiftY)
        f.repaint()
      }

      override def mouseEntered(e: MouseEvent): Unit = {}

      override def mousePressed(e: MouseEvent): Unit = {}

      override def mouseReleased(e: MouseEvent): Unit = {}
    })

  }

  val log2: Double = log(2)
  val radius = 2 << 16
  val ZERO = 1e-17

  private def mandel(col: Int, row: Int, zoom: Double, shiftX: Double, shiftY: Double): Double = {

    // todo implement periodic checking https://en.wikipedia.org/wiki/User:Simpsons_contributor/periodicity_checking
    val c_re = (col - width / 2) * zoom / width + shiftX
    val c_im = (row - height / 2) * zoom / width + shiftY
    var x = 0.0
    var y = 0.0
    var iterations = 0.0
    var x2 = x * x
    var y2 = y * y
    var x_new = 0.0
    var y_new = 0.0
    // https://en.wikibooks.org/wiki/Fractals/Iterations_in_the_complex_plane/Mandelbrot_set
    if ((c_re + 1) * (c_re + 1) + c_im * c_im >= 1 / 16.0) {

      val q = (c_re - 1 / 4.0) * (c_re - 1 / 4.0) + c_im * c_im
      if (q * (q + c_re - 1 / 4.0) >= c_im * c_im / 4.0) {
        while (x2 + y2 < radius && iterations < max_iterations) {
          x_new = x2 - y2 + c_re // zx
          y_new = 2 * x * y + c_im

          // this doesn't seem to work
          /*if (abs(x_new - x) < ZERO && abs(y_new - y) < ZERO) {
              iterations = max_iterations
              logger.info("ZERO")
            }
          */
          x = x_new
          y = y_new
          x2 = x * x
          y2 = y * y
          iterations = iterations + 1
        }
        if (iterations < max_iterations) {
          val log_zn = log(x2 + y2) / 2
          val nu = log(log_zn / log2) / log2
          iterations = (iterations + 1 - nu).toFloat
        }

        iterations
      } else max_iterations
    } else max_iterations
  }

  private def draw(image: BufferedImage, zoom: Double, shiftX: Double, shiftY: Double) {
    val s = System.currentTimeMillis()
    val g = image.createGraphics()
    g.setColor(Color.black)
    g.fillRect(0, 0, width, height)

    (0 until height).par.foreach(row => {
      (0 until width).foreach(col => {

        val iterations = mandel(col, row, zoom, shiftX, shiftY)
        if (iterations < max_iterations) {

          val color1 = palleta(floor(iterations).toInt)
          val color2 = palleta(floor(iterations + 1).toInt)
          val h = linearInterpolate(color1._1, color2._1, iterations % 1).toFloat
          val b = linearInterpolate(color1._3, color2._3, iterations % 1).toFloat
          image.getRaster.setPixel(col, row, intToRGB(Color.HSBtoRGB(h, 1, b)))
        }
      })
    })

    def intToRGB(rgb: Int) = Array((rgb >> 16) & 0xFF, (rgb >> 8) & 0xFF, (rgb >> 0) & 0xFF)

   /* palleta.zipWithIndex.foreach(color => {
      g.setColor(new Color(Color.HSBtoRGB(color._1._1, color._1._2.toFloat, color._1._3)))
      g.fillRect(color._2 * 1, 0, 1, 10)
    }) */

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

  private def iToC(i: Double) = (i / 256f, 1.0f, i / (i + 8f))

  private def linearInterpolate(v0: Double, v1: Double, t: Double) = (1 - t) * v0 + t * v1


}

object Helpers {

  implicit class Tupple2Math(x: (Float, Float)) {
    def +(y: (Float, Float)) = (x._1 + y._1, x._2 + y._2)

    def /(y: (Float, Float)) = (x._1 / y._1, x._2 / y._2)

    def *(y: (Float, Float)) = (x._1 * y._1, x._2 * y._2)
  }

}
