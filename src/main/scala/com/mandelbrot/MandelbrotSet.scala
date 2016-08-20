package com.mandelbrot

import java.awt.Color
import java.awt.event.{MouseEvent, MouseListener}
import java.awt.image.BufferedImage

//import net.jafama.FastMath._
//import java.lang.Math.{floor, log}
import org.apache.commons.math3.util.FastMath._

import javax.swing.{ImageIcon, JFrame, JLabel}


object MandelbrotSet {

  val width = 1920
  val height = 1024

  val black = 0
  val max_iterations = 1000
  val pallete = (0 to max_iterations).map(i => (i / 256f, 1, i / (i + 8f)))

  var zoom = 4.0
  var speed = 1.2
  var shiftX = 0.0
  var shiftY = 0.0

  def main(args: Array[String]): Unit = {

    val f = new JFrame()

    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    draw(image, zoom, 0.0, 0.0)

    f.getContentPane.add(new JLabel(new ImageIcon(image)))
    f.pack()
    f.repaint()
    f.setVisible(true)

    f.addMouseListener(new MouseListener {

      override def mouseExited(e: MouseEvent): Unit = {}

      override def mouseClicked(e: MouseEvent): Unit = {

        if (e.getButton == 1) {
          zoom = zoom / speed

          shiftX = shiftX + speed * zoom * 0.2 * (e.getX - width / 2) / width.toDouble
          shiftY = shiftY + speed * zoom * 0.2 * (e.getY - height / 2) / height.toDouble

        } else {
          zoom = zoom * 1.1
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

  private def mandel(col: Int, row: Int, zoom: Double, shiftX: Double, shiftY: Double): Double = {

    val c_re = (col - width / 2) * zoom / width + shiftX
    val c_im = (row - height / 2) * zoom / width + shiftY
    var x = 0.0
    var y = 0.0
    var iterations = 0.0
    while (x * x + y * y < 65536 && iterations < max_iterations) {
      val x_new = x * x - y * y + c_re
      y = 2 * x * y + c_im
      x = x_new
      iterations = iterations + 1
    }

    if (iterations < max_iterations) {
      val log_zn = log(x * x + y * y) / 2
      val nu = log(log_zn / log2) / log2
      iterations = (iterations + 1 - nu).toFloat
    }

    iterations
  }

  private def draw(image: BufferedImage, zoom: Double, shiftX: Double, shiftY: Double) {
    val s = System.currentTimeMillis()
    val g = image.createGraphics()
    g.setColor(Color.black)
    g.fillRect(0, 0, width, height)

    //val histogram = Array.ofDim[Int](max_iterations + 1)

      /*val m = (0 until height).par.map(row => {
      (0 until width).map(col => {

        val iterations = mandel(col, row, zoom, shiftX, shiftY)
      //  histogram(iterations.toInt) = histogram(iterations.toInt) + 1
        iterations
      })
    })*/

    //val total: Double = (1 until max_iterations).foldLeft(0.0d)((a, i) => a + histogram(i))

    //println(s"Total is $total")

    /*val hues = (1 until max_iterations).map(iterations => {

      //var hue = 0.0
      //(0 to iterations).foreach(i => hue += histogram(i) / total) // Must be floating-point division.


      val hue = 102400* (0 until iterations).foldLeft(0.0d)( (a:Double,i:Int) => a + (histogram(i) / total)  )
      Color.HSBtoRGB(hue.toFloat, 1, iterations / (iterations + 8f))
    }) */

//    println(histogram.toList)

    (0 until height).par.foreach(row => {
      (0 until width).par.foreach(col => {

        val iterations = mandel(col, row, zoom, shiftX, shiftY)

        if (iterations < max_iterations) {

          val color1 = pallete(floor(iterations).toInt)
          val color2 = pallete(floor(iterations + 1).toInt )
          val h = color1._1 //linearInterpolate(color1._1, color2._1, iterations % 1)
          val b = linearInterpolate(color1._3, color2._3, iterations % 1)

          image.setRGB(col, row, Color.HSBtoRGB(h.toFloat, 1, b.toFloat))
          //image.setRGB(col, row, colors(iterations.toInt))
        }
      })
    })
    val e = System.currentTimeMillis()

    println(s"zoom:$zoom shiftX:$shiftX shiftY$shiftY time:${e - s} ms")

  }

  private def timed(fname: String)(f: => Unit) {
    val s = System.nanoTime()

    val e = System.nanoTime()
    val t = e - s

    println(s"Time for $fname: $t")

  }

  private def linearInterpolate(v0: Double, v1: Double, t: Double) = (1 - t) * v0 + t * v1


}
