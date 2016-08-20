package com.mandelbrot

import java.awt.Color
import java.awt.event.{MouseEvent, MouseListener}
import java.awt.image.BufferedImage
import java.lang.Math.{floor, log}
import java.util.concurrent.atomic.AtomicInteger
import javax.swing.{ImageIcon, JFrame, JLabel}

import scala.collection.immutable.IndexedSeq

object MandelbrotSet {

  val width = 1920
  val height = 1024

  val black = 0
  val max_iterations = 1000
  val colors: IndexedSeq[Int] = (0 to max_iterations).map(i => Color.HSBtoRGB(i / 256f, 1, i / (i + 8f)))
  val hues2 = (0 to max_iterations).map(i => (i / 256f, 1, i / (i + 8f)))

  var zoom = 4.0
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
          zoom = zoom / 1.1

          shiftX = shiftX + zoom * 0.2 * (e.getX - width / 2) / width.toDouble
          shiftY = shiftY + zoom * 0.2 * (e.getY - height / 2) / height.toDouble

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

  private def mandel(col: Int, row: Int, zoom: Double, shiftX: Double, shiftY: Double): Double = {

    val c_re = (col - width / 2) * zoom / width + shiftX
    val c_im = (row - height / 2) * zoom / width + shiftY
    var x = 0.0
    var y = 0.0
    var iterations = 0.0
    while (x * x + y * y < 16 && iterations < max_iterations) {
      val x_new = x * x - y * y + c_re
      y = 2 * x * y + c_im
      x = x_new
      iterations = iterations + 1
    }

    if (iterations < max_iterations) {
      // sqrt of inner term removed using log simplification rules.
      val log_zn = log(x * x + y * y) / 2
      val nu = log(log_zn / log(2)) / log(2)
      // Rearranging the potential function.
      // Dividing log_zn by log(2) instead of log(N = 1<<8)
      // because we want the entire palette to range from the
      // center to radius 2, NOT our bailout radius.
      iterations = iterations + 1 - nu
    }

    iterations

  }

  private def draw(image: BufferedImage, zoom: Double, shiftX: Double, shiftY: Double) {
    val s = System.currentTimeMillis()
    val g = image.createGraphics()
    g.setColor(Color.black)
    g.fillRect(0, 0, width, height)

    val histogram = Array.ofDim[Int](max_iterations + 1)

    val m = (0 until height).par.map(row => {
      (0 until width).map(col => {

        val iterations = mandel(col, row, zoom, shiftX, shiftY)



        histogram(iterations.toInt) = histogram(iterations.toInt) + 1
        iterations
      })
    })

    println(histogram.toList)

    //var total = 0.0
    //(0 to  max_iterations).foreach( i => total = total + histogram(i) )

    val total: Double = (1 until max_iterations).foldLeft(0.0d)((a, i) => a + histogram(i))

    println(s"Total is $total")

    val hues = (1 until max_iterations).map(iterations => {

      //var hue = 0.0
      //(0 to iterations).foreach(i => hue += histogram(i) / total) // Must be floating-point division.


      val hue = 102400* (0 until iterations).foldLeft(0.0d)( (a:Double,i:Int) => a + (histogram(i) / total)  )
      //println(hue)
      Color.HSBtoRGB(hue.toFloat, 1, iterations / (iterations + 8f))
      //hue
    })

    println(histogram.toList)

    (0 until height).foreach(row => {
      (0 until width).foreach(col => {

        val iterations = m(row)(col)

        if (iterations < max_iterations) {
          //  image.setRGB(col, row, colors((max_iterations*hue).toInt))
          //image.setRGB(col, row, hues(iterations.toInt-1))

          //image.setRGB(col, row, new Color(max_iterations-iterations, max_iterations-iterations, max_iterations-iterations).getRGB)

          val l =  max_iterations - iterations
          //image.setRGB(col, row, l | (l << 8))

          //println(iteration)

          val color1 = hues2(floor(iterations).toInt)
          val color2 = hues2(floor(iterations + 1).toInt )
          // iteration % 1 = fractional part of iteration.
          //println(s"$iteration ${iteration % 1}")
          //println(iterations % 1)
          val h = linearInterpolate(color1._1, color2._1, iterations % 1)
          val b = linearInterpolate(color1._3, color2._3, iterations % 1)
       //   println(s"$color1 $color2 $color")
          //else image.setRGB(col, row, black)
          //image.setRGB(col, row, Color.HSBtoRGB(color.toFloat, 1, (iterations / (iterations + 8f)).toFloat ))
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
