package com.mandelbrot

import java.awt.Color
import java.awt.event.{MouseEvent, MouseListener}
import java.awt.image.BufferedImage
import javax.swing.{ImageIcon, JFrame, JLabel}

import scala.collection.immutable.IndexedSeq

object MandelbrotSet {

  val width = 1920 / 2
  val height = 1024 / 2

  val black = 0
  val max_iterations = 1000
  val colors: IndexedSeq[Int] = (1 to max_iterations).map(i => Color.HSBtoRGB(i / 256f, 1, i / (i + 8f)))
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

  private def draw(image: BufferedImage, zoom: Double, shiftX: Double, shiftY: Double) {
    val s = System.currentTimeMillis()

    val histogram = Array.ofDim[Int](max_iterations)

    (0 until height).par.foreach(row => {
      (0 until width).foreach(col => {

        val c_re = (col - width / 2) * zoom / width + shiftX
        val c_im = (row - height / 2) * zoom / width + shiftY
        var x = 0.0
        var y = 0.0
        var iteration = 0.0
        while (x * x + y * y < 4 && iteration < max_iterations) {
          val x_new = x * x - y * y + c_re
          y = 2 * x * y + c_im
          x = x_new
          iteration = iteration + 1
        }

        if (iteration < max_iterations) {
          image.setRGB(col, row, colors(iteration.toInt))
        } else image.setRGB(0,0,0)

      })
    })
    val e = System.currentTimeMillis()

    println(s"zoom:$zoom shiftX:$shiftX shiftY$shiftY time:${e - s} ms")

  }

  private def linearInterpolate(v0: Double, v1: Double, t: Double) = (1 - t) * v0 + t * v1


}
