/**
 * @author Chris Campo
 */
object Brotwurst extends App {

  def mandelbrot(nx: Int, ny: Int, xMin: Double, xMax: Double,
                 yMin: Double, yMax: Double, max_iterations: Int): Array[Array[Int]] = {
    val grid = Array.ofDim[Int](ny, nx)
    val dx = (xMax - xMin) / (nx - 1)
    val dy = (yMax - yMin) / (ny - 1)
    for (i <- 0 until nx; j <- 0 until ny)
      grid(j)(i) = iterate(0, xMin + dx * i, yMin + dy * j, 0d, 0d)

    @annotation.tailrec
    def iterate(step: Int, x0: Double, y0: Double, x: Double, y: Double): Int = {
      if (math.pow(x, 2) + math.pow(y, 2) >= 4 || step == max_iterations) return step
      iterate(step + 1, x0, y0, math.pow(x, 2) - math.pow(y, 2) + x0, 2 * x * y + y0)
    }
    grid
  }

  def printGrid(grid: Array[Array[Int]], iterations: Int): Unit = {
    grid.map(r => r.map(c => if (c == iterations) "*" else " ").mkString) foreach println
  }

  printGrid(mandelbrot(1000, 400, -2.5, 1, -1, 1, 100), 100)
}
