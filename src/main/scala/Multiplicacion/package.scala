import scala.annotation.tailrec

package object Multiplicacion {


  //Split Algorithm with lineal recursion
  def splitMultiply(a: Int, b: Int): Int = {

    def digitosDecimales(a: Int, c: Int): Int = {
      if ((a / 10) >= 1) digitosDecimales((a / 10), c + 1) else c
    }

    def separacionIzquierda(a: Int, m: Int): Int = a / math.pow(10, m).toInt

    def separacionDerecha(a: Int, m: Int): Int = a % math.pow(10, m).toInt

    def multiplicarNumero(a: Int, b: Int): Int = {
      val n = digitosDecimales(a, 1)
      val m = n / 2
      if (n == 1) a * b
      else (math.pow(10, 2 * m).toInt) * multiplicarNumero(separacionIzquierda(a, m), separacionIzquierda(b, m)) + (math.pow(10, m).toInt * (multiplicarNumero(separacionDerecha(a, m), separacionIzquierda(b, m)) + multiplicarNumero(separacionIzquierda(a, m), separacionDerecha(b, m)))) + multiplicarNumero(separacionDerecha(a, m), separacionDerecha(b, m))
    }

    multiplicarNumero(a, b)
  }
}