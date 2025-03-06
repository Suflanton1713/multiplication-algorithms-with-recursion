import scala.annotation.tailrec

package object Multiplicacion {

  
  def splitMultiply(a: Int, b: Int): Int = {
    
    def digitosDecimales(a: Int, c: Int): Int = {
      if ((a / 10) >= 1) digitosDecimales((a / 10), c + 1) else c
    }
    
    def separacionIzquierda(a: Int, m: Int): Int = a / math.pow(10, m).toInt
    
    def separacionDerecha(a: Int, m: Int): Int = a % math.pow(10, m).toInt

    def decisionMultiplicar(a: Int, b:Int):Int = if (a>b) multiplicarNumero(a,b) else multiplicarNumero(b,a)
    
    def multiplicarNumero(a: Int, b: Int): Int = {
      val n = digitosDecimales(a, 1)
      val n_dos = digitosDecimales(b, 1)
      val m = n / 2
      if ((n == 1) && (n_dos == 1)){
        a * b
      }
      else{
          (math.pow(10, 2 * m).toInt) * splitMultiply(separacionIzquierda(a, m), separacionIzquierda(b, m)) +
          (math.pow(10, m).toInt * (splitMultiply(separacionDerecha(a, m), separacionIzquierda(b, m)) + splitMultiply(separacionIzquierda(a, m), separacionDerecha(b, m)))) +
          splitMultiply(separacionDerecha(a, m), separacionDerecha(b, m))
   

      }
    }

    decisionMultiplicar(a, b)
  }


  def fastMultiply(a: Int, b: Int): Int = {

    def digitosDecimales(a: Int, c: Int): Int = {
      if ((a / 10) >= 1) digitosDecimales((a / 10), c + 1) else c
    }
    def separacionIzquierda(a: Int, m: Int): Int = a / math.pow(10, m).toInt

    def separacionDerecha(a: Int, m: Int): Int = a % math.pow(10, m).toInt

    def decisionMultiplicar(a: Int, b: Int): Int = if (a > b) multiplicarNumero(a, b) else multiplicarNumero(b, a)

    def multiplicarNumero(a: Int, b: Int): Int = {
   
      val n = digitosDecimales(a, 1)
      val n_dos = digitosDecimales(b, 1)
      val m = n / 2
  
      if ((n == 1) && (n_dos == 1)) {
        a * b
      }
      else {

        val mult_1= fastMultiply(separacionIzquierda(a, m), separacionIzquierda(b, m))
        val mult_2 = fastMultiply(separacionDerecha(a, m), separacionDerecha(b, m))
        
        math.pow(10, 2 * m).toInt * mult_1 + math.pow(10, m).toInt * (mult_1 + mult_2 - ( fastMultiply((separacionIzquierda(a, m) - separacionDerecha(a, m)), (separacionIzquierda(b, m) - separacionDerecha(b, m)))))  + mult_2

      }
    }

    decisionMultiplicar(a, b)
  }


  def peasantAlgorithm(a: Int, b: Int): Int = {
    if (a == 0) 0
    else if (a%2 != 0) peasantAlgorithm(a/2, b+b) +  b
    else peasantAlgorithm(a/2, b+b)
  }

  def peasantAlgorithmIt(a: Int, b: Int): Int = {
    @tailrec
    def algorithmIter(a: Int, b: Int, acc: Int): Int = {
      if (a == 0) acc
      else {
        val nuevoAcum = if (a%2!=0) acc + b else acc
        algorithmIter(a/2, b+b, nuevoAcum)
      }
    }

    algorithmIter(a, b, 0)
  }
}

