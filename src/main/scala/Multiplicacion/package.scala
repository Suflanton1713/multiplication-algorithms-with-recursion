import scala.annotation.tailrec

package object Multiplicacion {


  //Split Algorithm with lineal recursion
  def splitMultiply(a: Int, b: Int): Int = {

    //Función para ver cuantas cifras tiene el número que se le entrega
    def digitosDecimales(a: Int, c: Int): Int = {
      if ((a / 10) >= 1) digitosDecimales((a / 10), c + 1) else c
    }


    //Función que usa la división en potencias de 10 para eliminar los m digitos de la derecha de un número con n digitos
    //Extrae los m digitos de la izquierda de un número, o los m+1 digitos en caso de tener cifras impares
    //El número corresponde a n cifras tal que m = FunPiso((n/2))
    def separacionIzquierda(a: Int, m: Int): Int = a / math.pow(10, m).toInt

    //Función que usa el modulo en potencias de 10  para extraer los m digitos de la derecha de un número con n digitos
    //Extrae los m digitos de la derecha de un número de n cifras.
    //El número corresponde a n cifras tal que m = FunPiso((n/2))
    def separacionDerecha(a: Int, m: Int): Int = a % math.pow(10, m).toInt

    def decisionMultiplicar(a: Int, b:Int):Int = if (a>b) multiplicarNumero(a,b) else multiplicarNumero(b,a)


    //Función que se encarga de hacer la multiplicación de dos números
    //Dividiéndolos en números de menos cifras, siguiendo lo siguiente (a*b) = 10^(2m) * (x*z) + 10^m * (y*z+x*w) + (y*w)
    //El número a corresponde: a = 10^m * x + y, donde x son los digitos de la izq del número y y de la derecha
    //El número b corresponde: b = 10^m * z + w, donde z son los digitos de la izq del número y w de la derecha
    //La función solo realiza a*b cuando son números de una cifra.
    //El número corresponde a n cifras tal que m = FunPiso((n/2))
    def multiplicarNumero(a: Int, b: Int): Int = {
      //println(s"Multiplicaremos $a x $b")
      val n = digitosDecimales(a, 1)
      val n_dos = digitosDecimales(b, 1)
      val m = n / 2
      //println(s"Digitos de a $n")
      //println(s"Digitos de b $n_dos")
      //println(s"m: $m")
      if ((n == 1) && (n_dos == 1)){
        //println(s"$a*$b")
        a * b
      }
      else{
        //println(s"10^(2*$m) * (${separacionIzquierda(a, m)} * ${separacionIzquierda(b, m)}) + " +
          //s"10^($m) * (${separacionDerecha(a, m)} * ${separacionIzquierda(b, m)}) + " +
          //s"(${separacionIzquierda(a, m)} * ${separacionDerecha(b, m)}) + " +
          //s"(${separacionDerecha(a, m)} * ${separacionDerecha(b, m)})")

        val resultado = (math.pow(10, 2 * m).toInt) * decisionMultiplicar(separacionIzquierda(a, m), separacionIzquierda(b, m)) +
          (math.pow(10, m).toInt * (decisionMultiplicar(separacionDerecha(a, m), separacionIzquierda(b, m)) + decisionMultiplicar(separacionIzquierda(a, m), separacionDerecha(b, m)))) +
          decisionMultiplicar(separacionDerecha(a, m), separacionDerecha(b, m))

        //println(s"Resultado: $resultado")

        resultado

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
      //println(s"Multiplicaremos $a x $b")
      val n = digitosDecimales(a, 1)
      val n_dos = digitosDecimales(b, 1)
      val m = n / 2
      //println(s"Digitos de a $n")
      //println(s"Digitos de b $n_dos")
      //println(s"m: $m")
      if ((n == 1) && (n_dos == 1)) {
        //println(s"$a*$b")
        a * b
      }
      else {

        val mult_1= decisionMultiplicar(separacionIzquierda(a, m), separacionIzquierda(b, m))
        val mult_2 = decisionMultiplicar(separacionDerecha(a, m), separacionDerecha(b, m))

        //println(s"10^(2*$m) * $mult_1 + " +
          //s"10^($m) * ($mult_1 + $mult_2 - ((${separacionIzquierda(a, m)} - ${separacionDerecha(a, m)}) * " +
          //s"(${separacionIzquierda(b, m)} - ${separacionDerecha(b, m)}))) + " +
          //s"$mult_2")


        val resultado = math.pow(10, 2 * m).toInt * mult_1 + math.pow(10, m).toInt * (mult_1 + mult_2 - ( (separacionIzquierda(a, m) - separacionDerecha(a, m)) * (separacionIzquierda(b, m) - separacionDerecha(b, m))))  + mult_2

        //println(s"Resultado: $resultado")

        resultado

      }
    }

    decisionMultiplicar(a, b)
  }



  def peasantAlgorithm_Iterativo(a: Int, b: Int): Int = {

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

  def peasantAlgorithm_recursivoLineal(a: Int, b: Int): Int = {

    def algorithmIter(a: Int, b: Int): Int = {
      if (a == 1) b
      else if (a%2 !=0) b + algorithmIter(a/2, b+b) else algorithmIter(a/2, b+b)
    }

    algorithmIter(a, b)
  }
}