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


    //Función que se encarga de hacer la multiplicación de dos números
    //Dividiéndolos en números de menos cifras, siguiendo lo siguiente (a*b) = 10^(2m) * (x*z) + 10^m * (y*z+x*w) + (y*w)
    //El número a corresponde: a = 10^m * x + y, donde x son los digitos de la izq del número y y de la derecha
    //El número b corresponde: b = 10^m * z + w, donde z son los digitos de la izq del número y w de la derecha
    //La función solo realiza a*b cuando son números de una cifra.
    //El número corresponde a n cifras tal que m = FunPiso((n/2))
    def multiplicarNumero(a: Int, b: Int): Int = {
      val n = digitosDecimales(a, 1)
      val m = n / 2
      if (n == 1) a * b
      else (math.pow(10, 2 * m).toInt) * multiplicarNumero(separacionIzquierda(a, m), separacionIzquierda(b, m)) + (math.pow(10, m).toInt * (multiplicarNumero(separacionDerecha(a, m), separacionIzquierda(b, m)) + multiplicarNumero(separacionIzquierda(a, m), separacionDerecha(b, m)))) + multiplicarNumero(separacionDerecha(a, m), separacionDerecha(b, m))
    }

    multiplicarNumero(a, b)
  }
}