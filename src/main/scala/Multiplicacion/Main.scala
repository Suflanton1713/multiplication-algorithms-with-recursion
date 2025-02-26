package Multiplicacion

object Main {
  def main(args: Array[String]): Unit = {
    val resultado = splitMultiply(13,24) // Probamos con 2567 * 3
    val resultado2 = PeasantAlgorithm_recursivoLineal(13,24)
    println(s"Resultado de la multiplicación: $resultado")
    println("hi")
  }
}
