package Multiplicacion

object Main {
  def main(args: Array[String]): Unit = {
    //val resultado = splitMultiply(17235,25675)
    val num = 12022333
    val num2 = 83893445
    val resultado2 = fastMultiply(num,num2)// Probamos con 2567 * 3
    //println(s"Resultado de la multiplicación split: $resultado")
    val resultado3 = peasantAlgorithm_recursivoLineal(num,num2)
    println(s"Resultado de la multiplicación fast: $resultado2")
    println(s"Resultado de la multiplicación fast: $resultado3")
    println("hi")
  }
}
