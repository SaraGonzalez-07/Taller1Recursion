

package object Multiplicacion {

  //Version recursiva lineal del PeasantAlgorithm para multiplicar dos enteros positivos
  def PeasantAlgorithm(x: Int, y: Int): Int = {
    def peasantRecur (a:Int, b:Int):Int={

      if (a==0)0
      else if (a%2==0)PeasantAlgorithm (a/2, b+b)
      else PeasantAlgorithm (a/2, b+b)+b
    }
    peasantRecur(x,y)
  }


  // Version iterativa del PeasantAlgorithm para multiplicar dos enteros positivos
  def PeasantAlgorithmIt(x: Int, y: Int): Int = {
    def peasantIter(a: Int, b: Int, total: Int): Int = {
      if (a==0) total
      else if (a%2==0) peasantIter(a/2, b+b, total)
      else peasantIter(a/2, b+b, total+b)
    }
    peasantIter(x, y, 0)
  }

  //devuelve la multiplicacion de dos enteros recursivos usando el SplitAlgorithm
  def splitMultiply(x: Int, y: Int): Int = {
    if (x<10 && y<10) x*y

    else {

      val n=math.max(x.toString.length, y.toString.length)
      val m=n/2
      val pot=math.pow(10, m).toInt
      //potencia de 10 donde 10 esta elevado a 2*m
      val pot2=math.pow(10, 2*m).toInt

      val a1=x/pot
      val a0=x%pot
      val b1=y/pot
      val b0=y%pot

      val m1= splitMultiply(a1, b1)
      val m2= splitMultiply(a0, b1)
      val m3= splitMultiply(a1, b0)
      val m4= splitMultiply(a0, b0)

      pot2*m1+pot*(m2+m3)+m4
    }
  }


  // devuelve la multiplicacion de dos enteros recursivos usando el FastAlgorithm
  def fastMultiply(x: Int, y: Int): Int = {

    // caso base, si ambos numeros son de un solo digito se multiplican directamente
    if (x < 10 && y < 10) x * y

    else {
      val n = math.max(x.toString.length, y.toString.length)
      val m = n/2
      val pot = math.pow(10, m).toInt

      val a1 = x/pot
      val a0 = x%pot
      val b1 = y/pot
      val b0 = y%pot

      // las restas pueden dar negativo, por eso se maneja el signo aparte
      val diffA = a1-a0
      val diffB = b1-b0
      val diffAabs = math.abs(diffA)
      val diffBabs = math.abs(diffB)

      //aqui se maneja el signo de la resta
      val sign = diffA.sign*diffB.sign

      val p1 = fastMultiply(a1, b1)
      val p2 = fastMultiply(a0, b0)
      val p3 = fastMultiply(diffAabs, diffBabs)
      val p3signed = p3*sign

      pot*pot*p1+pot*(p1+p2-p3signed)+p2
    }
  }

}