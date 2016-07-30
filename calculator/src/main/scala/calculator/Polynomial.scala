package calculator

object Polynomial {
  val delta = new Var[Double](0)
  val solution = new Var[Set[Double]](Set())
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    delta() = Math.pow(b(), 2) - 4*a()*c()
    delta
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
      solution() = calculate(a(), b(), c(), delta())
      solution
  }

  private def calculate(a: Double, b:Double, c:Double, delta:Double):Set[Double] = {
    var set = Set[Double]()
    delta match {
      case x if(x < 0) =>
      case 0 => set += (-b/(2*a))
      case _ => {
        set += ((-b + Math.sqrt(delta))/(2*a))
        set += ((-b - Math.sqrt(delta))/(2*a))
      }
    }
    set
  }
}
