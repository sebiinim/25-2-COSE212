package kuplrg

object Implementation extends Template {

  // ---------------------------------------------------------------------------
  // Basic Data Types
  // ---------------------------------------------------------------------------
  def clamp(lower: Int, x: Int, upper: Int): Int = {
    if (x < lower) lower
    else if (x > upper) upper
    else x
  }

  // non-empty, start with uppercase, length <= 10
  def validName(name: String): Boolean = {
    if (name.nonEmpty && name.headOption.exists(_.isUpper) && name.length <= 10) true
    else false

    // headOption : 첫 번째 Char을 가져옴. 없으면 None
    // 두 번째 글자라면 그냥 name(1) 써도 되지만 없을 시 오류 발생하니까 name.lift(1) 써서 없을 때는 None을 반환하도록 하자.
    // _.isUpper 로 익명 함수 축약. 원래는 ch => ch.isUpper로 Char 받아서 Boolean 내놓는 함수.
  }


  // ---------------------------------------------------------------------------
  // Functions
  // ---------------------------------------------------------------------------

  def collatzLength(n: Int): Int = {
    // n=1 이면 1, n이 짝수면 n/2, n이 홀수면 3n+1 로 바뀔 때 몇 번 바뀌어야 하는지.
    // 단계마다 1씩 더하면서 재귀적으로 호출한다. 길이 저장용 변수가 딱히 필요 없다.

    if (n == 1) 1
    else if (n % 2 == 0) 1 + collatzLength(n/2)
    else 1 + collatzLength(3*n+1)
  }

  def fixpoint(f: Int => Int): Int => Int = n => {
    // 입력은 f라는 Int => Int 함수, 출력은 새로운 Int => Int 함수, 이 새 함수에 정수 n을 대입해 본문을 실행.
    // 고차함수라서 이해가 어려웠다. 함수 원형 마지막의 =n은 fixpoint의 결과인 Int => Int 함수에 입력으로 n을 넣는다는 뜻.

    val next = f(n) // 일단 f에 대입
    if (next == n) n // 값이 바뀌지 않았으면 종료
    else fixpoint(f)(next) // 값이 바뀌었으면 함수는 f로 같고 이번에는 next를 대입.
  }

  def applyK(f: Int => Int, k: Int): Int => Int = ???

  // ---------------------------------------------------------------------------
  // Collections
  // ---------------------------------------------------------------------------
  def sumEven(l: List[Int]): Int = ???

  def double(l: List[Int]): List[Int] = ???

  def generate(f: Int => Int): Int => List[Int] = ???

  def join(l: Map[String, Int], r: Map[String, Int]): Map[String, Int] = ???

  def subsets(set: Set[Int]): List[Set[Int]] = ???

  // ---------------------------------------------------------------------------
  // Trees
  // ---------------------------------------------------------------------------
  import Tree.*

  def heightOf(t: Tree): Int = ???

  def max(t: Tree): Int = ???

  def postorder(t: Tree): List[Int] = ???

  def count(t: Tree, f: Int => Boolean): Int = ???

  def merge(left: Tree, right: Tree): Tree = ???

  // ---------------------------------------------------------------------------
  // Boolean Expressions
  // ---------------------------------------------------------------------------
  import BE.*

  def isImply(expr: BE): Boolean = ???

  def noAnd(expr: BE): Boolean = ???

  def subExprs(expr: BE): Set[BE] = ???

  def getString(expr: BE): String = ???

  def eval(expr: BE, env: Map[String, Boolean]): Boolean = ???
}
