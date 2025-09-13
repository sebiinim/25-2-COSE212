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

  def applyK(f: Int => Int, k: Int): Int => Int = n => {
    // 함수 f를 k번 반복하는 함수, 인자는 따로 받는다
    // = n 이거 추가해도 되나요..? 없으면 도저히 방법이 없는데..

    if (k==0) n // k가 0이면 함수 적용을 할 필요가 없다.
    else applyK(f, k-1)(f(n)) // 이렇게 함수에 argument를 주면서 재귀 가능
  }

  // ---------------------------------------------------------------------------
  // Collections
  // ---------------------------------------------------------------------------
  def sumEven(l: List[Int]): Int = {
    // l 이라는 Int List를 받아서 거기서 짝수 항만 더한 값을 출력

    if (l == Nil) 0 // 빈 리스트이면 0 리턴, 여기 넘어가면 head가 존재함이 확정
    else if (l.head % 2 == 0) l.head + sumEven(l.tail) // 현재 리스트의 가장 앞이 짝수이면 더하고 다음 항
    else sumEven(l.tail)

    // l.filter(_%2==0).sum 으로 구현하면 최적이다.
  }

  def double(l: List[Int]): List[Int] = {
    // 모든 원소를 2번씩 반복해서 2배 길이 리스트 만들기
    // 강의자료 속에 답이 있다.
    l.flatMap(x => List(x, x))
  }

  def generate(f: Int => Int): Int => List[Int] = n => {
    // argument n에 함수 f를 적용하면서 그 결과를 리스트에 추가.
    // f(n) = n 이면 종료
    // 여기도 = n 을 추가해야 한다 괜찮겠지..

    val next = f(n)
    if (next == n) List(n)  // 이렇게 n 만 담은 List를 만들 수 있다.
    else n :: generate(f)(next)
  }

  def join(l: Map[String, Int], r: Map[String, Int]): Map[String, Int] = {
    // 두 개의 Map을 받아서 같은 key가 있으면 value를 더하고 다른 key는 그냥 쓰기

    val lrKeySet = l.keySet ++ r.keySet  // 집합끼리 합칠 때는 ++ 쓰자

    // for, while 대신 map 을 쓰자. lrKeySet의 모든 값 k에 대해서 k -> value의 합을 구한다. 이건 Set이니 Map으로 바꾼다.
    lrKeySet.map ( k =>
      k -> (l.getOrElse(k, 0) + r.getOrElse(k, 0))
    ).toMap
  }

  def subsets(set: Set[Int]): List[Set[Int]] = {
    // 상당한 난제다. Set을 받아서 가능한 모든 부분집합 Set을 사전순으로 출력해야 한다.
    // 앞쪽 수가 작을수록 먼저. 짧을수록 먼저.
    // 찾아보니 이걸 power Set 이라고 한다.

    val sortedList = set.toList.sorted // List로 바꿔야 sort가 된다.

    // 파워셋을 만들어주는 함수
    // foldLeft를 사용한다. 초깃값은 empty한 Int List의 List
    // foldLeft에 익명 함수를 준다. foldLeft는 항상 (누적값, 현재 원소) => 누적값 형태의 함수만 받는다.
    // acc는 누적 배열, x는 현재값.

    // 파워셋 만들기
    val all: List[List[Int]] =
      sortedList.foldLeft(List(List.empty[Int])) { (acc, x) =>
        acc ++ acc.map(xs => x :: xs)   // acc의 모든 원소(xs) 마다 앞에 foldLeft의 현재값 x를 붙이는 익명 함수
      }

    all.filter(_.nonEmpty)  // 공집합 제거
      .map(_.sorted)        // 원소 내부를 sort
      .distinct             // 겹치는 녀석 제거
      .sorted               // 원소를 sort
      .map(_.toSet)         // 모든 원소를 List에서 Set으로 변경
  }

  // ---------------------------------------------------------------------------
  // Trees
  // ---------------------------------------------------------------------------
  import Tree.*

  def heightOf(t: Tree): Int =


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
