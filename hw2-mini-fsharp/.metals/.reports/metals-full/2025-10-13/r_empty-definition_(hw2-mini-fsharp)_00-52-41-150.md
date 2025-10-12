file:///D:/coding/COSE212-plrg-hw/hw2-mini-fsharp/src/main/scala/kuplrg/Implementation.scala
empty definition using pc, found symbol in pc: 
semanticdb not found
empty definition using fallback
non-local guesses:
	 -value.
	 -value#
	 -value().
	 -scala/Predef.value.
	 -scala/Predef.value#
	 -scala/Predef.value().
offset: 5803
uri: file:///D:/coding/COSE212-plrg-hw/hw2-mini-fsharp/src/main/scala/kuplrg/Implementation.scala
text:
```scala
package kuplrg

object Implementation extends Template {

  import Expr.*, Value.*, Pattern.*

  // ---------------------------------------------------------------------------
  // Problem #1
  // ---------------------------------------------------------------------------
    // ---- 에러 메세지 상수 ----
  private val E_FREE_ID   = "free identifier"
  private val E_INV_OP    = "invalid operation"
  private val E_NOT_FUN   = "not a function"
  private val E_NOT_BOOL  = "not a boolean"
  private val E_NOT_LIST  = "not a list"
  private val E_INV_PM    = "invalid pattern match"
  private val E_UNMATCHED = "unmatched value"

  // Value: NumV, Boolv, ListV, TupleV, NoneV, SomeV, CloV

  // Value를 실제 값으로 바꾸기
  private def asNum(v: Value): BigInt = v match {
    case NumV(n) => n
    case _ => error(E_INV_OP)
  }

  private def asBool(v: Value): Boolean = v match {
    case BoolV(b) => b
    case _ => error(E_NOT_BOOL)
  }

  private def asList(v: Value): List[Value] = v match {
    case ListV(xs) => xs
    case _ => error(E_NOT_LIST)
  }

  private def asTuple(v: Value): List[Value] = v match {
    case TupleV(vs) => vs
    case _ => error(E_INV_OP)
  }

  private def asSome(v: Value): Value = v match {
  case SomeV(inner) => inner
  case _            => error(E_INV_PM)
  }

  private def asNone(v: Value): Unit = v match {
  case NoneV => ()
  case _     => error(E_INV_PM)
  }

  private def asClo(v: Value): (Pattern, Expr, () => Env) = v match {
    case CloV(pat, body, envThunk) => (pat, body, envThunk)
    case _ => error(E_NOT_FUN)
  }


  // equality 검사
  private def eqv(v1: Value, v2: Value): Boolean = (v1, v2) match {
    case (NumV(a), NumV(b)) => (a == b)
    case (BoolV(a), BoolV(b)) => (a == b)
    case (NoneV, NoneV) => true
    case (SomeV(a), SomeV(b)) => eqv(a, b)
    case (ListV(a), ListV(b)) => a.length == b.length && a.lazyZip(b).forall { case (x, y) => eqv(x, y) }
    case (TupleV(a), TupleV(b)) => a.length == b.length && a.lazyZip(b).forall { case (x, y) => eqv(x, y) }
    case _ => false
  }

  // 패턴 매칭으로 환경 확장, PNum, PBool, PId, PNil, PCons, PTuple, PNone, PSome
  // Pattern과 Value를 주면 확장된 Env 또는 None을 제공.
  private def tryExtend(env: Env, pat: Pattern, v: Value): Option[Env] = (pat, v) match {
    case (PNum(a), NumV(b)) if (a == b) => Some(env)
    case (PBool(a), BoolV(b)) if (a == b) => Some(env)
    case (PId(a), b) => Some(env + (a -> b))
    case (PNil, ListV(Nil)) => Some(env)

    // Cons는 리스트를 하나씩 쌓아 만드는 연산, x :: xs는 xs앞에 x를 붙이는 연산
    // ph는 단일 원소 가리키는 PId, pt는 나머지 리스트 패턴 가리키는 PId, h는 실제 head, t는 실제 tail
    case (PCons(ph, pt), ListV(h :: t)) => 
      for {
        e1 <- tryExtend(env, ph, h)
        e2 <- tryExtend(e1, pt, ListV(t)) // e1이라는 env에서 tail부분 패턴매칭
      } yield e2
    // for ~ yield 문법, 안에 있는 모든 요소가 성공하면 yield를 리턴, 하나라도 실패하면 None을 리턴. 
    
    case (PTuple(a), TupleV(b)) if (a.length == b.length) => 
      a.zip(b).foldLeft(Option(env)) {
        case (acc, (p, v)) => acc.flatMap(tryExtend(_, p, v)) 
        // acc는 현재까지 누적된 env, (p, v)는 현재 (패턴, 값)
        // flatMap은 acc에 있는 값을 꺼내서 _ 에 넣고 함수를 실행한 후 그 함수가 반환하는 값을 리턴.
        // 여기서는 acc(Env)를 _ 에 넣고 tryExtend(acc, p, v)를 실행, 그 결과(Option)을 리턴. 
        // 이걸 a.zip(b)로 만든 리스트의 모든 원소에 대해 반복. 중간에 오류나면 None 리턴. 
      }

    // None과 Some은 둘 다 Option의 일부. null보다 안전하게 없을 수도 있는 값을 표현. 
    case (PNone, NoneV) => Some(env)
    case (PSome(a), SomeV(b)) => tryExtend(env, a, b)  

    // 그 외 매칭 실패
    case _ => None
  }

  // 값이 반드시 필요해 None이 아니라 error를 리턴해야 하는 곳에서 사용하는 extend
  private def extendOrError(env: Env, pat: Pattern, v: Value): Env = 
    tryExtend(env, pat, v).getOrElse(error(E_INV_OP))

  // expr을 env에서 interpret
  def interp(expr: Expr, env: Env): Value = expr match {

    case ENum(n) => NumV(n)
    case EBool(b) => BoolV(b)
    case EId(a) => env.getOrElse(a, error(E_FREE_ID))
    
    case ENeg(a) => 
      NumV(asNum(interp(a, env)))
    
    case EAdd(l, r) => 
      NumV(asNum(interp(l, env)) + asNum(interp(r, env)))

    case EMul(l, r) => 
      NumV(asNum(interp(l, env)) * asNum(interp(r, env)))
    
    case EDiv(l, r) => 
      val n1 = asNum(interp(l, env)); val n2 = asNum(interp(r, env))
      if (n2 == 0) error(E_INV_OP) else NumV(n1 / n2)
    
    case EMod(l, r) => 
      val n1 = asNum(interp(l, env)); val n2 = asNum(interp(r, env))
      if (n2 == 0) error(E_INV_OP) else NumV(n1 % n2)

    case EEq(l, r) => 
      BoolV(eqv(interp(l, env), interp(r, env)))

    case ELt(l, r) => 
      BoolV(asNum(interp(l, env)) < asNum(interp(r, env)))

    case EIf(c, t, e) => 
      if (asBool(interp(c, env))) interp(t, env) else interp(e, env)

    case ENil => ListV(Nil)

    case ECons(h, t) => 
      val vh = interp(h, env)
      val vt = interp(t, env)
      val lst = asList(vt)
      ListV(vh :: lst)

    case ETuple(e) => 
     TupleV(e.map(interp(_, env)))

    case ENone => NoneV

    case ESome(v) => SomeV(interp(v, env))

    case ELet(p, v, s) => 
      val v1 = interp(v, env)
      val env2 = extendOrError(env, p, v1)
      interp(s, env2)

    case ERec(funs, scope) => 
      lazy val recEnv: Env = {
        funs.foldLeft(env) {
          case (acc, NamedFun(name, param, fbody)) => 
            acc + (name -> CloV(param, fbody, () => recEnv))
        }
      } 
      interp(scope, recEnv)

    case EFun(p, b) => 
      CloV(p, b, () => env) 

    case EApp(fun, args) => 
      val f = interp(fun, env)
      val a = interp(args, env)
      val (param, body, envThunk) = asClo(f)
      val env2 = extendOrError(envThunk(), param, a)
      interp(body, env2)

    case EMatch(value, cases) => 
      val v = interp(v@@alue, env)
      def loop(cs: List[Case]): Value = cs match {
        case Nil => error(E_UNMATCHED)
        case Case(p, b) :: rest => 
          tryExtend(env, p, v) match {
            case Some(env2) => interp(b, env2)
            case None => loop(rest)
          }
      }
      loop(cases)
  }
    


  // ---------------------------------------------------------------------------
  // Problem #2
  // ---------------------------------------------------------------------------
  def hanoiMovesBody: String = ???
}

```


#### Short summary: 

empty definition using pc, found symbol in pc: 