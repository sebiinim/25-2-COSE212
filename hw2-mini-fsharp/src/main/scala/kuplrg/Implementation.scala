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


  private def eqv(v1: Value, v2: Value): Boolean = (v1, v2) match {
    case (NumV(a), NumV(b)) => (a == b)
    case (BoolV(a), BoolV(b)) => (a == b)
    case (NoneV, NoneV) => true
    case (SomeV(a), SomeV(a)) => eqv(a, b)
    case (ListV(a), ListV(b)) => a.length == b.length && a.lazyZip(b).forall { case (x, y) => eqv(x, y) }
    case (TupleV(a), TupleV(b)) => a.length == b.length && a.lazyZip(b).forall { case (x, y) => eqv(x, y) }
    case _ => false
  }

  // 패턴 매칭으로 환경 확장, PNum, PBool, PId, PNil, PCons, PTuple, PNone, PSome
  private def tryExtend(env: Env, pat: Pattern, v: Value): Option[Env] = (pat, v) match {
    case (PNum(a), NumV(b)) if (a == b) => Some(env)
    case (PBool(a), BoolV(b)) if (a == b) => Some(env)
    case (PId(a), )
  }

  def interp(expr: Expr, env: Env): Value = ???
    


  // ---------------------------------------------------------------------------
  // Problem #2
  // ---------------------------------------------------------------------------
  def hanoiMovesBody: String = ???
}
