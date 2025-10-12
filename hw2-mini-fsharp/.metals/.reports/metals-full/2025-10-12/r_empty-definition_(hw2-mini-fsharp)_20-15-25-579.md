file:///D:/coding/COSE212-plrg-hw/hw2-mini-fsharp/src/main/scala/kuplrg/Implementation.scala
empty definition using pc, found symbol in pc: 
semanticdb not found
empty definition using fallback
non-local guesses:
	 -NumV.
	 -NumV#
	 -NumV().
	 -scala/Predef.NumV.
	 -scala/Predef.NumV#
	 -scala/Predef.NumV().
offset: 764
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
  private def asNum(v: Value): BigInt = v match {
    case N@@umV(n) => n
    case _ => error(E_Inv_OP)
  }

  private def asBool(v: Value): Boolean = v match {
    
  }

  def interp(expr: Expr, env: Env): Value = ???
    


  // ---------------------------------------------------------------------------
  // Problem #2
  // ---------------------------------------------------------------------------
  def hanoiMovesBody: String = ???
}

```


#### Short summary: 

empty definition using pc, found symbol in pc: 