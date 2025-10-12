file:///D:/coding/COSE212-plrg-hw/hw2-mini-fsharp/src/main/scala/kuplrg/Implementation.scala
empty definition using pc, found symbol in pc: 
semanticdb not found
empty definition using fallback
non-local guesses:
	 -E_FREE_ID.
	 -E_FREE_ID#
	 -E_FREE_ID().
	 -scala/Predef.E_FREE_ID.
	 -scala/Predef.E_FREE_ID#
	 -scala/Predef.E_FREE_ID().
offset: 375
uri: file:///D:/coding/COSE212-plrg-hw/hw2-mini-fsharp/src/main/scala/kuplrg/Implementation.scala
text:
```scala
package kuplrg

object Implementation extends Template {

  import Expr.*, Value.*, Pattern.*

  // ---------------------------------------------------------------------------
  // Problem #1
  // ---------------------------------------------------------------------------
  def interp(expr: Expr, env: Env): Value = 
      // ---- 에러 메세지 상수 ----
  private val E_F@@REE_ID   = "free identifier"
  private val E_INV_OP    = "invalid operation"
  private val E_NOT_FUN   = "not a function"
  private val E_NOT_BOOL  = "not a boolean"
  private val E_NOT_LIST  = "not a list"
  private val E_INV_PM    = "invalid pattern match"
  private val E_UNMATCHED = "unmatched value"

  // ---------------------------------------------------------------------------
  // Problem #2
  // ---------------------------------------------------------------------------
  def hanoiMovesBody: String = ???
}

```


#### Short summary: 

empty definition using pc, found symbol in pc: 