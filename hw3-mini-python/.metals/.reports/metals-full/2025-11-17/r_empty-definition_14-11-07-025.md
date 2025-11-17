error id: file:///D:/coding/COSE212-plrg-hw/hw3-mini-python/src/main/scala/kuplrg/Implementation.scala:`<none>`.
file:///D:/coding/COSE212-plrg-hw/hw3-mini-python/src/main/scala/kuplrg/Implementation.scala
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -Stmt.State#
	 -Expr.State#
	 -Value.State#
	 -BOp.State#
	 -Inst.State#
	 -Control.State#
	 -Error.State#
	 -State#
	 -scala/Predef.State#
offset: 161
uri: file:///D:/coding/COSE212-plrg-hw/hw3-mini-python/src/main/scala/kuplrg/Implementation.scala
text:
```scala
package kuplrg

object Implementation extends Template {

  import Stmt.*, Expr.*, Value.*, BOp.*, Inst.*, Control.*, Error.*

  def reduce(st: State): Sta@@te =
    val State(k, s, h, m) = st
    ???

  def locals(block: Block): Set[String] = ???
}

```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.