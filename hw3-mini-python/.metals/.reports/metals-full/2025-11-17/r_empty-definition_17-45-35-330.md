error id: file:///D:/coding/COSE212-plrg-hw/hw3-mini-python/src/main/scala/kuplrg/Template.scala:`<none>`.
file:///D:/coding/COSE212-plrg-hw/hw3-mini-python/src/main/scala/kuplrg/Template.scala
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -Inst.Value.
	 -Value.
	 -scala/Predef.Value.
offset: 623
uri: file:///D:/coding/COSE212-plrg-hw/hw3-mini-python/src/main/scala/kuplrg/Template.scala
text:
```scala
package kuplrg

trait Template {

  import Inst.*

  def eval(str: String, debug: Boolean = false): String =
    def aux(st: State): String =
      if (debug) log(st.str)
      st match
        case State(Nil, List(v), _, mem) =>
          val result = v.str(mem)
          if (debug) log("Result: " + result)
          result
        case _ => aux(reduce(st))
    def log(msg: String): Unit = println("-" * 80 + "\n" + msg)
    val Program(stmts, expr) = Program(str)
    val xs = if (stmts.isEmpty) Nil else locals(Block(stmts)).toList
    val addrs = (0 until xs.size)
    val mem = addrs.map(_ -> Val@@ue.NoneV).toMap
    val env = (xs zip addrs).toMap
    val initK = stmts.map(IStmt(env, _)) ::: IExpr(env, expr) :: Nil
    val initSt = State(initK, Nil, Map.empty, mem)
    aux(initSt)

  def reduce(st: State): State

  def locals(block: Block): Set[String]
}

```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.