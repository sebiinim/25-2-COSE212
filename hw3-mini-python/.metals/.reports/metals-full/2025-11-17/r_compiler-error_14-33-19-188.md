file:///D:/coding/COSE212-plrg-hw/hw3-mini-python/src/main/scala/kuplrg/Implementation.scala
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 383
uri: file:///D:/coding/COSE212-plrg-hw/hw3-mini-python/src/main/scala/kuplrg/Implementation.scala
text:
```scala
package kuplrg

object Implementation extends Template {

  import Stmt.*, Expr.*, Value.*, BOp.*, Inst.*, Control.*, Error.*

  def reduce(st: State): State =
    val State(k, s, h, m) = st
    
    k match
      case Nil => error("empty cont")

      case inst :: ks =>
        inst match
          case IBlock(env, Block(stmts)) =>
            val newK = stmts.map(@@)
        
    

  def locals(block: Block): Set[String] = ???
}

```



#### Error stacktrace:

```
scala.collection.LinearSeqOps.apply(LinearSeq.scala:129)
	scala.collection.LinearSeqOps.apply$(LinearSeq.scala:128)
	scala.collection.immutable.List.apply(List.scala:79)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:244)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:101)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:88)
	dotty.tools.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:46)
	dotty.tools.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:435)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: -1