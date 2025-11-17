file:///D:/coding/COSE212-plrg-hw/hw3-mini-python/src/main/scala/kuplrg/Implementation.scala
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 917
uri: file:///D:/coding/COSE212-plrg-hw/hw3-mini-python/src/main/scala/kuplrg/Implementation.scala
text:
```scala
package kuplrg

object Implementation extends Template {

  import Stmt.*, Expr.*, Value.*, BOp.*, Inst.*, Control.*, Error.*

  def reduce(st: State): State =
    // cont, stack, handler, memory
    val State(k, s, h, m) = st
    
    k match
      case Nil => error("empty cont")

      case inst :: ks =>
        inst match

          // Block(Statements의 List), 각 stmt를 IStmt로 바꾸기
          case IBlock(env, Block(stmts)) =>
            val newK = stmts.map(IStmt(env, _)) ::: ks
            State(newK, s, h, m)

          // IStmt는 다시 여러 Stmt로 case가 나눠진다.
          case IStmt(env, stmt) => stmt match
            
            // Pass면 아무것도 안 함
            case SPass => State(ks, s, h, m)  

            // Expr이면 IDrop 추가
            case SExpr(e) => State(IExpr(env, e) :: IDrop :: ks, s, h, m)

            // 
            case Assign(x, e) => 
              val addr = env(e@@State(IExpr(env, e) :: IWrite(x) :: ks, s, h, m)


          

        
    

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