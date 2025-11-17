file:///D:/coding/COSE212-plrg-hw/hw3-mini-python/src/main/scala/kuplrg/Implementation.scala
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 3932
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

          // 4-2. block
          // Block(Statements의 List), 각 stmt를 IStmt로 바꾸기
          case IBlock(env, Block(stmts)) =>
            val newK = stmts.map(IStmt(env, _)) ::: ks
            State(newK, s, h, m)

          // 4-1. Statement
          // IStmt는 다시 여러 Stmt로 case가 나눠진다.
          case IStmt(env, stmt) => stmt match
            
            // Pass면 아무것도 안 함
            case SPass => State(ks, s, h, m)  

            // Expr(e)이면 IDrop 추가
            case SExpr(e) => State(IExpr(env, e) :: IDrop :: ks, s, h, m)

            // Assign(x=e), e를 x의 addr에 저장
            case SAssign(x, e) => 
              val addr = env(x)
              State(IExpr(env, e) :: IWrite(addr) :: ks, s, h, m)

            // SetItem e0[e1]=e2
            case SSetItem(e0, e1, e2) =>
              State(IExpr(env, e2) :: IExpr(env, e0) :: IExpr(env, e1) :: ks, s, h, m)

            // If cond then B else B
            case SIf(cond, thenB, elseB) => 
              val kv = KValue(IBlock(env, thenB) :: ks, s, h, m)
              State(IExpr(env, cond) :: IJmpIf(kv) :: IBlock(env, elseB) :: ks, s, h, m)

            // while e: B
            case SWhile(cond, body) => 
              val psiCond = KValue(IStmt(env, SWhile(cond, body)) :: ks, s, h)
              val psiBreak = KValue(ks, s, h)

              val Body: Handler = h + (Continue->psiCond) + (Break->psiBreak)  // 여기 대소문자 확인!
              val psiBody = KValue(IBlock(env, body) :: IStmt(env, SWhile(cond, body) :: ks, s, hBody))
              State(IExpr(env, cond) :: IJumIf(psiBody) :: ks, s, h, m)

            // Break면 jmp
            case SBreak =>
              State(IJmp(Break) :: ks, s, h, m)

            // Continue면 jmp
            case SContinue => 
              State(IJmp(Continue) :: ks, s, h, m)

            // try / except
            case STry(body, except) => 
              val psiRaise = KValue(IBlock(env, except) :: ks, s, h)
              val psiFianlly = KValue(ks, s, h)

              val hBody = h + (Raise->psiRaise) + (Finally->psiFinally)

              State(IBlock(env, body) :: IJmp(Finally) :: Nil, s, hBody, m)

            // raise error
            case SRaise =>
              State(Raise(RuntimeError) :: Nil, s, h, m)

            // function def
            case SDef(name, params, body) => 
              val addr = newAddr(m) // 새로운 주소 만들기
              val isGen = hasYield(body)  // 
              val clo = 
                if isGen then
                  GenV(params, body, env) // gen은 제너레이터 함수 클로저(yield를 포함)
                else
                  CloV(params, body, env)
              val mem1 = m + (addr->clo)

              State(IWrite(env(name)) :: ks, AddrV(addr) :: s, h, mem1) 
              // name이 가리키는 주소(int)에 addrV 값을 저장, mem의 그 주소에 clo가 저장
              // env: [..., funName->42], addr: 42, mem: [..., 42->funClo]

            // return
            case SReturn(expr) => 
              State(IExpr(env, expr) :: IReturn :: ks, s, h, m)

            // yield
            case SYield(expr) => 
              State(IExpr(env, expr) :: IYield :: ks, s, h, m)


          // 4-3. Expression
          case IExpr(env, expr) => 
            expr match
              
              case ENone => 
                State(ks, None :: s, h, m)

              case ENum(number) => 
                State(ks, number :: s, h, m)

              case EBool(bool) => 
                State(ks, bool :: s, h, m)

              case EId(name) => 
                if(env(@@))
            



  // Helper func
  // 안 쓰인 주소 만드는 함수
  private def newAddr(memory: Mem): Addr = 
    if mem.isEmpty then 0 else mem.keys.max + 1
        
  // block이 yield문을 포함하는지
  private def hasYield(block: Block): Boolean = 
    block.stmts.exists(hasYieldStmt)

  // stmt가 yield문을 포함하는지
  private def hasYieldStmt(stmt: Stmt): Boolen = stmt match
    case SYield(expr) => true
    case SDef(name, params, body) => hasYieldStmt(body)
    case SIf(cond, thenBlock, elseBlock) => hasYieldStmt(thenBlock) || hasYieldStmt(elseBlock)
    case STry(body, except) => hasYieldStmt(body) || hasYieldStmt(except)
    case SWhile(cond, body) => hasYieldStmt(body)
    case _ => false

  


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