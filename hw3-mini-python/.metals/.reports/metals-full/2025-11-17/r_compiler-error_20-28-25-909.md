file:///D:/coding/COSE212-plrg-hw/hw3-mini-python/src/main/scala/kuplrg/Implementation.scala
### java.lang.IllegalArgumentException: Comparison method violates its general contract!

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 7828
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

          // 4.2 block
          // Block(Statements의 List), 각 stmt를 IStmt로 바꾸기
          case IBlock(env, Block(stmts)) =>
            val newK = stmts.map(IStmt(env, _)) ::: ks
            State(newK, s, h, m)

          // 4.1 Statement
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


          // 4.3 Expression
          case IExpr(env, expr) => 
            expr match
              
              case ENone => 
                State(ks, None :: s, h, m)

              case ENum(number) => 
                State(ks, number :: s, h, m)

              case EBool(bool) => 
                State(ks, bool :: s, h, m)

              case EId(name) => 
                env.get(name) match 
                  case Some(a) => 
                    State(ks, m(env(name)) :: s, h, m)
                  case None => 
                    State(IRaise(NameError(name)) :: Nil, s, h, m)
              
              case EBOp(bop, left, right) => 
                State(IExpr(env, left) :: IExpr(env, right) :: IBOp(bop) :: ks, s, h, m)

              case EList(elements) => 
                State(elements.map(IExpr(env, _)) ::: IList(elements.length) :: ks, s, h, m)

              case EAppend(list, elem) => 
                State(IExpr(env, list) :: IExpr(env, elem) :: IAppend :: ks, s, h, m)

              case EGetItem(list, idx) => 
                State(IExpr(env, list) :: IExpr(env, idx) :: IGetItem :: ks, s, h, m)

              case ELambda(params, body) => 
                val addr = newAddr(m)
                val block = Block(SReturn(body))  // closure의 body는 항상 Block이어야
                val clo = CloV(params, block, env)
                val mem1 = m + (addr->clo)
                State(ks, addrV(addr) :: s, h, mem1)

              case EApp(fun, args) => 
                State(IExpr(env, fun) :: args.map(IExpr(env, _)) ::: ICall(fun.length) :: ks, s, h, m)

              case ECond(cond, thenExpr, elseExpr) => 
                val psi = State(IExpr(env, cond) :: ks, s, h, m)
                State(IExpr(env, thenExpr) :: IJmpIf(psi) :: IExpr(env, elseExpr) :: ks, s, h, m)

              case EIter(expr) => 
                State(IExpr(env, expr) :: IIter :: ks, s, h, m)

              case ENext(expr) => 
                State(IExpr(env, expr) :: INext :: ks, s, h, m)

          // 4.4 Binary Operation
          case IBOp(bop) => 
            (bop, s) match

              case (Add, NumV(n2) :: NumV(n1) :: ss) => State(ks, NumV(n1 + n2) :: ss, h, m)

              case (Mul, NumV(n2) :: NumV(n2) :: ss) => State(ks, NumV(n1 * n2) :: ss, h, m)

              case (Div, NumV(0) :: NumV(n1) :: ss) => State(Raise(ZeroDivisionError) :: Nil, ss, h, m)

              case (Div, NumV(n2) :: NumV(n1) :: ss) => State(ks, NumV(n1/n2) :: ss, h, m)

              case (Mod, NumV(0) :: NumV(n1) :: ss) => State(Raise(ZeroDivisionError) :: Nil, ss, h, m)

              case (Mod, NumV(n2) :: NumV(n2) :: ss) => State(ks, NumV(n1%n2) :: ss, h, m)

              case (Eq, v2 :: v1 :: ss) => State(ks, Equal(v1, v2, m) :: ss, h, m)

              case (Is, v2 :: v1 :: ss) => State(ks, Is(v1, v2) :: ss, h, m)

            
          

          
            



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

  private def is(v1: Value, v2: Value): Boolean = (v1, v2) match
    case (NoneV, NoneV) => true
    case (NumV(n1), NumV(n2)) => n1 == n2
    case (BoolV(b1), BoolV(b2)) => b1 == b2
    case (AddrV(a1), AddrV(a2)) => a1 == a2
    case _ => false
  
  private def equal(v1: Value, v2: Value, mem: Mem): Boolean = (v1, v2) match
    case _ if is(v1, v2) => true
    case (AddrV(a1), AddrV(a2)) => 
      (mem.get(a1), mem.get(a2)) match 
        case (Some(v1p), Some(v2p)) => equal(v1p, v2p, mem)
        case _ => false
    case (ListV(xs), ListV(ys)) if xs.length == ys.length => 
      xs.zip(ys).forall {(x, y) => equal(x, y, me@@)} 
  


  def locals(block: Block): Set[String] = ???
}

```



#### Error stacktrace:

```
java.base/java.util.TimSort.mergeLo(TimSort.java:781)
	java.base/java.util.TimSort.mergeAt(TimSort.java:518)
	java.base/java.util.TimSort.mergeCollapse(TimSort.java:448)
	java.base/java.util.TimSort.sort(TimSort.java:245)
	java.base/java.util.Arrays.sort(Arrays.java:1234)
	scala.collection.SeqOps.sorted(Seq.scala:728)
	scala.collection.SeqOps.sorted$(Seq.scala:719)
	scala.collection.immutable.List.scala$collection$immutable$StrictOptimizedSeqOps$$super$sorted(List.scala:79)
	scala.collection.immutable.StrictOptimizedSeqOps.sorted(StrictOptimizedSeqOps.scala:82)
	scala.collection.immutable.StrictOptimizedSeqOps.sorted$(StrictOptimizedSeqOps.scala:82)
	scala.collection.immutable.List.sorted(List.scala:79)
	dotty.tools.pc.completions.Completions.completions(Completions.scala:145)
	dotty.tools.pc.completions.CompletionProvider.completions(CompletionProvider.scala:139)
	dotty.tools.pc.ScalaPresentationCompiler.complete$$anonfun$1(ScalaPresentationCompiler.scala:150)
```
#### Short summary: 

java.lang.IllegalArgumentException: Comparison method violates its general contract!