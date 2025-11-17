file:///D:/coding/COSE212-plrg-hw/hw3-mini-python/src/main/scala/kuplrg/Implementation.scala
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 12752
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
      case Nil => Error("empty cont")

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

              case (Div, NumV(0) :: NumV(n1) :: ss) => State(IRaise(ZeroDivisionError) :: Nil, ss, h, m)

              case (Div, NumV(n2) :: NumV(n1) :: ss) => State(ks, NumV(n1/n2) :: ss, h, m)

              case (Mod, NumV(0) :: NumV(n1) :: ss) => State(IRaise(ZeroDivisionError) :: Nil, ss, h, m)

              case (Mod, NumV(n2) :: NumV(n2) :: ss) => State(ks, NumV(n1%n2) :: ss, h, m)

              case (Eq, v2 :: v1 :: ss) => State(ks, Equal(v1, v2, m) :: ss, h, m)

              case (Is, v2 :: v1 :: ss) => State(ks, Is(v1, v2) :: ss, h, m)

              case (Lt, v2 :: v1 :: ss) => 
                lessThan(v1, v2, m) match 
                  case Some(b) => State(ks, BoolV(b) :: ss, h, m)
                  case None => State(IRaise(TypeError) :: Nil, ss, h, m)
              
              case (Lte, v2 :: v1 :: ss) => 
                lessThan(v1, v2, m) match 
                  case Some(b) => State(ks, BoolV(b || equal(v1, v2, m)) :: ss, h, m)
                  case None => State(IRaise(TypeError) :: Nil, ss, h, m)

              case _ => State(IRaise(TypeError) :: Nil, s, h, m)  // 정의되지 않은 연산은 TypeError
          
          // 4.5 Other Instructions
          // 현재 스택에 있는 값을 addr에 쓰기
          case IWrite(addr) => 
              s match 
                case v :: ss => 
                  State(ks, ss, h, m + (addr->v))
              
          // addr에 해당하는 list에서 n번째 값을 읽는 inst
          case IGetItem => 
            s match 
              case n :: addrV(addr) :: ss => 
                (asInt(n), m.get(addr)) match
                  case (Some(n), Some(ListV(xs))) => 
                    val m = xs.length // 리스트 길이

                    if -m<=n && n<= then State(ks, xs(m+n), h, m)
                    else if 0<=n && n<m then State(ks, xs(n) :: ss, h, m)
                    else if n< -m || m<=n then State(IRaise(IndexError) :: Nil, s, h, m)
                    else State(IRaise(IndexError) :: Nil, s, h, m)
              case _ => State(IRaise(IndexError) :: Nil, s, h, m)
                
          // addr에 해당하는 list의 n번째 값을 설정하는 inst
          case ISetItem => 
            s match 
              case n :: addrV(addr) :: v :: ss => 
                (asInt(n), m.get(addr)) match
                  case (Some(n), Some(ListV(xs))) => 
                    val m = xs.length // 리스트 길이

                    if -m<=n && n<= then 
                      val updatedList = xs.updated(m+n, v)
                      State(ks, ss, h, m + (addr->ListV(updatedList)))
                    else if 0<=n && n<m then 
                      val updatedList = xs.updated(n, v)
                      State(ks, xs(n) :: ss, h, m + (addr->ListV(updatedList)))
                    else if n< -m || m<=n then State(IRaise(IndexError) :: Nil, s, h, m)
                    else State(IRaise(IndexError) :: Nil, s, h, m)
              case _ => State(IRaise(IndexError) :: Nil, s, h, m)

          // 리스트 만들기
          case IList(length) => 
            // 스택에서 n개 뽑기, splitAt 사용
            val (values, rest) = s.splitAt(n)
            val vs = values.reverse // 스택이니까 뒤집혀 있을 거다.
            val addr = newAddr(m)
            val mem = m + (addr->vs)
            State(ks, addr :: s, h, mem)

          // 리스트에 원소 추가
          case IAppend =>
            s match
              case v :: Addr(addr) :: ss =>
                m.get(addr) match 
                  case Some(ListV(xs)) =>
                    val newList = xs :+ v
                    State(ks, AddrV(addr) :: ss, h, m + (addr -> ListV(newList)))
                  case _ =>
                    State(IRaise(TypeError) :: Nil, ss, h, m)
              case _ => State(IRaise(TypeError) :: Nil, s, h, m)

          // 특정 cont로 jump
          case IJmpIf(kv) => 
            s match
              case v :: ss =>
                if isTruthy(v, m) then 
                  val KValue(kp, sp, hp) = kv // kv 해체
                  State(kp, sp, hp, m) 
                else State(ks, ss, h, m)
              case _ => State(IRaise(TypeError) :: Nil, s, h, m)

          //
          case IJmp(control) => 
            val KValue(kp, sp, hp) = h(control)  // h 해체
            State(kp, sp, hp, m)


          //
          case ICall(argsize) => 
            s match
              val (vals, rest) = s.splitAt(n+1) // vn, ... , v1과 a 추출
              
              val values = vals.init.reverse
              val a = vals.last

              m.get(a) match 
                case Some(CloV(params, body, cloEnv)) => 
                  val kPrime = IBlock(env, body) :: Return :: Nil
                  val hBody = h + (Return->ContV(k, rest, h))
                  val m1 = m + newAddr(m)
                  val m2 = m1 + 

            
              
          case IReturn => 
            h.get(Return) match
              case Some(KValue(kp, sp, hp)) => 
                s match 
                  case v :: ss => 
                    State(ks, v :: sp, hp, m)
                  case _ => State(IRaise(TypeError) :: Nil, s, h, m)
              case _ => State(IRaise(RuntimeError) :: Nil, s, h, m)

          // 
          case IYield => 
            h.get(Return) match
              case Some(KValue(kp, sp, hp)) => 
                s match
                  case v :: ss => 
                    val (kp, sp, hp) => h(Return)
                    State(kp, KValue(ks, ss, h) :: v :: sp, hp, m)
                  case _ => State(IRaise(TypeError) :: Nil, s, h, m)
              case _ => State(IRaise(RuntimeError) :: Nil, s, h, m)

          //
          case IIter =>
            s match 
              case AddrV(addr) :: rest =>
                m.get(addr) match 
                  case Some(IterV(_, i)) => 
                    State(ks, s, h, m)
                  case Some(ListV(_)) => 
                    val aIter = newAddr(m)
                    State(ks, AddrV(aIter) :: rest, h, m + (aIter->IterV(addr, 0)))
                  case _ => State(IRaise(TypeError) :: Nil, rest, h, m)
              case _ => State(IRaise(TypeError) :: Nil, s, h, m)

          //
          case INext => 
            s match
              case addr :: rest =>
                m.get(addr) match   // v
                  case Some(IterV(addr2, idx)) =>
                    m.get(addr2) match  // v' 
                      case Some(KValue(kp, sp, hp)) =>
                        val psiYield = KValue(IWrite(addr2) :: ks, rest, h)
                        val psiReturn = KValue(IDrop :: IRaise(StopIteration) :: Nil, rest, h)
                        val hNext = hp + (Yield->psiYield) + (Return->psiReturn)
                        State(kp, sp, hNext, m)
                      case Some(IList(xs)) =>
                        val mNext = m + (@@)
                        if (idx < xs.length) then State(ks, xs(idx) :: rest, h, 
 
                        
                    
                
            

            
            



            



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


  // 문서가 워낙 잘 나와 있어서 그대로만 만들면 된다.
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
      xs.zip(ys).forall {(x, y) => equal(x, y, mem)} 
    case _ => false
  
  // 하이고많다. lessThan은 Option임에 주의하자
  private def lessThan(v1: Value, v2: Value, mem: Mem): Option[Boolean] = (v1, v2) match
    case(NumV(n1), NumV(n2)) => Some(n1 < n2)
    case(AddrV(a1), AddrV(a2)) => 
      (mem.get(a1), mem.get(a2)) match
        case (Some(v1p), Some(v2p)) => lessThan(v1p, v2p, mem)
        
    // 리스트인 경우. 빈 리스트는 시작이 Nil인걸 이용하자. 하나씩 차근차근
    case (ListV(xs), ListV(ys)) =>
      (xs, ys) match
        case (Nil, Nil) => Some(false) // n=m=0
        case (Nil, _) => Some(true) // n=0<m
        case (_, Nil) => Some(false) // n>0=m

        case (x1::xss, y1::yss) => 
          (lessThan(x1, y1, mem), equal(x1, y1, mem)) match
            case (Some(bLt), bEq) => 
              if bLt then true
              else if !bLt && !bEq then false
              else lessThan(xss, yss, mem)
            case _ => false
        case _ => false
    case _ => false
          
  // NumV를 Int로 바꾸기
  private def asInt(v: Value): Option[Int] = v match
    case NumV(n) => Some(n)
    case _ => None
  
  // 아마도 점프할 cont가 멀쩡한 녀석인지 확인하기 위함인듯
  private def isTruthy(v: Value, mem: Mem): Boolean = v match
    case NoneV => false
    case NumV(n) if n!=0 => n
    case BoolV(b) => b
    case AddrV(a) => isTruthy(mem(a), mem)
    case ListV(xs) => 0<n
    case _ => true

      
  


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