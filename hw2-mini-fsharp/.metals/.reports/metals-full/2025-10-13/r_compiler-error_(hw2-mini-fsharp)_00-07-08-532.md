error id: 715D768B5F64B349906F998001909419
file:///D:/coding/COSE212-plrg-hw/hw2-mini-fsharp/src/main/scala/kuplrg/Implementation.scala
### java.lang.StringIndexOutOfBoundsException: Range [5304, 5304 + -2) out of bounds for length 5556

occurred in the presentation compiler.



action parameters:
offset: 5316
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

  // Value를 실제 값으로 바꾸기
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


  // equality 검사
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
  // Pattern과 Value를 주면 확장된 Env 또는 None을 제공.
  private def tryExtend(env: Env, pat: Pattern, v: Value): Option[Env] = (pat, v) match {
    case (PNum(a), NumV(b)) if (a == b) => Some(env)
    case (PBool(a), BoolV(b)) if (a == b) => Some(env)
    case (PId(a), b) => Some(env + (x -> b))
    case (PNil(a), ListV(Nil)) => Some(env)

    // Cons는 리스트를 하나씩 쌓아 만드는 연산, x :: xs는 xs앞에 x를 붙이는 연산
    // ph는 단일 원소 가리키는 PId, pt는 나머지 리스트 패턴 가리키는 PId, h는 실제 head, t는 실제 tail
    case (PCons(ph, pt), ListV(h :: t)) => 
      for {
        e1 <- tryExtend(env, ph, h)
        e2 <- tryExtend(e1, pt, ListV(t)) // e1이라는 env에서 tail부분 패턴매칭
      } yield e2
    // for ~ yield 문법, 안에 있는 모든 요소가 성공하면 yield를 리턴, 하나라도 실패하면 None을 리턴. 
    
    case (PTuple(a), TupleV(b)) if (a.length == b.length) => 
      a.zip(b).foldLeft(Option(env)) {
        case (acc, (p, v)) => acc.flatMap(tryExtend(_, p, v)) 
        // acc는 현재까지 누적된 env, (p, v)는 현재 (패턴, 값)
        // flatMap은 acc에 있는 값을 꺼내서 _ 에 넣고 함수를 실행한 후 그 함수가 반환하는 값을 리턴.
        // 여기서는 acc(Env)를 _ 에 넣고 tryExtend(acc, p, v)를 실행, 그 결과(Option)을 리턴. 
        // 이걸 a.zip(b)로 만든 리스트의 모든 원소에 대해 반복. 중간에 오류나면 None 리턴. 
      }

    // None과 Some은 둘 다 Option의 일부. null보다 안전하게 없을 수도 있는 값을 표현. 
    case (PNone, NoneV) => Some(env)
    case (PSome(a), SomeV(b)) => tryExtend(env, a, b)  

    // 그 외 매칭 실패
    case _ => None
  }

  // 값이 반드시 필요해 None이 아니라 error를 리턴해야 하는 곳에서 사용하는 extend
  private def extendOrError(env: Env, pat: Pattern, v: Value): Env = 
    tryExtend(env, pat, v).getOrElse(error(E_INV_OP))

  def interp(expr: Expr, env: Env): Value = expr match {

    case ENum(n) => NumV(n)
    case EBool(b) => BoolV(b)
    case EId(a) => env.getOrElse(a, error(E_FREE_ID))
    
    case ENeg(a) => 
      NumV(asNum(interp(a, env)))
    
    case EAdd(l, r) => 
      NumV(asNum(interp(l, env)) + asNum(interp(r, env)))

    case EMul(l, r) => 
      NumV(asNum(interp(l, env)) * asNum(interp(r, env)))
    
    case EDiv(l, r) => 
      val n1 = asNum(interp(l, env)); val n2 = asNum(interp(r, env))
      if (n2 == 0) error(E_INV_OP) else NumV(n1 / n2)
    
    case EMod(l, r) => 
      val n1 = asNum(interp(l, env)); val n2 = asNum(interp(r, env))
      if (n2 == 0) error(E_INV_OP) else NumV(n1 % n2)

    case EEq(l, r) => 
      BoolV(eqv(interp(l, env), interp(r, env)))

    case ELt(l, r) => 
      BoolV(asNum(interp(l, env)) < asNum(interp(r, env)))

    case EIf(c, t, e) => 
      if (asBool(interp(c, env))) interp(t, env) else interp(e, env)

    case ENil => ListV(Nil)

    case ECons(h, t) => 
      val vh = interp(h, env)
      val vt = interp(t, env)
      val lst = asList(vt)
      ListV(vh :: lst)

    case ETuple(e) => 
     TupleV(e.map(interp(_, env)))

    case ENone => NoneV

    case ESome(v) => SomeV(interp(v, env))

    case ELet(p, v, s) => 
      val v1 = interp(v, env)
      val env2 = extendOrError(env, p, v1)
      interp(s, env2)

    case ERec => ???

    case EFun(p, b) => 
      CloV(p, b, () => env) 

    case EApp(fun, args) => 
      val f = interp(fun, en@@)

  }
    


  // ---------------------------------------------------------------------------
  // Problem #2
  // ---------------------------------------------------------------------------
  def hanoiMovesBody: String = ???
}

```


presentation compiler configuration:
Scala version: 2.12.18
Classpath:
<WORKSPACE>\.bloop\hw2-mini-fsharp\bloop-bsp-clients-classes\classes-Metals-OfTOqK_fSWGtFxi5mjk0GQ== [exists ], <HOME>\AppData\Local\bloop\cache\semanticdb\com.sourcegraph.semanticdb-javac.0.11.0\semanticdb-javac-0.11.0.jar [exists ], <WORKSPACE>\lib\warts.jar [exists ], <HOME>\.sbt\boot\scala-2.12.18\lib\scala-library.jar [exists ]
Options:
-Yrangepos -Xplugin-require:semanticdb




#### Error stacktrace:

```
java.base/jdk.internal.util.Preconditions$1.apply(Preconditions.java:55)
	java.base/jdk.internal.util.Preconditions$1.apply(Preconditions.java:52)
	java.base/jdk.internal.util.Preconditions$4.apply(Preconditions.java:213)
	java.base/jdk.internal.util.Preconditions$4.apply(Preconditions.java:210)
	java.base/jdk.internal.util.Preconditions.outOfBounds(Preconditions.java:98)
	java.base/jdk.internal.util.Preconditions.outOfBoundsCheckFromIndexSize(Preconditions.java:118)
	java.base/jdk.internal.util.Preconditions.checkFromIndexSize(Preconditions.java:397)
	java.base/java.lang.String.checkBoundsOffCount(String.java:4853)
	java.base/java.lang.String.rangeCheck(String.java:307)
	java.base/java.lang.String.<init>(String.java:303)
	scala.tools.nsc.interactive.Global.typeCompletions$1(Global.scala:1231)
	scala.tools.nsc.interactive.Global.completionsAt(Global.scala:1254)
	scala.meta.internal.pc.SignatureHelpProvider.$anonfun$treeSymbol$1(SignatureHelpProvider.scala:453)
	scala.Option.map(Option.scala:230)
	scala.meta.internal.pc.SignatureHelpProvider.treeSymbol(SignatureHelpProvider.scala:451)
	scala.meta.internal.pc.SignatureHelpProvider$MethodCall$.unapply(SignatureHelpProvider.scala:246)
	scala.meta.internal.pc.SignatureHelpProvider$MethodCallTraverser.visit(SignatureHelpProvider.scala:357)
	scala.meta.internal.pc.SignatureHelpProvider$MethodCallTraverser.traverse(SignatureHelpProvider.scala:351)
	scala.meta.internal.pc.SignatureHelpProvider$MethodCallTraverser.fromTree(SignatureHelpProvider.scala:320)
	scala.meta.internal.pc.SignatureHelpProvider.$anonfun$signatureHelp$3(SignatureHelpProvider.scala:31)
	scala.Option.flatMap(Option.scala:271)
	scala.meta.internal.pc.SignatureHelpProvider.$anonfun$signatureHelp$2(SignatureHelpProvider.scala:29)
	scala.Option.flatMap(Option.scala:271)
	scala.meta.internal.pc.SignatureHelpProvider.signatureHelp(SignatureHelpProvider.scala:27)
	scala.meta.internal.pc.ScalaPresentationCompiler.$anonfun$signatureHelp$1(ScalaPresentationCompiler.scala:439)
	scala.meta.internal.pc.CompilerAccess.withSharedCompiler(CompilerAccess.scala:148)
	scala.meta.internal.pc.CompilerAccess.$anonfun$withNonInterruptableCompiler$1(CompilerAccess.scala:132)
	scala.meta.internal.pc.CompilerAccess.$anonfun$onCompilerJobQueue$1(CompilerAccess.scala:209)
	scala.meta.internal.pc.CompilerJobQueue$Job.run(CompilerJobQueue.scala:152)
	java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1144)
	java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:642)
	java.base/java.lang.Thread.run(Thread.java:1583)
```
#### Short summary: 

java.lang.StringIndexOutOfBoundsException: Range [5304, 5304 + -2) out of bounds for length 5556