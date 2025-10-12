enum Tree:
  case Leaf(value: Int)
  case Branch(left: Tree, value: Int, right: Tree)


def sum(t: Tree): Int = t match
  case Leaf(n)          => n
  case Branch(l, n, r)  => sum(l) + n + sum(r)


def isEvenBranch(t: Tree): Boolean = t match
  case Branch(_, n, _) if n % 2 == 0 => true
  case _ => false


