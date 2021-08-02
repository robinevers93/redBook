package redBookTest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import redBook.Chapter3.{Branch, Cons, Leaf, List, Tree}

class Chapter3Test extends AnyFlatSpec with Matchers {

  "init" should "delete last element in list" in {
    val startList= List(1,2,3,4)
    val endList = List(1,2,3)
    List.init(startList) mustBe endList
  }

  //3.8 What happens when you pass Nil and Cons themselves to foldRight
  "foldRight" should "return original list when passing Nil as identity element and Cons as operation" in {
    val startList= List(1,2,3,4)
    val endList = List.foldRight(startList, List(): List[Int])(Cons(_,_))
    startList mustBe endList
  }

  //3.9 Compute the length of a list using foldRight
  "length" should "calculate the length of a list correctly" in {
    val startList= List(1,2,3,4)
    val length = List.length(startList)
    length mustBe 4
  }

  //3.10 Write a tail-recursive foldLeft function
  "foldLeft" should "be able to create sum function" in {
    def sum(l: List[Int]): Int =
      List.foldLeft(l, 0)((x,y) => x+y)

    sum(List(1,2,3)) mustBe 6
  }

  //3.11 Write a tail-recursive foldLeft function.
  "foldLeft implementations of sum, product and length" should "work" in {
    val list = List(1,2,3,4)
    val list2 = List(1.0,2.0,3.0,4.0)

    List.sum3(list) mustBe 10
    List.product3(list2) mustBe 24
    List.length2(list) mustBe 4
  }

  //3.12 Write a function that returns the reverse of a List using a fold
  "reverse" should "reverse list" in {
    val list = List(1,2,3,4)
    val reverseList = List(4,3,2,1)

    List.reverse(list) mustBe reverseList
  }

  //3.13 Write foldLeft in terms of foldRight and foldRight in terms of foldLeft
  "foldRight2" should "be able to create sum function" in {
    def sum(l: List[Int]): Int =
      List.foldRight2(l, 0)((x,y) => x+y)

    def sum2(l: List[Int]): Int =
      List.foldLeft2(l, 0)((x,y) => x+y)

    def reverse[A](l: List[A]): List[A] =
      List.foldLeft2(l, List(): List[A])((x, y) => Cons(y, x))

    sum(List(1,2,3)) mustBe 6 //doesn't test associativity or commutativity, but foldRight(List(1,2,3),0)(f) == 1+(2+(3+foldRight(Nil,0)(f))) == 1 + (2 + (3 + 0)) == foldLeft(Nil, 1+(2+(3+0)))(g) == foldLeft(List(3,2,1),0)(g) where g(x,y)=f(y,x)
    sum2(List(1,2,3)) mustBe 6 //doesn't test associativity or commutativity, but foldLight(List(1,2,3),0)(f) == foldLeft(Nil, (((0+1)+2)+3)(f) == ((0+1)+2)+3 == ((foldRight(Nil,0)(g)+1)+2)+3 == foldRight(List(3,2,1),0)(g) where g(x,y)=f(y,x)
    reverse(List(1,2,3)) mustBe List(3,2,1)
  }


  //3.14 Implement append in terms of either foldLeft or foldRight
  "append v2 and v3" should "append" in {
    val l1 = List(1,2,3)
    val l2 = List(4,5,6)
    val l3 = List.append2(l1, l2)
    val l4 = List.append3(l1, l2)
    val l5 = List(1,2,3,4,5,6)

    l3 mustBe l5
    l4 mustBe l5
  }

  //3.15 Write a function that concatenates a list of lists into a single list
  "concat" should "concatenate a list of lists into a single list" in {
    val l1 = List(1,2,3)
    val l2 = List(4,5,6)
    val l3 = List(7,8,9)
    val l4 = List(1,2,3,4,5,6,7,8,9)
    val lists = List(l1, l2, l3)
    val concatList = List.concat(lists)

    concatList mustBe l4
    //this basically does: append(list1, foldRight(...)) = append(list1, append( list2, foldRight(...))) etc.
  }

  //3.16
  "addOne" should "add 1 to each element in a list" in {
    val list = List(1, 2, 3, 4)
    val newList = List(2, 3, 4, 5)

    List.addOne(list) mustBe newList
  }

  //3.17
  "listToString" should "add turn Doubles in a list into String values" in {
    val list = List(0.1, 0.2, 0.3, 0.4)
    val newList = List("0.1", "0.2", "0.3", "0.4")

    List.listToString(list) mustBe newList
  }

  //3.18
  "map" should "function as map" in {
    val list = List(0.1, 0.2, 0.3, 0.4)
    val newList = List("0.1", "0.2", "0.3", "0.4")
    val f = (x:Double) => x.toString

    List.map(list)(f) mustBe newList

    val list2 = List(1, 2, 3, 4)
    val newList2 = List(2, 3, 4, 5)
    val g = (x:Int) => x+1

    List.map(list2)(g) mustBe newList2
  }

  //3.19
  "filter" should "function as filter" in {
    val list = List(0.1, 0.2, 0.3, 0.4)
    val newList = List(0.3, 0.4)
    val f: Double => Boolean = (x: Double) => x > 0.2

    List.filter(list)(f) mustBe newList

    val list2 = List(1, 2, 3, 4)
    val newList2 = List(1, 2, 4)
    val g: Int => Boolean = (x: Int) => x != 3

    List.filter(list2)(g) mustBe newList2
  }

  //3.20
  "flatmap" should "function as flatmap" in {
    val list = List(1,2,3)
    val newList = List(1, 1, 2, 2, 3, 3)
    val f: Int => List[Int] = (x: Int) => List(x, x)

    List.flatMap(list)(f) mustBe newList
  }

  //3.21
  "filter2" should "function as filter" in {
    val list = List(0.1, 0.2, 0.3, 0.4)
    val newList = List(0.3, 0.4)
    val f: Double => Boolean = (x: Double) => x > 0.2

    List.filter2(list)(f) mustBe newList

    val list2 = List(1, 2, 3, 4)
    val newList2 = List(1, 2, 4)
    val g: Int => Boolean = (x: Int) => x != 3

    List.filter2(list2)(g) mustBe newList2
  }

  //3.22
  "addList" should "add entries of lists together" in {
    val list = List(1,2,3)
    val list2 = List(4,5,6)
    val newList = List(5,7,9)

    List.addLists(list, list2) mustBe newList
  }

  //3.23
  "zipWith" should "add entries of lists together" in {
    val list = List(1,2,3)
    val list2 = List(4,5,6)
    val f = (x: Int, y: Int) => x+y
    val newList = List(5,7,9)

    List.zipWith(list, list2)(f) mustBe newList
  }

  //3.24
  "hasSubsequence" should "returns true if and only if a subsequence is found in a list" in {
    val list = List(1,2,3)
    val subList1 = List(1,2)
    val subList2 = List(3)
    val subList3 = List()
    val notSubList = List(3,2)

    List.hasSubsequence(list, subList1) mustBe true
    List.hasSubsequence(list, subList2) mustBe true
    List.hasSubsequence(list, subList3) mustBe true
    List.hasSubsequence(list, notSubList) mustBe false
  }

  //3.25
  "size" should "correctly count number of nodes in a tree" in {
    val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.size(tree) mustBe 5
  }

  //3.26
  "maximum" should "correctly find the maximum of nodes in a tree" in {
    val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.maximum(tree) mustBe 3
    val tree2: Tree[Int] = Branch(Branch(Leaf(10), Leaf(2)), Leaf(3))
    Tree.maximum(tree2) mustBe 10
  }

  //3.27
  "depth" should "correctly return the depth of a tree" in {
    val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.depth(tree) mustBe 2
    val tree2: Tree[Int] = Branch(Branch(Leaf(10), Branch(Leaf(1), Branch(Leaf(1), Leaf(2)))), Leaf(3))
    Tree.depth(tree2) mustBe 4
  }

  //3.28
  "map" should "work correctly on a tree" in {
    val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val f = (x:Int) => x+3
    val treeMapped: Tree[Int] = Branch(Branch(Leaf(4), Leaf(5)), Leaf(6))

    Tree.map(tree)(f) mustBe treeMapped
  }

  //3.29
  "fold" should "work correctly on a tree" in {
    val tree1: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.sizeFromFold(tree1) mustBe 5

    val tree2: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.maxFromFold(tree2) mustBe 3
    val tree3: Tree[Int] = Branch(Branch(Leaf(10), Leaf(2)), Leaf(3))
    Tree.maxFromFold(tree3) mustBe 10

    val tree4: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.depthFromFold(tree4) mustBe 2
    val tree5: Tree[Int] = Branch(Branch(Leaf(10), Branch(Leaf(1), Branch(Leaf(1), Leaf(2)))), Leaf(3))
    Tree.depthFromFold(tree5) mustBe 4

    val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val f = (x:Int) => x+3
    val treeMapped: Tree[Int] = Branch(Branch(Leaf(4), Leaf(5)), Leaf(6))

    Tree.mapFromFold(tree)(f) mustBe treeMapped
  }

}
