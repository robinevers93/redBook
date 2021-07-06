package redBookTest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import redBook.{Cons, List}

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

}
