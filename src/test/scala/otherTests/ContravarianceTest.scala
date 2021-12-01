package otherTests

class Test {

  trait Animal

  class Cat extends Animal

  class Dog extends Animal


  abstract class AnimalPrinter[-A] {
    def print(cat: A): String = s"$cat is an animal."
  }

  val catPrinter = new AnimalPrinter[Cat]{}
  val dogPrinter = new AnimalPrinter[Dog]{}
  val animalPrinter = new AnimalPrinter[Animal]{}

  def printerToString(x : AnimalPrinter[Cat]): String = ???

  printerToString(catPrinter)
  printerToString(animalPrinter)


}
