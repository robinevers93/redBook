case class Person(id: Int, name: String, email: Option[String], age: Int)

def fetchById(id: Int): Option[Person] = {
  if (id == 1) Some(Person(1, "Robin", None, 26))
  else None
}

def generateEmailAccount(person: Person) = {
  Person(person.id, person.name, Some(person.name + "@newbould.uk"), person.age)
}

def createPerson(id: Int, age: Int): Person = {
  generateEmailAccount(Person(id, "person"+id.toString, None, age))
}

def validate(person: Person): Option[Person] = {
  if (person.name.isInstanceOf[String]) Some(person)
  else None
}

def insertIntoDb(person: Person): Person ={
  println("inserted " + person.name + "into DB")
  person
}

def persist(person: Option[Person], id: Int): Option[Person] = {
  val p = person.getOrElse(createPerson(id, 20))
  validate(p).map(insertIntoDb)
}

val person1: Option[Person] = fetchById(1)
val person2: Option[Person] = fetchById(2)

val personWithAccount1 = person1.filter(_.age > 18).map(generateEmailAccount)
val personWithAccount2 = person2.map(generateEmailAccount)

persist(personWithAccount1, 1)
persist(personWithAccount2, 2)

val a: List[String] = List("hello", "i", "am", "me")
println(s"this is a list: $a")