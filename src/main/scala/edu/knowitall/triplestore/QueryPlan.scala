package edu.knowitall.triplestore


object QueryPlan {
  
  case class Schema(names: List[String]) {
    def indexOf(name: String) = names.indexOf(name)
  }
  
  case class Tuple(schema: Schema, values: List[Any]) {
    if (schema.names.size != values.size) {
      throw new IllegalArgumentException("invalid tuple size: " + (schema, values))
    }
    def getValue(col: String): Any = values(schema.indexOf(col))
  }
  
  case class Relation(name: String, schema: Schema, tuples: Iterable[Tuple])
  
  def addPrefix(prefix: String, schema: Schema): Schema = {
    Schema(schema.names.map{n => prefix + "." + n})
  }
  
  def rename(schema: Schema, tuples: Iterable[Tuple]): Iterable[Tuple] = {
    tuples map { t => Tuple(schema, t.values)}
  }
  
  def product(r1: Relation, r2: Relation): Iterable[Tuple] = {
    val s1 = addPrefix(r1.name, r1.schema)
    val s2 = addPrefix(r2.name, r2.schema)
    val s = Schema(s1.names ++ s2.names)
    for (t1 <- r1.tuples; t2 <- r2.tuples) yield Tuple(s, t1.values ++ t2.values)
  }

  def select(tuples: Iterable[Tuple], c: Tuple => Boolean): Iterable[Tuple] = {
    tuples.filter(c)
  }
  
  def join(r1: Relation, r2: Relation, c: (Tuple, Tuple) => Boolean): Iterable[Tuple] = {
    val s1 = addPrefix(r1.name, r1.schema)
    val ts1 = rename(s1, r1.tuples)
    val s2 = addPrefix(r2.name, r2.schema)
    val ts2 = rename(s2, r2.tuples)
    val s = Schema(s1.names ++ s2.names)
    for (t1 <- ts1; t2 <- ts2; if c(t1, t2)) yield Tuple(s, t1.values ++ t2.values)
  }
 
}

object Examples {
  import QueryPlan._
  val s1 = Schema(List("name", "age"))
  val s2 = Schema(List("name", "pet"))
  
  val rel1 = Relation("people_ages", s1, List(
      Tuple(s1, List("joe", 25)),
      Tuple(s1, List("bob", 5)),
      Tuple(s1, List("chris", 29)),
      Tuple(s1, List("sally", 15))
  ))
  
  val rel2 = Relation("people_pets", s2, List(
      Tuple(s1, List("joe", "snake")),
      Tuple(s1, List("bob", "dog")),
      Tuple(s1, List("bob", "another dog")),
      Tuple(s1, List("sally", "lobster"))
  ))
  
  def valEq(col1: String, col2: String): (Tuple, Tuple) => Boolean = {
    (t1: Tuple, t2: Tuple) => {
      val v1 = t1.getValue(col1)
      val v2 = t2.getValue(col2)
      v1 == v2
    }
  }
  
}