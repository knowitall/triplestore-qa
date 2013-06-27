package edu.knowitall.triplestore


object Test extends Application {
  
  import Conditions._
  import Operators._
  import Search._
  import Field._
  
  def triple(n: String, x: String, r: String, y: String) = Tuple(Map("arg1" -> List(x), "rel" -> List(r), "arg2" -> List(y))).renamePrefix(n)
  
  val r = Stream(
      triple("r", "honey", "kills", "bacteria"),
      triple("r", "bleach", "kills", "bacteria"),
      triple("r", "poison", "does not kill", "bacteria")
  )
  val s = Stream(
      triple("s", "sweet honey", "is", "good"),
      triple("s", "bleach", "is", "good"),
      triple("s", "food", "is", "good")
  )
  
  val syns = Set(Set("honey", "sweet honey"))
  
  /*val select = Select(AttrEquals("r.rel", "kills"))
  val joinCond = AttrsEqual("r.arg1", "s.arg1")
  val join = NestedLoopJoin(joinCond)
  val project = Project(OnAttrs("r.arg1"))*/
  
  val url = "http://rv-n12:8983/solr/triplestore"
  val c = TriplestoreClient(url, 10)
  
  val taft = List("William Taft", "Taft", "William Howard Taft")
  val jfk = List("John F. Kennedy", "JFK", "Kennedy", "President Kennedy")
  val arlington = List("Arlington", "Arlington Cemetery", "Arlington National Cemetery")
  val buried = List("is buried in", "final resting place")
  val isa = List("is", "type")
  val president = List("US President", "president")
  

  val ExecQuery = c.namedSearch _
  val Join = NestedLoopJoin _
  val SynonymEquality = AttrsSyn _
  
  val q1 = Conjunction(FieldIn(rel, isa), FieldIn(arg2, president))
  val q2 = Conjunction(FieldIn(rel, buried), FieldIn(arg2, arlington))
  
  val rel1 = ExecQuery("r1", q1)
  val rel2 = ExecQuery("r2", q2)
  
  val cond = SynonymEquality("r1.arg1", "r2.arg1", List(taft, jfk))
  val join = Join(cond)
  
  val project = Project(On("r1.arg1"))
  
  
  type Strings = List[String]
  def ProjectOn(a: String, ts: Tuples) = Project(On(a))(ts)
  
  def JoinOn(p: TuplePred, ts1: Tuples, ts2: Tuples) = Join(p)(ts1, ts2)
  
  def SearchFor(s: String, q: Query*) = ExecQuery(s, Conjunction(q:_*))
  
  def Rel(ss: Strings) = FieldIn(rel, ss)
  
  def Arg2(ss: Strings) = FieldIn(arg2, ss)
  
  project(join(rel1, rel2))
  
  val synonyms = List(taft, jfk)
  
  ProjectOn("r1.arg1", 
      JoinOn(
          SynonymEquality("r1.arg1", "r2.arg2", synonyms), 
          SearchFor("r1", Rel(isa), Arg2(president)), 
          SearchFor("r2", Rel(buried), Arg2(arlington))
      )
  )

}