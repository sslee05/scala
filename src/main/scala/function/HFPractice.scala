package function


case class Person(name:String,isMale:Boolean,children:Person*)
case class Book(title:String,authors:String*)

object HFPractice {
  
  val ho = Person("hhlee",true)
	val ha = Person("hylee",false)
	val park = Person("yjpark",false,ho,ha)
	val lee = Person("sslee",true,ho,ha)

	val persons = List(ho,ha,park,lee)

	val books:List[Book] = 
		List(
			Book("Structure and Interpretation of ComputerPrograms","Abelson, harold","Sussman, Gerald J."),
			Book("Principles of Compiler Design","Aho, Alfred","Ullman, Jeffrey"),
			Book("Programming in Modula-2","Wirth, Niklaus"),
			Book("Elements Of ML Programming","Ullman, Jeffrey"),
			Book("The Java Launguage Specification","Gosling,James","Joy,Bill","Steele, Guy","Bracha, Gilad")
		)
		
	def test01 = {
    var results = persons withFilter(person => !person.isMale) flatMap(person => person.children 
        map(child => (person.name,child.name)))
        
    println(results)
  }
  
  def test02 = {
    var result = for {
      person <- persons withFilter(person => !person.isMale)
      child  <- person.children
    } yield(person.name,child.name)
    
    println(result)
  }
  
  def test03 = {
    val results = books flatMap(book => book.authors withFilter(author => author startsWith "Gosling")
      map(author => (book.title,author)))
    
    println(results)
    
  }
  
  def test04 = {
    val results = for {
      book <- books
      author <- book.authors withFilter(author => author startsWith "Gosling")
    }yield(book.title,author)
    
    println(results)
  }
  
  def test05 = {
    val result = books flatMap(book => books withFilter(book2 => book2 != book) flatMap(book2 => 
      book2.authors flatMap(author2 => book.authors withFilter(author => author == author2) map(author => (book.title,author)))))
    
    println(result)
  }
  
  def test06 = {
    var result = for {
      book <- books
      book2 <- books withFilter(book2 => book != book2)
      author2 <- book2.authors
      author <- book.authors withFilter(author => author == author2)
    } yield(book.title,author)
    
    println(result)
  }
  
}

object HFPracticeApp extends App {
  HFPractice.test06
}