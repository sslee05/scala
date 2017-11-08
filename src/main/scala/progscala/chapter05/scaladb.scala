package progscala.chapter05 {


  package databaseapi {
    case class InvalidColumnName(name: String) extends RuntimeException(s"INvalid column name $name")
    
    trait Row {
      def getInt(colName: String): Int
      def getDouble(colName: String): Double
      def getText(colName: String): String
    }
  }
  
  package javadb {
    import databaseapi._
    
    case class JRow(representation: Map[String,Any]) extends Row {
      private def get(colName: String): Any = 
        representation.getOrElse(colName, throw InvalidColumnName(colName))
        
        def getInt(colName: String): Int = get(colName).asInstanceOf[Int]
        def getDouble(colName: String): Double = get(colName).asInstanceOf[Double]
        def getText(colName: String): String = get(colName).asInstanceOf[String]
    }
    
    object JRow {
      def apply(pairs: (String,Any)*) = new JRow(Map(pairs: _*))
    }
  }
  
  package scaladb {
    object implicits {
      
      import javadb.JRow
      
      implicit class SRow(jRow: javadb.JRow) {
        def get[T](colName: String)(implicit toT: (JRow,String) => T): T = 
          toT(jRow,colName)
      }
      
      implicit val jrowToInt: (JRow,String) => Int = 
        (jRow: JRow, colName: String) => jRow.getInt(colName)
        
      implicit val jrowToDouble: (JRow,String) => Double =
        (jRow: JRow, colName: String) => jRow.getDouble(colName)
        
      implicit val jrowToString: (JRow, String) => String = 
        (jRow: JRow, colName: String) => jRow.getText(colName)
    }
    
}
  
  object DB {
      import progscala.chapter05.javadb.JRow
      import progscala.chapter05.scaladb.implicits._
      
      def main(args: Array[String]) = {
        val row = javadb.JRow("one" -> 1, "two" -> 2.2, "three" -> "THREE!")
        
        val oneValue1: Int = row get "one"
        val twoValue1: Double = row get "two"
        val threeValue1: String = row get "three"
        //val fourValue1: Byte = row get "four"
        
        println(s"one1   -> $oneValue1")
        println(s"two1   -> $twoValue1")
        println(s"three1 -> $threeValue1")
        
        val oneValue2 = row get[Int] "one"
        val twoValue2 = row get[Double] "two"
        val threeValue2 = row get[String] "three"
        //val fourValue2 = row get[Byte] "four"
        
        println(s"one2    -> $oneValue2")
        println(s"two2    -> $twoValue2")
        println(s"three2  -> $threeValue2")
        
        
      }
    }
  }