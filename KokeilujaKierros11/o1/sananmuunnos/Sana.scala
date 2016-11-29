package o1.sananmuunnos
import scala.collection.mutable.Buffer

class Sana(val merkkijono: String) {
  
  /// nakyykotama
  
  /// to kjk
  
  ///nakyyko mita vittua
  
val Vokaalit = "\u0061\u0065\u0069\u006F\u0075\u0079\u00E4\u00F6\u00E5"

val Konsonantit = "\u0062\u0063\u0064\u0066\u0067\u0068\u006A\u006B\u006C\u006D\u006E\u0070\u0071\u0072\u0073\u0074\u0076\u0077\u0078\u007A"

val MuuttuvatTakavokaalit = "\u0061\u006F\u0075"

val MuuttuvatEtuvokaalit = "\u00E4\u00F6\u0079"

def onKirjain(symboli: Char) = onVokaali(symboli) || onKonsonantti(symboli)

def onVokaali(symboli: Char) = Vokaalit.contains(symboli.toLower)

def onKonsonantti(symboli: Char) = Konsonantit.contains(symboli.toLower)

override def toString  = {
  val uusiSana = Buffer[String]()
  var counter = 0
  for( kirjain <- merkkijono) {
    if(onVokaali(kirjain) && counter == 0){
      uusiSana += s"|$kirjain|"
      counter += 1
    }else{
      uusiSana += kirjain.toString()
    }
  }
  uusiSana.mkString("")
}
  
}


object Sana {

  def muunnos(ekaSana: String, tokaSana: String): String = {
    "kikkelikarva"
    
  }
  
}