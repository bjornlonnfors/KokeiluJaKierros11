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

def onMuuttuvaEtuvokaali(symboli: Char) = MuuttuvatEtuvokaalit.contains(symboli.toLower)

def onMuuttuvaTakavokaali(symboli: Char) = MuuttuvatTakavokaalit.contains(symboli.toLower)

def onMuuttuvaVokaali(symboli: Char) = onMuuttuvaTakavokaali(symboli) || onMuuttuvaEtuvokaali(symboli)

def taakse(symboli: Char) = if (onMuuttuvaEtuvokaali(symboli)) MuuttuvatTakavokaalit(MuuttuvatEtuvokaalit.indexOf(symboli)) else symboli

def eteen(symboli: Char) = if (onMuuttuvaTakavokaali(symboli)) MuuttuvatEtuvokaalit(MuuttuvatTakavokaalit.indexOf(symboli)) else symboli

override def toString  = {
  /*val uusiSana = Buffer[String]()
  var counter = 0
  for( kirjain <- merkkijono) {
    if(onVokaali(kirjain) && counter == 0 ){
      uusiSana += s"|$kirjain|"
      counter += 1
    }else{
      uusiSana += kirjain.toString()
    }
  }
  * 
  */
  val uusiSana = merkkijono.split("").toBuffer
  val numero = ekaVokaali(merkkijono)
  if(this.kategoria(this)){
    uusiSana.insert(numero,"|")
    uusiSana.insert(numero + 3,"|")
    uusiSana.mkString("")
  }else{
    uusiSana.insert(numero,"|")
    uusiSana.insert(numero + 2,"|")
    uusiSana.mkString("")
    
  }
  
}

   def kategoria(sana: Sana): Boolean = sana.merkkijono(ekaVokaali(sana.merkkijono)) == sana.merkkijono(ekaVokaali(sana.merkkijono) + 1)
  


  private def muunnos(tokaSana: Sana) = {
    val ensimmainenNumero = ekaVokaali(tokaSana.merkkijono)
    val toinenNumero = ekaVokaali(this.merkkijono)
    
    
    val ekaEka = tokaSana.merkkijono.take(ensimmainenNumero + 1 ).toLowerCase()
    val ekaToka = this.merkkijono.drop(toinenNumero + 1)
    val tokaEka =  this.merkkijono.take(toinenNumero + 1).toLowerCase()
    val tokaToka = tokaSana.merkkijono.drop(ensimmainenNumero + 1)
    


    
   val melkeinValmisEka = {
      val loppu = ekaEka.split("").toBuffer
      if(ekaEka.exists(onMuuttuvaTakavokaali(_))){
       for(kirjain <- (ekaToka)){
         loppu += taakse(kirjain).toString
       }
       loppu.mkString("")
    }else if(ekaEka.exists(onMuuttuvaEtuvokaali(_))) {
      for(kirjain <- (ekaToka)){
        loppu += eteen(kirjain).toString
      }
      loppu.mkString("")
    }else{
      ekaEka + ekaToka
    }
    }
      
      
      val valiAikainenToka = {
        val loppu = tokaEka.split("").toBuffer
        if(tokaEka.exists(onMuuttuvaTakavokaali(_))){
          for(kirjain <- (tokaToka)){
            loppu += taakse(kirjain).toString
          }
          loppu.mkString("")
        
      }else if(tokaEka.exists(onMuuttuvaEtuvokaali(_))) {
        for(kirjain <- (tokaToka)) {
          loppu += eteen(kirjain).toString
        }
        loppu.mkString("")
      }else {
        tokaEka + tokaToka
      }
      
     
    
      
      
    }
    
      
    val melkeinValmisToka = valiAikainenToka
    
    melkeinValmisEka + " " + melkeinValmisToka
   val lopullinenEka = {
      if(this.kategoria(this)){
    
     val valiaikainen = melkeinValmisEka.split("")
     valiaikainen(ensimmainenNumero + 1) = valiaikainen(ensimmainenNumero)
     valiaikainen.mkString("")
    } else
      melkeinValmisEka
    }
    
    val lopullinenToka  = {
      if(tokaSana.kategoria(tokaSana)){
        val valiaikainen = melkeinValmisToka.split("")
        valiaikainen(toinenNumero + 1) = valiaikainen(toinenNumero )
        valiaikainen.mkString("")
      }else
        melkeinValmisToka
    }
    
    lopullinenEka + " " +  lopullinenToka
    
    
        
    
  }
  
  private def ekaVokaali(sana: String): Int = {
  val vokaali = Buffer[Char]()
  
  for(kirjain <- sana){
    if(vokaali.length == 0 && onVokaali(kirjain)){
      vokaali += kirjain
    }
  }
  
 val ekaNumero =  sana.indexOf(vokaali.mkString(""))
 ekaNumero
 
  /* if(sana(ekaNumero + 1) == sana(ekaNumero)){
     ekaNumero + 1
   }else{
     ekaNumero
   }
   * 
   */
  
  }
  
}


object Sana {

  def muunnos(ekaSana: String, tokaSana: String): String = {
    
    new Sana(ekaSana).muunnos(new Sana(tokaSana))
    
  }
    
}  
