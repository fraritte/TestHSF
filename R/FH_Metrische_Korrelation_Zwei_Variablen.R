#' Eine Funktion, die bestehende R-Pakete zusammenschnürt um das Ergebnis einer bivariaten Pearson-Korrelation auszugeben.
#' @param data als (data.frame), voreingestellt ist "dataset". Hier muss der Datensatz angegeben werden.
#' @param dv als Charakter-Vektor mit der Länge 1. Voreingestellt ist "AV". Hier muss eine numerische Variable (metrisch) angegeben werden.
#' @param iv als Charakter-Vektor mit der Länge 1. Voreingestellt ist "UV". Hier muss eine numerische Variable (metrisch) angegeben werden.
#' @return Es wird ein kurzer Ergebnistext ausgegeben.

#' @import dplyr
#' @import Hmisc

#' @export


FH_Metrische_Korrelation_Zwei_Variablen <- function(data = dataset, dv=AV, iv= UV){



  # Hier nutze ich den cor.test-Befehl, um die Korrelation und das Signifikanzniveau zu errechnen. Damit das automatisch geht, wird der Befehl hier so aufwendig zusammengebunden.
  Korrelationsbefehl <- paste("cor.test(formula = ~" , noquote(iv),   "+" , noquote(dv) ,  ', data = data, method = "pearson")', sep ="")

  # Das Ergebnis von cor.test wird im Objekt "Korrelationsergebnis" gespeichert.
  Korrelationsergebnis <- eval(parse(text= Korrelationsbefehl))

  # Die relevanten Parameter der Korrelation werden im Ergebnistext zusammengefasst
  Ergebnistext <- paste(Korrelationsergebnis$method , " zwischen ", iv, " und " , dv, " betraegt r(", Korrelationsergebnis$parameter , ") = ",round(Korrelationsergebnis$estimate, digits = 3) , " , p = " , round(Korrelationsergebnis$p.value, digits = 2), ".",  sep="")



  return(Ergebnistext)


}
