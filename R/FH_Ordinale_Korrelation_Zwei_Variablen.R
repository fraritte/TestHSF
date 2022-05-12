#' Eine Funktion, die bestehende R-Pakete zusammenschnürt um das Ergebnis einer bivariaten Spearman-Korrelation auszugeben.
#' @param data als (data.frame), voreingestellt ist "dataset". Hier muss der Datensatz angegeben werden.
#' @param dv als Charakter-Vektor mit der Länge 1. Voreingestellt ist "AV". Hier muss eine numerische Variable (ordinal) angegeben werden.
#' @param iv als Charakter-Vektor mit der Länge 1. Voreingestellt ist "UV". Hier muss eine numerische Variable (ordinal) angegeben werden.
#' @return Es wird ein kurzer Ergebnistext ausgegeben.

#' @import dplyr
#' @import Hmisc

#' @export


FH_Ordinale_Korrelation_Zwei_Variablen <- function(data = dataset, dv=AV, iv= UV){



  # Hier nutze ich den cor.test-Befehl, um die Korrelation und das Signifikanzniveau zu errechnen. Damit das automatisch geht, wird der Befehl hier so aufwendig zusammengebunden.
  Korrelationsbefehl <- paste("cor.test(formula = ~" , noquote(iv),   "+" , noquote(dv) ,  ', data = data, method = "spearman", exact =FALSE)', sep ="")

  # Das Ergebnis von cor.test wird im Objekt "Korrelationsergebnis" gespeichert.
  Korrelationsergebnis <- eval(parse(text= Korrelationsbefehl))


  # das hier mache ich nur, um an die df zu kommen. die werden mir bei spearmann nicht angezeigt.
  Korrelationsbefehl_pearson <- paste("cor.test(formula = ~" , noquote(iv),   "+" , noquote(dv) ,  ', data = data, method = "pearson", exact =FALSE)', sep ="")
  Korrelationsergebnis_pearson <- eval(parse(text= Korrelationsbefehl_pearson))

  # Die relevanten Parameter der Korrelation werden im Ergebnistext zusammengefasst
  Ergebnistext <- paste(Korrelationsergebnis$method , " zwischen ", iv, " und " , dv, " betraegt r(", Korrelationsergebnis_pearson$parameter , ") = ",round(Korrelationsergebnis$estimate, digits = 3) , " , p = " , round(Korrelationsergebnis$p.value, digits = 2), ".",  sep="")



  return(Ergebnistext)


}
