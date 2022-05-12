#' Eine Funktion, die bestehende R-Pakete zusammenschnürt um als Ergebnis eine Inter-Korrelations-Matrix auszugeben. ACHTUNG: Alle Korrelationen sind Pearson-Korrelationen.
#' @param data als (data.frame), voreingestellt ist "dataset". Hier muss der Datensatz angegeben werden.
#' @param variables als Charakter-Vektor mit der Länge X. Voreingestellt ist "Variablen_Korrelation". Hier müssen numerische Variablen (metrisch) angegeben werden.
#' @param tabnumber als  Zahl. Voreingestellt ist "Tabellennummer". Das ist die Tabellennummer, die dann in der Tabellenueberschrift der Inter-Korrelations-Matrix steht.
#' @return Es wird ein eine Inter-Korrelationsmatrix als Word-Datei mit dem Namen "Korrelationstabelle_tabnumber_APA ausgeben. Zusätzlich wird noch eine Grafik ausgegeben, diese ist aber nicht APA-konform.

#' @import dplyr
#' @import Hmisc
#' @import apaTables
#' @import PerformanceAnalytics

#' @export


FH_Korrelationsmatrix <- function(data = dataset, variables=Variablen_Korrelation, tabnumber= Tabellennummer){


  subdataset <- subset(data, select = variables)
  # Hier wird wieder ein Subdatensatz gebildet mit den Variablen, die miteinander korreliert werden sollen.

  # Name der Word-Datei, die die Korrelationstabelle enthaelt
  Tabellenname <- paste("Korrelationstabelle_", tabnumber, "_APA.doc", sep="")

  Hinweis_Korrelationstabelle <- paste("Die Korrelationstabelle finden Sie unter dem Namen ", Tabellenname , "in Ihrem Working-Directory, Beachten Sie: Die Korrelationstabelle zeigt ausschliesslich Pearson-Korrelationen.")

  chart_correlation_Befehl <- paste('chart.Correlation(subdataset, histogram=TRUE, pch=19, method ="pearson")', sep ="")

  apa.cor.table(subdataset, filename=Tabellenname, table.number=tabnumber)

  Plot <- eval(parse(text= chart_correlation_Befehl)) # Erzeugt eine Grafik mit Korrelation, Historgramm, Scatterplott

  return(list(Plot, Hinweis_Korrelationstabelle))



}
