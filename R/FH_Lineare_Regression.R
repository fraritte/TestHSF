### Ich sollte das "absolute R^2" rausnehmen oder anders beschreiben, denn das scheint nur Näherungsweise zu klappen!!!!



#' Eine Funktion, die bestehende R-Pakete zusammenschnürt um das Ergebnis einer multiplen Regression in Tabellenform als Word-Datei auszugeben. Zusätzlich wird auch noch eine Interkorrelationstabelle ausgegeben.
#' @param data als (data.frame), voreingestellt ist "dataset". Hier muss der Datensatz angegeben werden.
#' @param dv als Charakter-Vektor mit der Länge 1. Voreingestellt ist "AV". Hier muss die AV angegeben werden.
#' @param iv als Charakter-Vektor mit der Länge > 1. Voreingestellt ist "UV".Hier müssen die UVs angegeben werden.
#' @param tabnumber als Vektor mit der Länge 1 oder als Zahl. Voreingestellt ist "Tabellennummer". Das ist die Tabellennummer, die dann in der Tabellenueberschrift der Inter-Korrelationstabelle steht. Die Tabellennummer der Regressionstabelle ist "tabnumber" + 1-
#' @return Hauptsaechlich werden verschiedene Word-Dateien erstellt. Die Word-Datei "Korrelationstabelle" beinhaltet die Interkorrelationsmatrix, die Word-Datei "Word_Tabelle_multiple_Regression" beinhaltet die Ergebnisse der Regression.
#' @import dplyr
#' @import knitr
#' @import kableExtra
#' @import expss
#' @import apaTables
#' @import jtools
#' @import lm.beta
#' @import r2rtf
#' @import aod
#' @import xtable
#' @export

FH_Lineare_Regression <- function(data = dataset, dv=AV, iv=UV, tabnumber = Tabellennummer){

  options(knitr.kable.NA = '')

  # Name der Word-Datei, die die Korrelationstabelle enthaelt
  Tabellenname_Korrelationstabelle <- paste("Korrelationstabelle_", tabnumber, "_APA.doc", sep="")

  # Name der Word-Datei, die die Korrelationstabelle enthaelt
  Tabellenname_Regressionstabelle <- paste("Alternative_Darstellung_Regressionstabelle_", tabnumber, "_APA.doc", sep="")



  # Hier bilde ich einen neuen Vektor, der die UVs hat, aber dazwischen immer ein "+", damit ich das dann in den Modellbefehl einbauen kann
  UV_plus_Zwischenschritt <- paste(iv[1:length(iv)-1], "+")
  UV_plus_Zwischenschritt_2 <- c(UV_plus_Zwischenschritt, iv[length(iv)])
  UV_plus <- paste(UV_plus_Zwischenschritt_2, collapse = " ")

  Variablen_multiple_Regression_0 <- vctrs::vec_c(dv, iv) # Hier bilde ich einen neuen Vektor, in dem erst die AV, dann die UV steht
  Variablen_multiple_Regression <- grep(paste0(":", collapse = "|"),Variablen_multiple_Regression_0, invert = TRUE, value =  TRUE ) # Das hier ist wichtig, damit er aus dem Variablenvektor die ":" raushaut, also die Moderatoren


  subdataset <- as.data.frame(subset(data, select = Variablen_multiple_Regression)) # Alle AV und UV werden jetzt in einem Subdatensatz gespeichert

  ### Vorab: Interkorrelationstabelle

  # Hier wird eine APA-konforme Korrelationstabelle gebildet und als .doc-Datei (Word) gespeichert.
  # Auch wenn Sie eine multiple Regression machen, muessen Sie erst einmal die bivariaten Korrelationen
  # der verschiedenen UVs mit der AV berichten, daher diese Korrelationstabelle.
  apa.cor.table(subdataset, filename=Tabellenname_Korrelationstabelle, table.number=tabnumber)

  Hinweis_Korrelationstabelle <- paste("Die Korrelationstabelle finden Sie unter dem Namen ", Tabellenname_Korrelationstabelle , " in Ihrem Working-Directory, Auch wenn Sie eine multiple Regression machen, muessen Sie erst einmal die bivariaten Korrelationen der verschiedenen UVs mit der AV berichten, daher diese Korrelationstabelle.")



  ### Beginn multipler Regression

  # Hier wieder der Befehl der multiplen Regression, der ist hier nur zu kompliziert, damit das alles automatisiert ablaufen kann.
  Modellbefehl <- paste("lm(", noquote(Variablen_multiple_Regression[1]),   "~" , noquote(UV_plus) ,  ", data= subdataset)", sep ="")

  # Hier wird das Modell der logitischen Regression geschaetzt.
  Modell<- eval(parse(text= Modellbefehl))


  Korrelation__Hilfe <- apa.reg.table(Modell, filename = Tabellenname_Regressionstabelle, table.number = (tabnumber + 1))
  Korrelation__Hilfe<-Korrelation__Hilfe$table_block_results
  Korrelation__Hilfe <- Korrelation__Hilfe[[1]][c("model_details_extended")]
  Korrelation_r <- Korrelation__Hilfe$model_details_extended$r



  # Hier wird jetzt eine Tabelle gebaut. Dafuer werden jede Menge Variablen erstellt, in denen bestimmte sachen drinstehen

  Betas <- lm.beta(Modell)
  Betas <- Betas$standardized.coefficients
  Ergebnistabelle <- summ(Modell, part.corr = TRUE)
  R_Quadrat <- attr(Ergebnistabelle, "rsq")
  ZFreiheitsgrade <- attr(Ergebnistabelle, "fnum")
  NFreiheitsgrade <- attr(Ergebnistabelle, "fden")
  FWert <- attr(Ergebnistabelle, "fstat")
  FWErt_sig <- attr(Ergebnistabelle, "modpval")

  Ergebnistabelle <- (Ergebnistabelle$coeftable)
  Ergebnistabelle <- as.data.frame(Ergebnistabelle)
  Ergebnistabelle$beta <- Betas
  Ergebnistabelle$Korrelation <- Korrelation_r
  Ergebnistabelle$Effektanteil_inkrementell <- Ergebnistabelle$part.r^2
  Ergebnistabelle$Effektanteil_absolut <- abs(Ergebnistabelle$beta * Ergebnistabelle$Korrelation)


  Ergebnistabelle_kurz <- subset(Ergebnistabelle, select = c("Est.", "S.E.", "beta", "t val.", "p", "Effektanteil_absolut", "Effektanteil_inkrementell"))

  colnames(Ergebnistabelle_kurz) <- c("Regressionskoeffizient B", "Std.-Fehler von B", "standardisiertes Beta", "t-Wert", "p-Wert", "absoluter Anteil am R-Quadrat", "inkrementelles R-Quadrat")


  Tabellenueberschrift <- paste("Tabelle ", (tabnumber+1), " : lineares Regressionsmodell der AV "  , dv  , ".", sep="")
  Tabellentext <- paste("Die Signifikanz des Regressionsmodells als Ganzes ist F(", ZFreiheitsgrade, ", " , NFreiheitsgrade, ") = "  , round(FWert, digits = 2)  , ", p = ",  round(FWErt_sig, digits = 3) , ". Das R-Quadrat betraegt " ,    round(R_Quadrat, digits = 3), ". Der absolute Anteil am R-Quadrat gibt an, wie viel Varianz der AV durch den jeweiligen Praediktor aufgeklaert wird. Die Summe der absoluten Anteile ergibt (abgesehen von Rundungsfehlern) ", round(R_Quadrat, digits = 3), ". Das inkrementelle R-Quadrat gibt an, wie viel Varianz inkrementell nur durch den jeweiligen Praediktor erklaert werden kann."       , sep="")


  result <- Ergebnistabelle_kurz %>%
    kbl(align = "c", digits = 2, caption = Tabellenueberschrift) %>%
    kable_classic_2(full_width = F, html_font = "Cambria" ) %>%
    footnote(general = Tabellentext
    )



  # Hier wird jetzt noch eine Tabelle erzeugt, die direkt als Word-Dokument vorliegt.


  # Hier fuege ich jetzt die Zeilennamen, in denen die Variablennamen drinstehen, noch als extra Spalte hinzu.
  Ergebnistabelle_kurz$Praediktor <- as.character(row.names(Ergebnistabelle_kurz))
  # Jetzt kommt die letzte Spalte noch nach ganz vorne
  Ergebnistabelle_kurz <- Ergebnistabelle_kurz %>% select(Praediktor, everything())


  # Hier weren jetzt noch Tabellenueberschrift, Tabellentext, und die Spaltennamen fuer die Word-Tabelle erzeugt.
  Spaltennamen_Word  <- c("Praediktor", "Regressionskoeffizient_B", "SE_B", "Standardisiertes_Beta", "t-Wert", "p-Wert", "R-Quadrat_absolut", "R-Quadrat_inkrementell")
  Spaltennamen_Word_Zwischenschritt <- paste(Spaltennamen_Word[1:length(Spaltennamen_Word)-1], "|")
  Spaltennamen_Word_Zwischenschritt_2 <- c(Spaltennamen_Word_Zwischenschritt, Spaltennamen_Word [length(Spaltennamen_Word)], collapse = " ")
  Spaltennamen_Word_final <- paste(Spaltennamen_Word_Zwischenschritt_2, collapse = " ")

  # Hier brauche ich einen Vektor, der Anzeigt, wie viele Spalten meine letztendliche Tabelle hat.
  Tabellen_Spaltenzahl<- c(rep(1,length(Spaltennamen_Word)))

  # Hier muss ich jetzt festlegen, wie viel breiter die erste Spalte sein soll. ICh sage mal 3x so breit
  Tabellen_Spaltenzahl[1] <- 3


  # Hier runde ich jetzt alle Spalten, die numerisch sind.
  Ergebnistabelle_kurz <- Ergebnistabelle_kurz %>% mutate_if(is.numeric, round, digits=3)
  # Hier konvertiere ich alle Spalten in Chars, das gibt weniger SChwierigkeiten beim Export.
  Ergebnistabelle_kurz <- Ergebnistabelle_kurz %>% mutate_if(is.numeric,as.character)


  # Hier wird die Post-Hoc-Tabelle jetzt in Word gebaut und gespeichert.
  Ergebnistabelle_kurz %>%
    rtf_title(title = Tabellenueberschrift, text_format = c("i"), text_justification = c("l"))%>%
    rtf_footnote(cell_justification = c("l"),footnote = Tabellentext, text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", text_justification = c("l")) %>%
    rtf_body(cell_justification = c("l"), as_colheader = FALSE,  col_rel_width = Tabellen_Spaltenzahl,  text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", border_last = "single", border_first = "single") %>%
    rtf_colheader(cell_justification = c("l"), col_rel_width = Tabellen_Spaltenzahl, colheader = Spaltennamen_Word_final , text_font_size = 12 , border_bottom = "single", border_left = "", border_right = "", border_top = "", ) %>%
    rtf_page(orientation = "landscape", border_last = "", border_first = "single") %>%
    rtf_encode() %>%
    write_rtf("Word_Tabelle_multiple_Regression.rtf")


  Hinweis_Regressiontabelle <- paste('Die Tabelle mit den Ergebnissen der multiplen Regression finden Sie unter dem Namen Word_Tabelle_multiple_Regression.rtf in Ihrem Working-Directory. Diese können Sie in Word uebernehmen und dort ggf. anpassen. In Ihrem Working Directory wird auch eine Datei mit dem Namen' ,Tabellenname_Regressionstabelle, '. Diese Datei wird automatisch miterzeugt und beinhaltet eine alternative Darstellung der multiplen Regression. Ich wuerde Ihnen aber empfehlen, die Datei Word_Tabelle_multiple_Regression.rtf zu verwenden und NICHT ', Tabellenname_Regressionstabelle, '.' )


  #print(Hinweis_Korrelationstabelle)
  #print(Hinweis_Regressiontabelle)
  #return(result)
  return(list(Hinweis_Korrelationstabelle, Hinweis_Regressiontabelle, result, Ergebnistabelle_kurz))


}


