#' Eine Funktion, die bestehende R-Pakete zusammenschnürt um das Ergebnis einer logistischen Regression (einfach oder multiple) in Tabellenform als Word-Datei auszugeben.
#' @param data als (data.frame), voreingestellt ist "dataset". Hier muss der Datensatz angegeben werden.
#' @param dv als Charakter-Vektor mit der Länge 1. Voreingestellt ist "AV". Hier muss die AV angegeben werden.
#' @param iv als Charakter-Vektor mit der Länge >= 1. Voreingestellt ist "UV".Hier muss die UV (einfache logistische Regression), oder die UVs (multiple logistische Regression) angegeben werden.
#' @param tabnumber als Vektor mit der Länge 1 oder als Zahl. Voreingestellt ist "Tabellennummer". Das ist die Tabellennummer, die dann in der Tabellenueberschrift der Regressionstabelle steht.
#' @return Hauptsaechlich wird eine Word-Datei erstellt. Die Word-Datei "Word_Tabelle_logistische_Regression" beinhaltet die Ergebnisse der Regression.
#' @import dplyr
#' @import knitr
#' @import kableExtra
#' @import expss
#' @import apaTables
#' @import r2rtf
#' @import aod
#' @import xtable
#' @export

FH_Logistische_Regression <- function(data = dataset, dv=AV, iv=UV, tabnumber = 1){



  # Hier bilde ich einen neuen Vektor, der die UVs hat, aber dazwischen immer ein "+", damit ich das dann in den Modellbefehl einbauen kann

  UV_plus_Zwischenschritt <- paste(iv[1:length(iv)-1], "+")
  UV_plus_Zwischenschritt_2 <- c(UV_plus_Zwischenschritt, iv[length(iv)])
  UV_plus <- paste(UV_plus_Zwischenschritt_2, collapse = " ")

  Variablen_logistische_Regression_0 <- vctrs::vec_c (dv, iv) # Hier bilde ich einen neuen Vektor, in dem erst die AV, dann die UV steht

  Variablen_logistische_Regression <- grep(paste0(":", collapse = "|"),Variablen_logistische_Regression_0, invert = TRUE, value =  TRUE ) # Das hier ist wichtig, damit er aus dem Variablenvektor die ":" raushaut, also die Moderatoren

  subdataset <- as.data.frame(subset(data, select = Variablen_logistische_Regression)) # Alle AV und UV werden jetzt in einem Subdatensatz gespeichert

  # Hier wieder der Befehl der logistischen Regression, der ist hier nur zu kompliziert, damit das alles automatisiert ablaufen kann.
  Modellbefehl <- paste("glm(", noquote(Variablen_logistische_Regression[1]),   "~" , noquote(UV_plus) ,  ", data= data, family = binomial)", sep ="")

  # Hier wird das Modell der logitischen Regression geschaetzt.
  Modell<- eval(parse(text= Modellbefehl))


  Modellzusammenfassung <-summary(Modell) # Einige wichtige Modellkennwerte



  ## odds ratios and 95% CI --> dDas ist der Odds-Ratio, also das Exp(b) aus SPSS, das wollen wir haben und die dazugehoerigen Konfidenzintervalle
  Odds_Ratios <- as.data.frame(exp(cbind(Odds_Ratio = coef(Modell), confint(Modell))))



  Chi_Quadrat_Wert <-with(Modell, null.deviance - deviance) # Das ist der Chi-Quadrat-Wert
  Freihietsgrade <- with(Modell, df.null - df.residual) # Das sind die Freiheitsgrade
  Sig_Chi_Quadrat <- with(Modell, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) # Das ist die Signifikanz des Chi-Quadrt-Wertes
  Likelihood_Ratio_Test <- logLik(Modell) # das ist der likelihood-ratio-Test. the deviance residual is -2*log likelihood --> Das ergebnis hier *2 ist das Ergebnis aus SPSS





 # Diese Zeile macht Probleme!!! aber warum?

  #Pseudo_R_Quadrat <- rcompanion::nagelkerke(fit=Modell, null = NULL, restrictNobs = FALSE) # Hiermit bekomme ich Pseudo-R-Quadrate ausgegeben
  #Nagelkerke_Wert <- as.data.frame(Pseudo_R_Quadrat$Pseudo.R.squared.for.model.vs.null)

      # Das hier sollte das ausgleichen, funktioniert aber auch nicht mehr. diese Pakete...
  #Pseudo  <-rms::lrm(Modell)
  #Nagelkere_Zwischenschritt <- Pseudo[[3]]

 # Nagelkerke_Wert <- Nagelkere_Zwischenschritt[10]


  # Hier wird Nagelkerke dann eben "per Hand" berechnet...
  n <- nrow(Modell$model)
  Nagelkerke_Wert <- (1 - exp((Modell$dev - Modell$null)/n))/(1 - exp(-Modell$null/n))



  # Das hier erzeugt eine Klassifikationstabelle, mit der ich sehen kann, wie viele ich jetzt zuordnen kann.

  my_Steps_Befehl <- paste("step(Modell,", noquote(Variablen_logistische_Regression[1]),   "~" , noquote(UV_plus)  ,  ", data= data, family = binomial)", sep ="")

  # dieser Teil ist ?bernommen von: https://stackoverflow.com/questions/13661025/classification-table-for-logistic-regression-in-r
  mysteps <- eval(parse(text= my_Steps_Befehl))
  theProbs <- fitted(mysteps)

  ClassDF_Befehl <- paste("data.frame(response = data$", noquote(Variablen_logistische_Regression[1]), ", predicted = round(fitted(mysteps),0))", sep="" )

  classDF <- eval(parse(text= ClassDF_Befehl))


  #Erechnen des Prozentsatzes korrekt vorhergesagt Baseline
  Moeglichkeit_1 <-mean(subdataset[,1])
  Moeglichkeit_2 <- 1-(mean(subdataset[,1]))
  Prozent_Korrekt_Vorher <-(max(Moeglichkeit_1,Moeglichkeit_2))*100


  #Erechnen des Prozentsatzes korrekt vorhergesagt mit Modell
  Klassifikationstabelle_nachher <- as.data.frame(xtabs(~ predicted + response, data = classDF))
  Korrekt_0 <- Klassifikationstabelle_nachher$Freq[1]
  Korrekt_1 <- Klassifikationstabelle_nachher$Freq[4]
  Summe_Korrekt <- Korrekt_0 + Korrekt_1
  Summe_Insgesamt <-sum(Klassifikationstabelle_nachher$Freq)
  Prozent_Korrekt_Nachher <- (Summe_Korrekt/Summe_Insgesamt)*100


  # Hier baue ich jetzt noch eine schoene Tabelle

  Ergebnistabelle <- as.data.frame(round(Modellzusammenfassung$coefficients, digits = 3))
  Ergebnistabelle$Odds_Rations <- Odds_Ratios$Odds_Ratio
  Ergebnistabelle$unteres_CI <- Odds_Ratios$`2.5 %`
  Ergebnistabelle$oberes_CI <- Odds_Ratios$`97.5 %`


  ## Je nachdem, ob die Odds_Ratio positiv oder negativ ist, muss der Prozentsatz anders berechnet werden.
  if (Ergebnistabelle$Odds_Rations[2] > 1) {Odds_Ratio_Prozent_UV1 <- (Ergebnistabelle$Odds_Rations[2]-1 ) *100
  } else {Odds_Ratio_Prozent_UV1<- (1 -Ergebnistabelle$Odds_Rations[2] ) *100}




  Tabellenueberschrift <- paste("Tabelle ", tabnumber, " : Logistisches Regressionsmodell der AV "  , dv  , ".", sep="")

  Tabellentext <- paste("Die Signifikanz des logistischen Regressionsmodells als Ganzes ist Chi-Quadrat(", Freihietsgrade, ") = "  , round(Chi_Quadrat_Wert, digits = 2)  , ", p = ",  round(Sig_Chi_Quadrat, digits = 3) , " n = " , Summe_Insgesamt, ". Die Vorhersagegenauigkeit des Modells steigt von " , round(Prozent_Korrekt_Vorher, digits = 1) , " Prozent (Baseline) auf ",  round(Prozent_Korrekt_Nachher, digits = 1), " Prozent. Das R-Quadrat nach Nagelkerke betraegt " ,    round(Nagelkerke_Wert, digits = 3), "."       , sep="")
  Tabellentext2positiv <- paste("Steigt die unabhaengige Variable ", iv[1], " um eine Einheit, so nimmt die relative Wahrscheinlichkeit, dass die AV ", dv, " zutrifft um ",round(Odds_Ratio_Prozent_UV1, digits = 1), "% zu.", sep ="")
  Tabellentext2negativ <- paste("Steigt die unabhaengige Variable ", iv[1], " um eine Einheit, so nimmt die relative Wahrscheinlichkeit, dass die AV ", dv, " zutrifft um ",round(Odds_Ratio_Prozent_UV1, digits = 1), "% ab", sep ="")

  ## Je nachdem, ob die Odds_Ratio positiv oder negativ ist, muss der Tabellentext leicht anders sein.
  if (Ergebnistabelle$Odds_Rations[2] > 1) {Tabellentext2 <- Tabellentext2positiv
  } else {Tabellentext2 <- Tabellentext2negativ}

  Tabellentext_Gesamt <- paste(Tabellentext, Tabellentext2)



  result <- Ergebnistabelle %>%
    kbl(align = "c", digits = 2, caption = Tabellenueberschrift ) %>%
    kable_classic_2(full_width = F, html_font = "Cambria") %>%
    footnote(general = Tabellentext_Gesamt
    )


  # Hier wird jetzt noch eine Tabelle erzeugt, die direkt als Word-Dokument vorliegt.


  # Hier f?ge ich jetzt die Zeilennamen, in denen die Variablennamen drinstehen, noch als extra Spalte hinzu.
  Ergebnistabelle$Variable <- as.character(row.names(Ergebnistabelle))
  # Jetzt kommt die letzte Spalte noch nach ganz vorne
  Ergebnistabelle <- Ergebnistabelle %>% dplyr::select(Variable, everything())


  # Hier weren jetzt noch Tabellen?berschrift, Tabellentext, und die Spaltennamen f?r die Word-Tabelle erzeugt.
  Spaltennamen_Word  <- c("Variable", "Estimate", "SE", "z-Wert", "p-Wert", "Odds_Ratio", "unteres_CI", "oberes_CI")
  Spaltennamen_Word_Zwischenschritt <- paste(Spaltennamen_Word[1:length(Spaltennamen_Word)-1], "|")
  Spaltennamen_Word_Zwischenschritt_2 <- c(Spaltennamen_Word_Zwischenschritt, Spaltennamen_Word [length(Spaltennamen_Word)], collapse = " ")
  Spaltennamen_Word_final <- paste(Spaltennamen_Word_Zwischenschritt_2, collapse = " ")

  # Hier brauche ich einen Vektor, der Anzeigt, wie viele Spalten meine letztendliche Tabelle hat.
  Tabellen_Spaltenzahl<- c(rep(1,length(Spaltennamen_Word)))

  # Hier muss ich jetzt festlegen, wie viel breiter die erste Spalte sein soll. ICh sage mal 3x so breit
  Tabellen_Spaltenzahl[1] <- 3

  Ergebnistabelle$Odds_Rations <- as.numeric(Ergebnistabelle$Odds_Rations)
  Ergebnistabelle$unteres_CI <- as.numeric(Ergebnistabelle$unteres_CI)
  Ergebnistabelle$oberes_CI <- as.numeric(Ergebnistabelle$oberes_CI)

  # Hier runde ich jetzt alle Spalten, die numerisch sind.
  Ergebnistabelle <- Ergebnistabelle %>% mutate_if(is.numeric, round, digits=3)
  # Hier konvertiere ich alle Spalten in Chars, das gibt weniger SChwierigkeiten beim Export.
  Ergebnistabelle <- Ergebnistabelle %>% mutate_if(is.numeric,as.character)


  # Hier wird die Post-Hoc-Tabelle jetzt in Word gebaut und gespeichert.
  Ergebnistabelle %>%
    rtf_title(title = Tabellenueberschrift, text_format = c("i"), text_justification = c("l"))%>%
    rtf_footnote(cell_justification = c("l"),footnote = Tabellentext_Gesamt, text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", text_justification = c("l")) %>%
    rtf_body(cell_justification = c("l"), as_colheader = FALSE,  col_rel_width = Tabellen_Spaltenzahl,  text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", border_last = "single", border_first = "single") %>%
    rtf_colheader(cell_justification = c("l"), col_rel_width = Tabellen_Spaltenzahl, colheader = Spaltennamen_Word_final , text_font_size = 12 , border_bottom = "single", border_left = "", border_right = "", border_top = "", ) %>%
    rtf_page(orientation = "landscape", border_last = "", border_first = "single") %>%
    rtf_encode() %>%
    write_rtf("Word_Tabelle_logistische_Regression.rtf")

  Hinweis_Regressiontabelle <- c("Die Tabelle mit den Ergebnissen der logistischen Regression finden Sie unter dem Namen Word_Tabelle_logistische_Regression.rtf in Ihrem Working-Directory.")


  return(list(Hinweis_Regressiontabelle, result ))


}
