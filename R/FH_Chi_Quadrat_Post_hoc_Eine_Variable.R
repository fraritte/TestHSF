#' Eine Funktion, die bestehende R-Pakete zusammenschnürt um Post-Hoc-Tests eines einfachen Chi-Quadrat-Tests mit einer Variablen als Word-Datei auszugeben. Diese sollte nur interpretiert werden, wenn der Chi-Quadrat-Test signifikant ist. ACHTUNG: Der Befehl funktioniert nur, wenn die kategoriale Variable mindestens 3 verschiedene Auspraegungen hat.
#' @param data als (data.frame), voreingestellt ist "dataset". Hier muss der Datensatz angegeben werden.
#' @param dv als Charakter-Vektor mit der Länge 1. Voreingestellt ist "AV". Hier muss die kategoriale Variable angegeben werden, ueber die der Chi-Quadrat-Test laufen soll.
#' @param tabnumber als Vektor mit der Länge 1 oder als Zahl. Voreingestellt ist "Tabellennummer". Das ist die Tabellennummer, die dann in der Tabellenueberschrift der Post-Hoc-Tests-Tabelle steht.
#' @return Hauptsaechlich wird eine Word-Datei erstellt. Die Word-Datei Word_Tabelle_Chi_Quadrat_Posthoc beinhaltet die Ergebnisse der Post-Hoc-Tests.
#' @import dplyr
#' @import knitr
#' @import kableExtra
#' @import expss
#' @import lsr
#' @import gmodels
#' @import sjlabelled
#' @import chisq.posthoc.test
#' @import r2rtf
#' @export


FH_Chi_Quadrat_Post_Hoc_Eine_Variable <- function(data = dataset, dv=AV,  tabnumber = Tabellennummer){



  ##### Zwischenvariablen / Zwischenbefehle

  Dataset_Verwendete_Variablen <- paste("data$",dv, sep="", collapse = ",")

  subdataset_Befehl <- paste( "data.frame(", Dataset_Verwendete_Variablen, ")" ,sep="")

  subdataset <- eval(parse(text= subdataset_Befehl))

  Crosstable_Befehl <- paste("cro(", Dataset_Verwendete_Variablen, ")", sep="")

  Tabellen_Befehl <- eval(parse(text= paste("table(",Dataset_Verwendete_Variablen,")",sep="")))

# Tabellenueberschrift <- paste("Tabelle ", tabnumber, ": Kontingenztabelle der Variable ", dv [1] ,".", sep = ""  ) # Hier wird der Text, der unter der Tabelle steht generiert.


  ## Berechnen des Chi-Quadrat-Tests
  #Chi_Quadrat_Test <- summary(Tabellen_Befehl)
 # Chi_Quadrat_Test <- chisq.test(Tabellen_Befehl)

  ## Berechnen von Cramers V
#  Cramers_V <-  cramersV(Tabellen_Befehl)

  ## Grobinterpretation von Cramers V

#  if (Cramers_V < 0.3) {Cramer_Effekt <- c("kleinen")
#  } else if (Cramers_V > 0.5) {Cramer_Effekt <- c("gro?en")
#  } else {Cramer_Effekt <- c("mittleren")}

#  Tabellentext <- paste("Chi-Quadrat(",Chi_Quadrat_Test$parameter, ", n = ", nrow(subdataset), ")", " = ", round(Chi_Quadrat_Test$statistic, digits = 2), ", p =" , round(Chi_Quadrat_Test$p.value, digits = 3), ". Cramer's V betraegt ", round(Cramers_V, digits = 2), ". Das entspricht nach Cohen (1988) einem " , Cramer_Effekt, " Effekt.", sep="")


  ########Erstellen einer Kontinzentabelle


 Vektor_Label_1 <-as.vector (get_labels(subdataset[1]))
 Vektor_Label_1b <- unlist(Vektor_Label_1, use.names = FALSE)
# Zeilennamen <- c(Vektor_Label_1b)


#  Kreuztabelle <- eval(parse(text= Crosstable_Befehl))

#  rownames(Kreuztabelle) <- c(Zeilennamen, "Total")

#  Kreuztabelle <- Kreuztabelle [,-1]


#  result <- Kreuztabelle %>%
#    kbl(    align = "c", digits = 2, caption = Tabellenueberschrift ) %>%
#    kable_classic_2(full_width = F, html_font = "Cambria") %>%
#    footnote(general = Tabellentext
#    )


  # Hier wird jetzt noch eine Tabelle erzeugt, die direkt als Word-Dokument vorliegt.

  # Hier weren jetzt noch Tabellen?berschrift, Tabellentext, und die Spaltennamen f?r die Word-Tabelle erzeugt.

#  Spaltennamen_Word  <- c(dv, colnames(Kreuztabelle))
#  Spaltennamen_Word_Zwischenschritt <- paste(Spaltennamen_Word[1:length(Spaltennamen_Word)-1], "|")
#  Spaltennamen_Word_Zwischenschritt_2 <- c(Spaltennamen_Word_Zwischenschritt, Spaltennamen_Word [length(Spaltennamen_Word)], collapse = " ")
#  Spaltennamen_Word_final <- paste(Spaltennamen_Word_Zwischenschritt_2, collapse = " ")

  # Hier fuege ich jetzt die Zeilennamen, in denen die Variablennamen drinstehen, noch als extra Spalte hinzu.
#  Kreuztabelle$Kategorie <- as.character(row.names(Kreuztabelle))
  # Jetzt kommt die letzte Spalte noch nach ganz vorne

#  Kreuztabelle <- as.data.frame(Kreuztabelle)
#  Kreuztabelle <- Kreuztabelle %>% select(Kategorie, everything())



  # Hier brauche ich einen Vektor, der Anzeigt, wie viele Spalten meine letztendliche Tabelle hat.
#  Tabellen_Spaltenzahl<- c(rep(1,length(Spaltennamen_Word)))

  # Hier muss ich jetzt festlegen, wie viel breiter die erste Spalte sein soll. ICh sage mal 3x so breit
#  Tabellen_Spaltenzahl[1] <- 3



  # Hier wird die Post-Hoc-Tabelle jetzt in Word gebaut und gespeichert.
#  Kreuztabelle %>%
#    rtf_title(title = Tabellenueberschrift, text_format = c("i"), text_justification = c("l"))%>%
#    rtf_footnote(cell_justification = c("l"),footnote = Tabellentext, text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", text_justification = c("l")) %>%
#    rtf_body(cell_justification = c("l"), as_colheader = FALSE,  col_rel_width = Tabellen_Spaltenzahl,  text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", border_last = "single", border_first = "single") %>%
#    rtf_colheader(cell_justification = c("l"), col_rel_width = Tabellen_Spaltenzahl, colheader = Spaltennamen_Word_final , text_font_size = 12 , border_bottom = "single", border_left = "", border_right = "", border_top = "", ) %>%
#    rtf_page(orientation = "landscape", border_last = "", border_first = "single") %>%
#    rtf_encode() %>%
#    write_rtf("Word_Tabelle_Haeufigkeitstabelle.rtf")

#  Hinweis_Haeufingkeitstabelle <- c("Die Hauefingkeitstabelle mit dem Chi-Quadrat-Test finden Sie unter dem Namen Word_Tabelle_Haeufigkeitstabelle.rtf in Ihrem Working-Directory.")





  ################################################################################
  ########optionale Befehle, auskommentiert ######################################
  ################################################################################


  #### Post-Hoc-Test##############################################################

  # Falls der Chi-Quadrat-Test signifikant wird, wei? man zwar, DASS es einen signifikaten Unterschied in den Daten gibt,
  # d.h., dass die beobachteten Werte von den erwarteten Werten abweichen.
  # Aber man wei? noch nicht, WO diese Abweichung ist. Daf?r macht man sog. Post-Hoc-Tests,
  # d.h. man schaut, welche an welcher Stelle / an welchen stellen die beobachteten Werte signifikant von den erwarteten Werten abweichen .






  ## Dieser Teil ist von PeterStatistics.com uebernommen und angepasst

  #Store frequencies of each categories as a table:
  myFreq <- as.data.frame(Tabellen_Befehl)

  #List all possible pairs
  comb <-combn(myFreq$Var1,2)

  #determine the frequency of each pair
  comb2 <- data.frame(t(combn(myFreq$Freq,2)))

  #sum of frequencies for each pair
  comb2$PairN <- comb2$X1+comb2$X2

  #perform binomial test for each pair
  comb2$sig <- apply(comb2,1,function(x) binom.test(x[1],x[3])$p.value)

  #adjustment using Bonferroni
  npairs<-dim(comb2)[1]
  comb2$adjSig <- comb2$sig*npairs

  #list all results
  comb <- data.frame(t(comb))
  Post_Hoc_Tabelle <- cbind(comb, comb2)
  last<-2*npairs
  Post_Hoc_Tabelle<-Post_Hoc_Tabelle[-c(npairs+1:last), ]

  levels(Post_Hoc_Tabelle$t.comb.) <- Vektor_Label_1b

  #####Ende ?Bernahme von PeterStatistics.com###




  Tabellenueberschrift_posthoctabelle <- paste("Tabelle ", (tabnumber), ": Post-Hoc-Tests des Chi-Quadrat-Tests der Variable ", dv [1] , ".", sep = ""  ) # Hier wird der Text, der unter der Tabelle steht generiert.
  Tabellentext_posthoctabelle <- c(' "p-Wert"  < 0.05 = signifikante Abweichung der beobachteten Haeufigkeiten von den erwarteten Haeufigkeiten, unkorrigiert.  Der "adjustierte p-Wert" ist Holm-Bonferroni-korrigiert. Ob dieser Holm-Bonferroni-korrigierte Test signifikant ist oder nicht ist in der Spalte "adj.sig" zu finden.' )


  Post_Hoc_Tabelle_Hilfe <- as.data.frame(Post_Hoc_Tabelle$t.comb.)
  Post_Hoc_Tabelle_Hilfe <- Post_Hoc_Tabelle_Hilfe[complete.cases(Post_Hoc_Tabelle_Hilfe),]

  Post_Hoc_Tabelle_Hilfe[] <- lapply(Post_Hoc_Tabelle_Hilfe, as.character)

  #Post_Hoc_Tabelle_Hilfe <- as.data.frame(Post_Hoc_Tabelle_Hilfe)

  Post_Hoc_Tabelle$Auspraegung_1 <- Post_Hoc_Tabelle_Hilfe [,1]
  Post_Hoc_Tabelle$Auspraegung_2 <- Post_Hoc_Tabelle_Hilfe [,2]

  Post_Hoc_Tabelle <- subset(Post_Hoc_Tabelle, select = c(Auspraegung_1, Auspraegung_2,X1,X2,PairN,sig,adjSig))



  # Hier wandel ich jetzt die Bonferroni-korrigierten p-Werte in Holm-Bonferroni-Korrigierte p-Werte um.

  # Hier sortiere ich die Post-Hoc-Tests nach P-Wert.
  # D.h. der "signifikanteste" Paarvergleich steht ganz oben.
  # Das mache ich, um die Holm-Bonferroni-Korrektur anwenden zu koennen.
  # Ich schreibe einfach den entsprechend multiplizierten P-WErt in die Spalte "adjusted p-wert".
  Post_Hoc_Tabelle <- Post_Hoc_Tabelle[order(Post_Hoc_Tabelle$sig),]
  for (i in 1:length(Post_Hoc_Tabelle$sig)) {
    Post_Hoc_Tabelle$adjSig[i] <- (Post_Hoc_Tabelle$sig[i] * (1+length(Post_Hoc_Tabelle$sig) - i))
  }

  # Ich lege noch eine neue Spalte an, in der "sig." steht, wenn der adjustierte P-Wert unter 0,05 ist. und "non-sig." wenn nicht.
  for (i in 1:length(Post_Hoc_Tabelle$sig)) {
    if (Post_Hoc_Tabelle$adjSig[i] < 0.05) {
      Post_Hoc_Tabelle$sig_or_not[i] <- "sig."
    } else {Post_Hoc_Tabelle$sig_or_not[i] <- "non-sig."
    }
  }

  # Da man bei Holm-Bonferroni ja abbrechen muss, sobald ein Test nicht signifikant ist, gehe ich diese neue Spalte nochmal durch und ?berschreibe
  # jedes "sig." durch ein "non-sig,", wenn in der Zeile davor ein "non-sig." stand.
  for (i in 2:length(Post_Hoc_Tabelle$sig)) {
    if (Post_Hoc_Tabelle$sig_or_not[i] == "sig." && Post_Hoc_Tabelle$sig_or_not[i-1] == "non-sig.") {
      Post_Hoc_Tabelle$sig_or_not[i] <- "non-sig."
    }
  }

  colnames(Post_Hoc_Tabelle) <- c("Auspraegung_1", "Auspraegung_2" , "N_1", "N_2" , "PairN" , "p-Wert" , "adjustierter p-Wert", "adj.sig")

  result_2 <-Post_Hoc_Tabelle %>%
    kbl(align = "c", digits = 2, caption = Tabellenueberschrift_posthoctabelle ) %>%
    kable_classic_2(full_width = F, html_font = "Cambria") %>%
    footnote(general = Tabellentext_posthoctabelle
    )


  # Hier wird jetzt noch eine Tabelle erzeugt, die direkt als Word-Dokument vorliegt.

  # Hier weren jetzt noch Tabellen?berschrift, Tabellentext, und die Spaltennamen f?r die Word-Tabelle erzeugt.

  Spaltennamen_Word  <- c(colnames(Post_Hoc_Tabelle))
  Spaltennamen_Word_Zwischenschritt <- paste(Spaltennamen_Word[1:length(Spaltennamen_Word)-1], "|")
  Spaltennamen_Word_Zwischenschritt_2 <- c(Spaltennamen_Word_Zwischenschritt, Spaltennamen_Word [length(Spaltennamen_Word)], collapse = " ")
  Spaltennamen_Word_final <- paste(Spaltennamen_Word_Zwischenschritt_2, collapse = " ")

  Post_Hoc_Tabelle$`p-Wert` <- round(Post_Hoc_Tabelle$`p-Wert`, digits = 3)
  Post_Hoc_Tabelle$`adjustierter p-Wert` <- round(Post_Hoc_Tabelle$`adjustierter p-Wert`, digits = 3)

  # Hier runde ich jetzt alle Spalten, die numerisch sind.
  Post_Hoc_Tabelle <- Post_Hoc_Tabelle %>% mutate_if(is.numeric, round, digits=3)
  # Hier konvertiere ich alle Spalten in Chars, das gibt weniger SChwierigkeiten beim Export.
  Post_Hoc_Tabelle <- Post_Hoc_Tabelle %>% mutate_if(is.numeric,as.character)


  # Hier wird die Post-Hoc-Tabelle jetzt in Word gebaut und gespeichert.
  Post_Hoc_Tabelle %>%
    rtf_title(title = Tabellenueberschrift_posthoctabelle, text_format = c("i"), text_justification = c("l"))%>%
    rtf_footnote(cell_justification = c("l"),footnote = Tabellentext_posthoctabelle, text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", text_justification = c("l")) %>%
    rtf_body(cell_justification = c("l"), as_colheader = FALSE,  col_rel_width = c(3,3,1,1,1,1,1,1),  text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", border_last = "single", border_first = "single") %>%
    rtf_colheader(cell_justification = c("l"), col_rel_width = c(3,3,1,1,1,1,1,1), colheader = Spaltennamen_Word_final , text_font_size = 12 , border_bottom = "single", border_left = "", border_right = "", border_top = "", ) %>%
    rtf_page(orientation = "landscape", border_last = "", border_first = "single") %>%
    rtf_encode() %>%
    write_rtf("Word_Tabelle_Chi_Quadrat_Posthoc.rtf")

  Hinweis_Post_Hoc_Tabelle <- c("Die Tabelle  mit den Ergebnissen der Post-hoc-Tests finden Sie unter dem Namen Word_Tabelle_Chi_Quadrat_Posthoc.rtf in Ihrem Working-Directory. Sie sollten die Post-Hoc-Tests nur interpretieren, wenn der Chi-Quadrat-Test signifikant geworden ist.")


  return(list(result_2, Hinweis_Post_Hoc_Tabelle))


}
