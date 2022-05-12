
########## HINWEIS AN MICH SELBST: DIE BERECHNUNG DES ABSOLUTEN ETA-QUADRAT SCHEINT NUR BEI BALANCIERTEN DESIGNS ZU PASSEN.
########## BITTE DEN TEXT ANPASSEN BZW. DEN "FEHLER" KORRIGIEREN!!!


#' Eine Funktion, die bestehende R-Pakete zusammenschnürt um das Ergebnis einer zweifaktoriellen Varianzanalyse (2-way-ANOVA) als Word-Datei auszugeben.
#' @param data als (data.frame), voreingestellt ist "dataset". Hier muss der Datensatz angegeben werden.
#' @param dv als Charakter-Vektor mit der Länge 1. Voreingestellt ist "AV". Hier muss eine numerische Variable (metrisch) angegeben werden.
#' @param iv als Charakter-Vektor mit der Länge 2. Voreingestellt ist "UV". Hier müssen zwei kategoriale Variablen (nominal/Faktor, belabeled) angegeben werden.
#' @param tabnumber als  Zahl. Voreingestellt ist "Tabellennummer". Das ist die Tabellennummer, die dann in der Tabellenueberschrift der deskriptiven Angaben der 2-way-ANOVA steht. Der Hypothesentest der ANOVA steht in der ANOVA-Tabelle mit der Tabellennummer tabnumber +1. Die Post-Hoc-Ergebnisse haben dann die Tabellennummer tabnumber+2.
#' @return Hauptsaechlich werden drei Word-Dateien erstellt. Die Word-Datei ANOVA-Tabelle_Tabellennummer_Deskriptive_Ergebnisse_APA. enthält die deskriptiven Angaben der 2-way-ANOVA, die Word-Datei ANOVA-Tabelle_Tabellennummer_APA"beinhaltet den 2-way-ANOVA-Hypothesen-Test. Die Word-Datei "Word_Tabelle_ANOVA_Posthoc_Ergebnisse" beinhaltet Homl-Bonferroni-korrigierte Post-Hoc-Tests. Zusaetzlich wird noch eine Abbildung erstellt.

#' @import dplyr
#' @import knitr
#' @import kableExtra
#' @import apaTables
#' @import r2rtf
#' @import expss
#' @import ggplot2
#' @import emmeans
#' @import rstatix
#' @import CGPfunctions
#' @export


FH_Zweifaktorielle_ANOVA <- function(data = dataset, dv=AV, iv= UV,  tabnumber = Tabellennummer){

  options(knitr.kable.NA = '')


  # Name der Word-Datei, die die ANOVA_Ergebnisse enthaelt
  Tabellenname_ANOVA <- paste("Alternative_ANOVA_Ergebnisdarstellung_", (tabnumber+1), "_APA.doc", sep="")




  # Hier bilde ich einen neuen Vektor, der die UVs hat, aber dazwischen immer ein "+", damit ich das dann in den Modellbefehl einbauen kann
  UV_plus_Zwischenschritt <- paste(iv[1:length(iv)-1], "*")
  UV_plus_Zwischenschritt_2 <- c(UV_plus_Zwischenschritt, iv[length(iv)])
  UV_plus <- paste(UV_plus_Zwischenschritt_2, collapse = " ")

  Variablen_ANOVA <- vctrs::vec_c(dv, iv) # Hier bilde ich einen neuen Vektor, in dem erst die AV, dann die UV steht

  subdataset <- as.data.frame(subset(data, select = Variablen_ANOVA)) # Alle AV und UV werden jetzt in einem Subdatensatz gespeichert

  subdataset_Plot <- (subset(data, select = Variablen_ANOVA))


  Vektor_Label_2 <-as.vector (get_labels(subdataset[2]))
  Vektor_Label_2b <- unlist(Vektor_Label_2, use.names = FALSE)
  UV_1_Faktorstufen <- c(Vektor_Label_2b)

  Vektor_Label_3 <-as.vector (get_labels(subdataset[3]))
  Vektor_Label_3b <- unlist(Vektor_Label_3, use.names = FALSE)
  UV_2_Faktorstufen <- c(Vektor_Label_3b)



  # Hier wird dafuer gesorgt, dass die UV auf jeden Fall ein "Faktor" ist, also als nominale Variable behandelt wird.
  subdataset[,2:length(subdataset)] <- as_factor(subdataset[,2:length(subdataset)])

  # Hier vergebe ich für die UVs die Namen der Faktorstufen. Das ich das überhaupt tun muss, zeigt, wie wenig ich von R verstehe, denn je nachdem, welche PAkete geladen werden, ist das auch nicht notwendig...
  levels(subdataset[,2]) <- UV_1_Faktorstufen
  levels(subdataset[,3]) <- UV_2_Faktorstufen



  # Das hier ist die deskriptive Statistik fuer die Ausgabe in knitr, die Word-Datei sieht etwas besser aus, wird spaeter erzeugt.
  Tabellenueberschrift_Deskriptiv <- paste("Tabelle ", tabnumber, ": Mittelwert und Standardabweichung der AV ",noquote(dv), " fuer die verschiedenen Gruppen der Varianzanalyse",  sep="")


  Deskriptiv_knitr_Befehl <- paste('subdataset%>% group_by(', noquote(iv[1]), ' , ' , noquote(iv[2]) , ') %>% get_summary_stats (', noquote(dv), ', type ="mean_sd")', sep="")
  Deskriptiv_knitr <- eval(parse(text= Deskriptiv_knitr_Befehl))


  #Hier wird die deskriptive Statistik  jetzt mit knitr "veroeffentlicht". Sieht nicht ganz so gut aus wie die Word-Version, passt aber.
  result <- Deskriptiv_knitr %>%
    kbl(align = "c", digits = 2, caption = Tabellenueberschrift_Deskriptiv) %>%
    kable_classic_2(full_width = F, html_font = "Cambria" )









  options(contrasts = c("contr.sum", "contr.poly")) # Kontraste setzen, Befehl ist nur uebernommen aus apaTables.

  # Hier wieder der Befehl der  ANOVA, der ist hier nur zu kompliziert, damit das alles automatisiert ablaufen kann.
  Modellbefehl <- paste("lm(", noquote(Variablen_ANOVA[1]),   "~" , noquote(UV_plus) ,  ", data= subdataset)", sep ="")

  # Hier wird das Modell der einfaktoriellen ANOVA geschaetzt.
  Modell<- eval(parse(text= Modellbefehl))

  Modellzusammenfassung <- summary(Modell)


  R_Quadrat <- Modellzusammenfassung$r.squared


  #apa.aov.table(Modell, filename = Tabellenname_ANOVA, table.number = tabnumber)
  #body(apa.aov.table)

  # Hier baue ich jetzt die Ergebnis-Tabelle

  ANOVA_Hilfe <- apa.aov.table(Modell, filename = Tabellenname_ANOVA, table.number = tabnumber)
  ANOVA_Hilfe<-ANOVA_Hilfe$table_body
  Ergebnistabelle <- subset(ANOVA_Hilfe, select = -CI_90_partial_eta2)
  Ergebnistabelle$SS <- as.numeric(Ergebnistabelle$SS)
  Ergebnistabelle$Effektanteil_Absolut <- Ergebnistabelle$SS/ sum(Ergebnistabelle$SS[2:length(Ergebnistabelle$SS)])
  Ergebnistabelle$Effektanteil_Absolut [1] <- NA
  Ergebnistabelle$Effektanteil_Absolut [length(Ergebnistabelle$Effektanteil_Absolut)] <- NA
  Ergebnistabelle$Effktanteil_relativ <- Ergebnistabelle$Effektanteil_Absolut / R_Quadrat


  # Hier bekommen die Spalten der Ergebnistabelle neue Namen
  colnames(Ergebnistabelle) <- c("Praediktor", "Sum of Squares", "df", "Mean Square", "F-Wert", "p-Wert", "partielles Eta-Quadrat", "absolutes Eta-Quadrat", "relatives Eta-Quadrat")

  # Hier die Ueberschrift und der Tabellentext der ANOVA-Ergebnistabelle
  Tabellenueberschrift <- paste("Tabelle ", (tabnumber+1), ": Ergebnisse der Varianzanalyse der AV "  , dv  , ".", sep="")
  Tabellentext <- paste("Das R-Quadrat betraegt " ,    round(R_Quadrat, digits = 3), ". Das absolute Eta-Quadrat gibt an, wie viel Varianz der AV durch den jeweiligen Praediktor aufgeklaert wird. Die Summe der absoluten Anteile ergibt den Anteil der insgesamt erklaerten Varianz von ", round(R_Quadrat, digits = 3), ". Das relative Eta-Quadrat gibt an, wie stark der Einfluss eines Praediktors im Vergleich zu den anderen Praediktoren im Modell ist. Die Summe der relativen Anteile ergibt immer 1 (100%)."       , sep="")

  #Hier wird die Ergebnistabelle jetzt mit knitr "veroeffentlicht"
  result2 <- Ergebnistabelle %>%
    kbl(align = "c", digits = 2, caption = Tabellenueberschrift) %>%
    kable_classic_2(full_width = F, html_font = "Cambria" ) %>%
    footnote(general = Tabellentext
    )


  # Hier wird jetzt noch eine Word-Tabelle erstellt mit den Ergebnissen der ANOVA

  # Hier muessen die Spaltennamen fuer die Word-Tabelle nochmal neu aufbereitet werden. Zwischen jedem Spalennamen muss ein | stehen.
  Spaltennamen <- colnames(Ergebnistabelle)
  Spaltennamen_Zwischenschritt <- paste(Spaltennamen[1:length(Spaltennamen)-1], "|")
  Spaltennamen_Zwischenschritt2 <- c(Spaltennamen_Zwischenschritt, Spaltennamen [length(Spaltennamen)], collapse = " ")
  Spaltennamen_final <- paste(Spaltennamen_Zwischenschritt2, collapse = " ")

  # Ich bekomme es nicht hin, dass in der Word-Tabelle nur Zahlen mit 3 Nachkommastellen ausgegeben werden, daher verwandel ich die Zahlen einfach in Chars
  Ergebnistabelle$`absolutes Eta-Quadrat` <- as.character(round(Ergebnistabelle$`absolutes Eta-Quadrat`, digits = 3))
  Ergebnistabelle$`relatives Eta-Quadrat` <- as.character(round(Ergebnistabelle$`relatives Eta-Quadrat`, digits = 3))


  # Hier wird die Tabelle jetzt in Word gebaut und gespeichert.
  Ergebnistabelle %>%
    rtf_title(title = Tabellenueberschrift, text_format = c("i"), text_justification = c("l"))%>%
    rtf_footnote(cell_justification = c("l"),footnote = Tabellentext, text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", text_justification = c("l")) %>%
    rtf_body(cell_justification = c("l"), as_colheader = FALSE,  col_rel_width = c(2,1,1,1,1,1,1,1,1),  text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", border_last = "single", border_first = "single") %>%
    rtf_colheader(cell_justification = c("l"), col_rel_width = c(2,1,1,1,1,1,1,1,1), colheader = Spaltennamen_final , text_font_size = 12 , border_bottom = "single", border_left = "", border_right = "", border_top = "", ) %>%
    rtf_page(orientation = "landscape", border_last = "", border_first = "single") %>%
    rtf_encode() %>%
    write_rtf("Word_Tabelle_ANOVA_Ergebnisse.rtf")


  Hinweis_ANOVA_Tabelle <- c("Die Tabelle mit den Ergebnissen der ANOVA finden Sie unter dem Namen  Word_Tabelle_ANOVA_Ergebnisse.rtf  in Ihrem Working-Directory.")





  # Hier wird eine Tabelle mit der deskriptiven Statistik der ANOVA im APA-Style erstellt und als .doc-Datei (Word) gespeichert
  # Da mache ich es mir relativ einfach, da ich die Tabelle mit dem apaTables paket erstelle.

  # Name der Word-Datei, die die ANOVA_Descriptives enthaelt
  Tabellenname_2 <- paste("ANOVA-Tabelle_", tabnumber, "_Deskriptive_Ergebnisse_APA.doc", sep="")

  Descriptive_Tabelle <- paste('apa.2way.table(iv1 = ', noquote(Variablen_ANOVA[2]), ' ,iv2 = ',  noquote(Variablen_ANOVA[3]),   ', dv = ', noquote(Variablen_ANOVA[1]), ', data = subdataset,  filename = Tabellenname_2, table.number = tabnumber)', sep="")
  eval(parse(text= Descriptive_Tabelle))

  Hinweis_Deskriptive_ANOVA <- paste("Eine Tabelle mit der deskriptiven Statistik finden Sie unter dem Namen ", Tabellenname_2 , "in Ihrem Working-Directory.")




  #### Post-Hoc-Test##############################################################

  # Falls die ANOVA signifikant wird, weiss man zwar, DASS es einen signifikaten Unterschied in den Daten gibt,
  # d.h., dass es signifikante Abweichungen vom Gruppenmittelwert gibt.
  # Aber man weiss noch nicht, WO diese Abweichung ist. Dafuer macht man sog. Post-Hoc-Tests,
  # d.h. man schaut, welche Gruppenmittelwerte sich denn unterschieden, d.h.
  # wo genau die Unterschiede zwischen den Verschiedenen Faktorstufen liegen.

  # Hier werden jetzt die Post-hoc-Tests gerechnet.

  # Hier wird der erste Teil der Post-Hoc-Tests gerechnet, Hier getrennt fuer jede Faktorstufe der UV1
  Pairwise_1_Befehl <- paste("subdataset %>%  group_by (" , noquote(iv[1]),  ") %>% emmeans_test(", noquote(dv), " ~ ", noquote(iv[2]),  ', p.adjust.method = "none")')
  Pairwise_1 <- eval(parse(text= Pairwise_1_Befehl))

  # Hier wird der erste Teil der Post-Hoc-Tests gerechnet, Hier getrennt fuer jede Faktorstufe der UV2
  Pairwise_2_Befehl <- paste("subdataset %>%  group_by (" , noquote(iv[2]),  ") %>% emmeans_test(", noquote(dv), " ~ ", noquote(iv[1]),  ', p.adjust.method = "none")')
  Pairwise_2 <- eval(parse(text= Pairwise_2_Befehl))


  # Hier baue ich das in eine Tabelle, d.h. hier stehen jetzt alle Post-Hoc-Tests untereinander.
  Pairwise_Tabelle <- as.data.frame(mapply(c, Pairwise_1,Pairwise_2))

  Pairwise_Tabelle <- (subset(Pairwise_Tabelle, select = -.y.)) # Hier schmeisse ich eine unnoetige Spalte raus
  Pairwise_Tabelle <- (subset(Pairwise_Tabelle, select = -term)) # Hier schmeisse ich eine unnoetige Spalte raus
  Pairwise_Tabelle <- (subset(Pairwise_Tabelle, select = -p.adj.signif)) # Hier schmeisse ich eine unnoetige Spalte raus

  # Hier bekommen die Spalten neue Namen
  colnames(Pairwise_Tabelle) <- c("Teilgruppe", "Faktorstufe 1" , "Faktorstufe 2", "df" , "t-Wert" , "p-Wert" , "adjustierter p-Wert")

  Pairwise_Tabelle$`p-Wert` <- as.numeric(Pairwise_Tabelle$`p-Wert`) # hier wird der p-Wert von "Char" nach "numeric" umgewandelt, damit ich damit weiterrechnen kann


  # Hier sortiere ich die Post-Hoc-Tests nach P-Wert.
  # D.h. der "signifikanteste" Paarvergleich steht ganz oben.
  # Das mache ich, um die Holm-Bonferroni-Korrektur anwenden zu koennen.
  # ICh schreibe einfach den entsprechend multiplizierten P-WErt in die Spalte "adjusted p-wert".
  Pairwise_Tabelle_2 <- Pairwise_Tabelle[order(Pairwise_Tabelle$`p-Wert`),]
  for (i in 1:length(Pairwise_Tabelle_2$`p-Wert`)) {
    Pairwise_Tabelle_2$`adjustierter p-Wert`[i] <- (Pairwise_Tabelle_2$`p-Wert`[i] * (1+length(Pairwise_Tabelle_2$`p-Wert`) - i))
  }

  # Ich lege noch eine neue Spalte an, in der "sig." steht, wenn der adjustierte P-Wert unter 0,05 ist. und "non-sig." wenn nicht.
  for (i in 1:length(Pairwise_Tabelle_2$`p-Wert`)) {
    if (Pairwise_Tabelle_2$`adjustierter p-Wert`[i] < 0.05) {
      Pairwise_Tabelle_2$adj.sig[i] <- "sig."
    } else {Pairwise_Tabelle_2$adj.sig[i] <- "non-sig."
    }
  }

  # Da man bei Holm-Bonferroni ja abbrechen muss, sobald ein Test nicht signifikant ist, gehe ich diese neue Spalte nochmal durch und ?berschreibe
  # jedes "sig." durch ein "non-sig,", wenn in der Zeile davor ein "non-sig." stand.
  for (i in 2:length(Pairwise_Tabelle_2$`p-Wert`)) {
    if (Pairwise_Tabelle_2$adj.sig[i] == "sig." && Pairwise_Tabelle_2$adj.sig[i-1] == "non-sig.") {
      Pairwise_Tabelle_2$adj.sig[i] <- "non-sig."
    }
  }

  # Jetzt wird noch eine neue Spalte erzeugt, in der einfach nur die Testnummer des Tests steht.
  # Danach wird die Tabelle dann noch sortiert, d.h. wir haben jetzt wieder die urspruengliche Sortierung.
  Pairwise_Tabelle_2$Test_Nr <- as.numeric(row.names(Pairwise_Tabelle_2))
  Pairwise_Tabelle_2 <- Pairwise_Tabelle_2[order(Pairwise_Tabelle_2$Test_Nr),]

  # Jetzt kommt die letzte Spalte noch nach ganz vorne
  Pairwise_Tabelle_2 <- Pairwise_Tabelle_2[c(9,1,2,3,4,5,6,7,8)]

  # Hier werden die Zahlen dann nochmal gerundet.
  Pairwise_Tabelle_2$`t-Wert` <- round(as.numeric(Pairwise_Tabelle_2$`t-Wert`), digits = 2)
  Pairwise_Tabelle_2$`p-Wert` <- round(as.numeric(Pairwise_Tabelle_2$`p-Wert`), digits = 3)
  Pairwise_Tabelle_2$`adjustierter p-Wert` <- round(as.numeric(Pairwise_Tabelle_2$`adjustierter p-Wert`), digits = 3)

  # jetzt verwandel ich alle zahlen in Chars zurueck


  # Hier weren jetzt noch Tabellenueberschrift, Tabellentext, und die Spaltennamen fuer die Word-Tabelle erzeugt.
  Tabellenueberschrift_Posthoc <- paste("Tabelle ", (tabnumber+2), ": Ergebnisse der paarweisen Post-Hoc-Tests der AV "  , dv  , ".", sep="")
  Tabellentext_Posthoc <- c('Die "Teilgruppe" bezieht sich auf die Teilgruppe an Versuchspersonen, mit denen der Post-Hoc-Test gerechnet wurde. "Faktorstufe 1" und "Faktorstufe 2" bezieht sich auch die beiden Auspraegungen, die in der "Teilgruppe" miteinander verglichen wurden. Der "adjustierte p-Wert" ist Holm-Bonferroni-korrigiert. Ob dieser Holm-Bonferroni-korrigierte Test signifikant ist oder nicht ist in der Spalte "adj.sig" zu finden.' )
  Spaltennamen_Posthoc  <- colnames(Pairwise_Tabelle_2)
  Spaltennamen_Posthoc_Zwischenschritt <- paste(Spaltennamen_Posthoc[1:length(Spaltennamen_Posthoc)-1], "|")
  Spaltennamen_Posthoc_Zwischenschritt_2 <- c(Spaltennamen_Posthoc_Zwischenschritt, Spaltennamen_Posthoc [length(Spaltennamen_Posthoc)], collapse = " ")
  Spaltennamen_Posthoc_final <- paste(Spaltennamen_Posthoc_Zwischenschritt_2, collapse = " ")

  # Hier wird die Post-Hoc-Tabelle jetzt in Word gebaut und gespeichert.
  Pairwise_Tabelle_2 %>%
    rtf_title(title = Tabellenueberschrift_Posthoc, text_format = c("i"), text_justification = c("l"))%>%
    rtf_footnote(cell_justification = c("l"),footnote = Tabellentext_Posthoc, text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", text_justification = c("l")) %>%
    rtf_body(cell_justification = c("l"), as_colheader = FALSE,  col_rel_width = c(1,2,2,2,1,1,1,1,1),  text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", border_last = "single", border_first = "single") %>%
    rtf_colheader(cell_justification = c("l"), col_rel_width = c(1,2,2,2,1,1,1,1,1), colheader = Spaltennamen_Posthoc_final , text_font_size = 12 , border_bottom = "single", border_left = "", border_right = "", border_top = "", ) %>%
    rtf_page(orientation = "landscape", border_last = "", border_first = "single") %>%
    rtf_encode() %>%
    write_rtf("Word_Tabelle_ANOVA_Posthoc_Ergebnisse.rtf")



  #Hier wird die Post-Hoc-Tabelle jetzt mit knitr "veroeffentlicht"
  result3 <- Pairwise_Tabelle_2 %>%
    kbl(align = "c", digits = 2, caption = Tabellenueberschrift_Posthoc) %>%
    kable_classic_2(full_width = F, html_font = "Cambria" ) %>%
    footnote(general = Tabellentext_Posthoc
    )


  Hinweis_ANOVA_Posthoc <- c("Die Post-Hoc-Tests der ANOVA finden Sie unter dem Namen  Word_Tabelle_ANOVA_Posthoc_Ergebnisse.rtf  in Ihrem Working-Directory. Sie sollten die Post-Hoc-Tests nur interpretieren, wenn die ANOVA signifikant geworden ist.")





  # Hier wird jetzt noch eine Grafik erzeugt.

#  Dataset_Befehl_1 <- paste('data$', noquote(iv[1]), '<- as_factor(data$', noquote(iv[1]), ')', sep="")
#  Dataset_Befehl_2 <- paste('data$', noquote(iv[2]), '<- as_factor(data$', noquote(iv[2]), ')', sep="")
#  eval(parse(text= Dataset_Befehl_1))
#  eval(parse(text= Dataset_Befehl_2))

#  Faktorlevel_BEfehl_1 <- paste('levels(data$',noquote(iv[1]), ') <- UV_1_Faktorstufen', sep = ""  )
#  Faktorlevel_BEfehl_2 <- paste('levels(data$',noquote(iv[2]), ') <- UV_2_Faktorstufen', sep = ""  )

#  eval(parse(text= Faktorlevel_BEfehl_1))
#  eval(parse(text= Faktorlevel_BEfehl_2))



#  Plot_Befehl <- paste ('Plot2WayANOVA(', noquote(dv), " ~ ", noquote(iv[1]), ' * ', noquote(iv[2]), ', dataframe = data, plottype = "line", overlay.type = "none", mean.label = TRUE, posthoc.method = "lsd")' , sep="")


#  Plot <- eval(parse(text= Plot_Befehl))





  return(list(result, result2, result3, Hinweis_Deskriptive_ANOVA, Hinweis_ANOVA_Tabelle, Hinweis_ANOVA_Posthoc))


}
