#' Eine Funktion, die bestehende R-Pakete zusammenschnürt um das Ergebnis einer einfaktoriellen Varianzanalyse (ANOVA) als Word-Datei auszugeben.
#' @param data als (data.frame), voreingestellt ist "dataset". Hier muss der Datensatz angegeben werden.
#' @param dv als Charakter-Vektor mit der Länge 1. Voreingestellt ist "AV". Hier muss eine numerische Variable (metrisch) angegeben werden.
#' @param iv als Charakter-Vektor mit der Länge 1. Voreingestellt ist "UV". Hier muss eine kategoriale Variable (nominal/Faktor, belabeled) angegeben werden.
#' @param tabnumber als  Zahl. Voreingestellt ist "Tabellennummer". Das ist die Tabellennummer, die dann in der Tabellenueberschrift der deskriptiven Angaben der ANOVA steht. Der Hypothesentest der ANOVA steht in der ANOVA-Tabelle mit der Tabellennummer tabnumber +1. Die Post-Hoc-Ergebnisse haben dann die Tabellennummer tabnumber+2.
#' @return Hauptsaechlich werden drei Word-Dateien erstellt. Die Word-Datei ANOVA-Tabelle_Tabellennummer_Deskriptive_Ergebnisse_APA. enthält die deskriptiven Angaben der ANOVA, die Word-Datei ANOVA-Tabelle_Tabellennummer_APA"beinhaltet den ANOVA-Hypothesen-Test. Die Word-Datei "Word_Tabelle_ANOVA_Posthoc_Ergebnisse" beinhaltet Homl-Bonferroni-korrigierte Post-Hoc-Tests. Zusaetzlich wird noch eine Abbildung erstellt.

#' @import dplyr
#' @import knitr
#' @import kableExtra
#' @import apaTables
#' @import r2rtf
#' @import expss
#' @import ggplot2
#' @import jtools
#' @import rstatix
#' @import sjlabelled
#' @import jmv
#' @export


FH_Einfaktorielle_ANOVA <- function(data = dataset, dv=AV, iv= UV,  tabnumber = Tabellennummer){




  # Name der Word-Datei, die die ANOVA_Ergebnisse enthaelt
  Tabellenname_ANOVA <- paste("Alternative_ANOVA_Ergebnisdarstellung_", (tabnumber+1), "_APA.doc", sep="")



  UV_plus <- iv

  Variablen_ANOVA <- vctrs::vec_c(dv, iv) # Hier bilde ich einen neuen Vektor, in dem erst die AV, dann die UV steht

  subdataset <- as.data.frame(subset(data, select = Variablen_ANOVA)) # Alle AV und UV werden jetzt in einem Subdatensatz gespeichert

  Vektor_Label_2 <-as.vector (get_labels(subdataset[2]))
  Vektor_Label_2b <- unlist(Vektor_Label_2, use.names = FALSE)
  Zeilennamen <- c(Vektor_Label_2b)


  # Hier wird dafuer gesorgt, dass die UV auf jeden Fall ein "Faktor" ist, also als nominale Variable behandelt wird.
  subdataset[,2:length(subdataset)] <- as_factor(subdataset[,2:length(subdataset)])






  # Das hier ist die deskriptive Statistik fuer die Ausgabe in knitr, die Word-Datei sieht etwas besser aus, wird spaete rerzeugt.
  Tabellenueberschrift_Deskriptiv <- paste("Tabelle ", tabnumber, ": Mittelwert und Standardabweichung der AV ",noquote(dv), " fuer die verschiedenen Gruppen der Varianzanalyse",  sep="")


  Deskriptiv_knitr_Befehl <- paste('subdataset%>% group_by(', noquote(iv[1]), ') %>% get_summary_stats (', noquote(dv), ', type ="mean_sd")', sep="")
  Deskriptiv_knitr <- eval(parse(text= Deskriptiv_knitr_Befehl))

  Deskriptiv_knitr <- subset(Deskriptiv_knitr, select = -variable)
  Deskriptiv_knitr[,1] <- Zeilennamen

  #Hier wird die deskriptive Statistik  jetzt mit knitr "veroeffentlicht". Sieht nicht ganz so gut aus wie die Word-Version, passt aber.
  result <- Deskriptiv_knitr %>%
    kbl(align = "c", digits = 2, caption = Tabellenueberschrift_Deskriptiv) %>%
    kable_classic_2(full_width = F, html_font = "Cambria" )




  # Hier wird jetzt noch eine Word-Tabelle erstellt mit den Ergebnissen der ANOVA

  # Hier muessen die Spaltennamen fuer die Word-Tabelle nochmal neu aufbereitet werden. Zwischen jedem Spalennamen muss ein | stehen.
  Spaltennamen <- colnames(Deskriptiv_knitr)
  Spaltennamen_Zwischenschritt <- paste(Spaltennamen[1:length(Spaltennamen)-1], "|")
  Spaltennamen_Zwischenschritt2 <- c(Spaltennamen_Zwischenschritt, Spaltennamen [length(Spaltennamen)], collapse = " ")
  Spaltennamen_final <- paste(Spaltennamen_Zwischenschritt2, collapse = " ")



  # Hier wird die Tabelle jetzt in Word gebaut und gespeichert.
  Deskriptiv_knitr %>%
    rtf_title(title = Tabellenueberschrift_Deskriptiv, text_format = c("i"), text_justification = c("l"))%>%
    #rtf_footnote(cell_justification = c("l"),footnote = Tabellentext, text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", text_justification = c("l")) %>%
    rtf_body(cell_justification = c("l"), as_colheader = FALSE,  col_rel_width = c(3,1,1,1),  text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", border_last = "single", border_first = "single") %>%
    rtf_colheader(cell_justification = c("l"), col_rel_width = c(3,1,1,1), colheader = Spaltennamen_final , text_font_size = 12 , border_bottom = "single", border_left = "", border_right = "", border_top = "", ) %>%
    rtf_page(orientation = "landscape", border_last = "", border_first = "single") %>%
    rtf_encode() %>%
    write_rtf("Word_Tabelle_Einfaktorielle_ANOVA_Deskriptive_Ergebnisse.rtf")



  Hinweis_ANOVA_Deskriptiv <- c("Die deskriptiven Ergebnisse der ANOVA finden Sie unter dem Namen  Word_Tabelle_Einfaktorielle_ANOVA_Deskriptive_Ergebnisse.rtf  in Ihrem Working-Directory.")



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
  #Ergebnistabelle$Effektanteil_Absolut <- Ergebnistabelle$SS/ sum(Ergebnistabelle$SS[2:length(Ergebnistabelle$SS)])
  #Ergebnistabelle$Effektanteil_Absolut [1] <- NA
  #Ergebnistabelle$Effektanteil_Absolut [length(Ergebnistabelle$Effektanteil_Absolut)] <- NA
  #Ergebnistabelle$Effktanteil_relativ <- Ergebnistabelle$Effektanteil_Absolut / R_Quadrat


  # Hier bekommen die Spalten der Ergebnistabelle neue Namen
  colnames(Ergebnistabelle) <- c("Praediktor", "Sum of Squares", "df", "Mean Square", "F-Wert", "p-Wert", "Eta-Quadrat")

  # Hier die Ueberschrift und der Tabellentext der ANOVA-Ergebnistabelle
  Tabellenueberschrift <- paste("Tabelle ", (tabnumber+1), ": Ergebnisse der Varianzanalyse der AV "  , dv  , ".", sep="")
  Tabellentext <- paste("Das R-Quadrat betraegt " ,    round(R_Quadrat, digits = 3), "." , sep="")

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



  # Hier wird die Tabelle jetzt in Word gebaut und gespeichert.
  Ergebnistabelle %>%
    rtf_title(title = Tabellenueberschrift, text_format = c("i"), text_justification = c("l"))%>%
    rtf_footnote(cell_justification = c("l"),footnote = Tabellentext, text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", text_justification = c("l")) %>%
    rtf_body(cell_justification = c("l"), as_colheader = FALSE,  col_rel_width = c(2,1,1,1,1,1,1),  text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", border_last = "single", border_first = "single") %>%
    rtf_colheader(cell_justification = c("l"), col_rel_width = c(2,1,1,1,1,1,1), colheader = Spaltennamen_final , text_font_size = 12 , border_bottom = "single", border_left = "", border_right = "", border_top = "", ) %>%
    rtf_page(orientation = "landscape", border_last = "", border_first = "single") %>%
    rtf_encode() %>%
    write_rtf("Word_Tabelle_Einfaktorielle_ANOVA_Ergebnisse.rtf")



  Hinweis_ANOVA_Tabelle <- c("Die Tabelle mit den Ergebnissen der ANOVA finden Sie unter dem Namen  Word_Tabelle_Einfaktorielle_ANOVA_Ergebnisse.rtf  in Ihrem Working-Directory.")

  # Hier vergebe ich für die UV die Namen der Faktorstufen. Das ich das überhaupt tun muss, zeigt, wie wenig ich von R verstehe, denn je nachdem, welche PAkete geladen werden, ist das auch nicht notwendig...
  levels(subdataset[,2]) <- Zeilennamen

  Abbildungs_Befehl <- paste('plot <- ggplot(subdataset, aes(x = ', noquote(Variablen_ANOVA[2]), ', y = ',noquote(Variablen_ANOVA[1]), ')) +
        geom_jitter(width = 0.2) +
        stat_summary(fun=mean, geom = "point", size = 5, aes(ymax = ..y.., ymin = ..y..), width = 0.75, size = 1, linetype = "solid") +
        stat_summary(fun.data = mean_se, geom = "errorbar")+
        stat_summary(fun=mean, geom = "text", show_guide = FALSE, vjust=-0.7, aes( label=round(..y.., digits=1)))', sep = "")

  Abbildung <- eval(parse(text= Abbildungs_Befehl))

  #Hier wird die Abbildung jetzt ausgegeben
  APA_Abbildung <- Abbildung + theme_apa()



  #### Post-Hoc-Test##############################################################

  # Falls die ANOVA signifikant wird, weiss man zwar, DASS es einen signifikaten Unterschied in den Daten gibt,
  # d.h., dass es signifikante Abweichungen vom Gruppenmittelwert gibt.
  # Aber man weiss noch nicht, WO diese Abweichung ist. Dafuer macht man sog. Post-Hoc-Tests,
  # d.h. man schaut, welche Gruppenmittelwerte sich denn unterschieden, d.h.
  # wo genau die Unterschiede zwischen den Verschiedenen Faktorstufen liegen.

  # Dafuer nutze ich jetzt noch einmal ein anders Packet, naemlich jvm, das ich eigentlich fuer die ANCOVA nutze.

  #Hier der Befehlt fuer die ANOVA/ANCOVA, wieder kompliziert, damit es automatisch geht.
  ANOVA_Befehl <- paste('ancova(subdataset, dep = "', dv, '",  factors = "', iv, '", effectSize = "eta", ss = "3", postHoc = "', iv,'", emMeans = "', iv, '", ciWidth = 95, postHocCorr = "holm", emmPlots = TRUE, emmPlotData = TRUE, emmPlotError = "se", emmTables = TRUE)', sep="")
  ANOVA_Modell <- eval(parse(text= ANOVA_Befehl))




  # Hier ist der Post-Hoc-Test
  ANOVA_Modell$postHoc
  Post_hoc_Ergebnisse <- as.data.frame(ANOVA_Modell$postHoc[[1]])
  # hiermit wird das "active binding object" als data.frame abgespeichert.
  # das Kann ich dann wieder mit kbl als Tabelle formatieren.

  #Tabellenueberschrift
  Tabellenueberschrift <- paste("Tabelle ", (tabnumber+2), " : Post-Hoc-Tests der Faktorstufen der UV "  , iv  , ".", sep="")

  # Tabellenunterschrift
  Tabellentext <- c("md = Mittelwertsdifferenz, pholm = Signifikanzwert, Holm-Bonferroni-korrigiert")

  # Hier erstelle ich die Tabelle
  result3 <- Post_hoc_Ergebnisse %>%
    kbl(align = "c", digits = 2,  caption = Tabellenueberschrift) %>%
    kable_classic_2(full_width = F, html_font = "Cambria") %>%
    footnote(general = Tabellentext
    )



  # Hier weren jetzt noch Tabellenueberschrift, Tabellentext, und die Spaltennamen fuer die Word-Tabelle erzeugt.
  Spaltennamen_Posthoc  <- colnames(Post_hoc_Ergebnisse)
  Spaltennamen_Posthoc_Zwischenschritt <- paste(Spaltennamen_Posthoc[1:length(Spaltennamen_Posthoc)-1], "|")
  Spaltennamen_Posthoc_Zwischenschritt_2 <- c(Spaltennamen_Posthoc_Zwischenschritt, Spaltennamen_Posthoc [length(Spaltennamen_Posthoc)], collapse = " ")
  Spaltennamen_Posthoc_final <- paste(Spaltennamen_Posthoc_Zwischenschritt_2, collapse = " ")


  # Hier runde ich jetzt alle Spalten, die numerisch sind.
  Post_hoc_Ergebnisse <- Post_hoc_Ergebnisse %>% mutate_if(is.numeric, round, digits=3)
  # Hier konvertiere ich alle Spalten in Chars, das gibt weniger SChwierigkeiten beim Export.
  Post_hoc_Ergebnisse <- Post_hoc_Ergebnisse %>% mutate_if(is.numeric,as.character)



  # Hier wird die Post-Hoc-Tabelle jetzt in Word gebaut und gespeichert.
  Post_hoc_Ergebnisse %>%
    rtf_title(title = Tabellenueberschrift, text_format = c("i"), text_justification = c("l"))%>%
    rtf_footnote(cell_justification = c("l"),footnote = Tabellentext, text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", text_justification = c("l")) %>%
    rtf_body(cell_justification = c("l"), as_colheader = FALSE,  col_rel_width = c(3,1,3,1,1,1,1,1),  text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", border_last = "single", border_first = "single") %>%
    rtf_colheader(cell_justification = c("l"), col_rel_width = c(3,1,3,1,1,1,1,1), colheader = Spaltennamen_Posthoc_final , text_font_size = 12 , border_bottom = "single", border_left = "", border_right = "", border_top = "", ) %>%
    rtf_page(orientation = "landscape", border_last = "", border_first = "single") %>%
    rtf_encode() %>%
    write_rtf("Word_Tabelle_ANOVA_Posthoc_Ergebnisse.rtf")



Hinweis_ANOVA_Posthoc <- c("Die Post-Hoc-Tests der ANOVA finden Sie unter dem Namen  Word_Tabelle_ANOVA_Posthoc_Ergebnisse.rtf  in Ihrem Working-Directory. Sie sollten die Post-Hoc-Tests nur interpretieren, wenn die ANOVA signifikant geworden ist.")









  return(list(result, result2, APA_Abbildung, result3, Hinweis_ANOVA_Deskriptiv, Hinweis_ANOVA_Tabelle, Hinweis_ANOVA_Posthoc))


}


