#' Eine Funktion, die bestehende R-Pakete zusammenschnürt um das Ergebnis eines einfachen Chi-Quadrat-Tests mit zwei Variablen als Word-Datei auszugeben.
#' @param data als (data.frame), voreingestellt ist "dataset". Hier muss der Datensatz angegeben werden.
#' @param dv als Charakter-Vektor mit der Länge 1. Voreingestellt ist "AV". Hier muss die erste kategoriale Variable angegeben werden, ueber die der Chi-Quadrat-Test laufen soll.
#' @param iv als Charakter-Vektor mit der Länge 1. Voreingestellt ist "UV". Hier muss die zweite kategoriale Variable angegeben werden, ueber die der Chi-Quadrat-Test laufen soll.
#' @param tabnumber als  Zahl. Voreingestellt ist "Tabellennummer". Das ist die Tabellennummer, die dann in der Tabellenueberschrift der Chi-Quadrat-Tabelle steht.
#' @return Hauptsaechlich wird eine Word-Datei erstellt. Die Word-Datei Word_Tabelle_Haeufigkeitstabelle beinhaltet den Chi-Quadrat-Test.
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


FH_Chi_Quadrat_Post_Hoc_Zwei_Variablen <- function(data = dataset, dv=AV, iv= UV,  tabnumber = Tabellennummer){



  Verwendete_Variablen <- vctrs::vec_c(dv, iv) # Hier bilde ich einen neuen Vektor, in dem erst die AV, dann die UV steht

  Dataset_Verwendete_Variablen <- paste("dataset$",Verwendete_Variablen, sep="", collapse = ",")

  subdataset <- as.data.frame(subset(data, select = Verwendete_Variablen)) #  AV und UV werden jetzt in einem Subdatensatz gespeichert


  Crosstable_Befehl <- paste("cro(", Dataset_Verwendete_Variablen, ")", sep="")

  Tabellen_Befehl <- eval(parse(text= paste("table(",Dataset_Verwendete_Variablen,")",sep="")))

  Vektor_Label_1 <-as.vector (get_labels(subdataset[1]))
  Vektor_Label_1b <- unlist(Vektor_Label_1, use.names = FALSE)
  Zeilennamen <- c(Vektor_Label_1b)


  Vektor_Label_2 <-as.vector (get_labels(subdataset[2]))
  Vektor_Label_2b <- unlist(Vektor_Label_2, use.names = FALSE)
  Spaltennamen <- c(Vektor_Label_2b)

  #### Post-Hoc-Test##############################################################

  # Falls der Chi-Quadrat-Test signifikant wird, wei? man zwar, DASS es einen signifikaten Unterschied in den Daten gibt,
  # d.h., dass die beobachteten Werte von den erwarteten Werten abweichen.
  # Aber man wei? noch nicht, WO diese Abweichung ist. Daf?r macht man sog. Post-Hoc-Tests,
  # d.h. man schaut, welche an welcher Stelle / an welchen stellen die beobachteten Werte signifikant von den erwarteten Werten abweichen .

  Post_Hoc_Tabelle <- chisq.posthoc.test(Tabellen_Befehl, method = "holm")

  colnames(Post_Hoc_Tabelle) <- c("", "", Spaltennamen)

  Wiederholung_Zeilenname <- nrow(Post_Hoc_Tabelle)/length(Zeilennamen)

  Zeilennamen_Posthoctabelle <- rep(Zeilennamen, each = Wiederholung_Zeilenname)

  Post_Hoc_Tabelle [1] <- Zeilennamen_Posthoctabelle

  Tabellennummer_Posthoctabelle <- tabnumber

  Tabellenueberschrift_posthoctabelle <- paste("Tabelle ", Tabellennummer_Posthoctabelle, ": Post-Hoc-Tests des Chi-Quadrat-Tests zwischen ", Verwendete_Variablen [1] , " und ", Verwendete_Variablen [2], ".", sep = ""  ) # Hier wird der Text, der unter der Tabelle steht generiert.
  Tabellentext_posthoctabelle <- c(" p < 0.05 = signifikante Abweichung der beobachteten Haeufigkeiten von den erwarteten Haeufigkeiten, Holm-Bonferroni-korrigiert")

  result <- Post_Hoc_Tabelle %>%
    kbl(align = "c", digits = 2, caption = Tabellenueberschrift_posthoctabelle ) %>%
    kable_classic_2(full_width = F, html_font = "Cambria") %>%
    footnote(general = Tabellentext_posthoctabelle
    )




  # Hier wird jetzt noch eine Tabelle erzeugt, die direkt als Word-Dokument vorliegt.

  # Hier weren jetzt noch Tabellen?berschrift, Tabellentext, und die Spaltennamen f?r die Word-Tabelle erzeugt.
  colnames(Post_Hoc_Tabelle)[1] <- Verwendete_Variablen[1]
  colnames(Post_Hoc_Tabelle)[2] <- "-"

  Spaltennamen_Word  <- c(colnames(Post_Hoc_Tabelle))
  Spaltennamen_Word_Zwischenschritt <- paste(Spaltennamen_Word[1:length(Spaltennamen_Word)-1], "|")
  Spaltennamen_Word_Zwischenschritt_2 <- c(Spaltennamen_Word_Zwischenschritt, Spaltennamen_Word [length(Spaltennamen_Word)], collapse = " ")
  Spaltennamen_Word_final <- paste(Spaltennamen_Word_Zwischenschritt_2, collapse = " ")

  # Hier brauche ich einen Vektor, der Anzeigt, wie viele Spalten meine letztendliche Tabelle hat.
  Tabellen_Spaltenzahl<- c(rep(1,length(Spaltennamen_Word)))

  # Hier muss ich jetzt festlegen, wie viel breiter die erste Spalte sein soll. ICh sage mal 1x so breit
  Tabellen_Spaltenzahl[1] <- 1

  # Hier runde ich jetzt alle Spalten, die numerisch sind.
  Post_Hoc_Tabelle <- Post_Hoc_Tabelle %>% mutate_if(is.numeric, round, digits=3)
  # Hier konvertiere ich alle Spalten in Chars, das gibt weniger SChwierigkeiten beim Export.
  Post_Hoc_Tabelle <- Post_Hoc_Tabelle %>% mutate_if(is.numeric,as.character)


  # Hier wird die Post-Hoc-Tabelle jetzt in Word gebaut und gespeichert.
  Post_Hoc_Tabelle %>%
    rtf_title(title = Tabellenueberschrift_posthoctabelle, text_format = c("i"), text_justification = c("l"))%>%
    rtf_footnote(cell_justification = c("l"),footnote = Tabellentext_posthoctabelle, text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", text_justification = c("l")) %>%
    rtf_body(cell_justification = c("l"), as_colheader = FALSE,  col_rel_width = Tabellen_Spaltenzahl,  text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", border_last = "single", border_first = "single") %>%
    rtf_colheader(cell_justification = c("l"), col_rel_width = Tabellen_Spaltenzahl, colheader = Spaltennamen_Word_final , text_font_size = 12 , border_bottom = "single", border_left = "", border_right = "", border_top = "", ) %>%
    rtf_page(orientation = "landscape", border_last = "", border_first = "single") %>%
    rtf_encode() %>%
    write_rtf("Word_Tabelle_Chi_Quadrat_Posthoc.rtf")


  Hinweis_Post_Hoc_Tabelle <- c("Die Tabelle  mit den Ergebnissen der Post-hoc-Tests finden Sie unter dem Namen Word_Tabelle_Chi_Quadrat_Posthoc.rtf in Ihrem Working-Directory. Sie sollten die Post-Hoc-Tests nur interpretieren, wenn der Chi-Quadrat-Test signifikant geworden ist.")



  return(list(result,  Hinweis_Post_Hoc_Tabelle))


}

