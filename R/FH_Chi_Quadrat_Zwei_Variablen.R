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


FH_Chi_Quadrat_Zwei_Variablen <- function(data = dataset, dv=AV, iv= UV,  tabnumber = Tabellennummer){


  Verwendete_Variablen <- vctrs::vec_c(dv, iv) # Hier bilde ich einen neuen Vektor, in dem erst die AV, dann die UV steht

  Dataset_Verwendete_Variablen <- paste("dataset$",Verwendete_Variablen, sep="", collapse = ",")

  subdataset <- as.data.frame(subset(data, select = Verwendete_Variablen)) #  AV und UV werden jetzt in einem Subdatensatz gespeichert


  Crosstable_Befehl <- paste("cro(", Dataset_Verwendete_Variablen, ")", sep="")

  Tabellen_Befehl <- eval(parse(text= paste("table(",Dataset_Verwendete_Variablen,")",sep="")))

  Tabellenueberschrift <- paste("Tabelle ", tabnumber, ": Kontingenztabelle der Variablen ", Verwendete_Variablen [1] , " und ", Verwendete_Variablen [2], ".", sep = ""  ) # Hier wird der Text, der unter der Tabelle steht generiert.


  ## Berechnen des Chi-Quadrat-Tests
  Chi_Quadrat_Test <- summary(Tabellen_Befehl)

  ## Berechnen von Cramers V
  Cramers_V <-  cramersV(Tabellen_Befehl)

  ## Grobinterpretation von Cramers V

  if (Cramers_V < 0.3) {Cramer_Effekt <- c("kleinen")
  } else if (Cramers_V > 0.5) {Cramer_Effekt <- c("gro?en")
  } else {Cramer_Effekt <- c("mittleren")}

  Tabellentext <- paste("Chi-Quadrat(",Chi_Quadrat_Test$parameter, ", n = ", Chi_Quadrat_Test$n.cases, ")", " = ", round(Chi_Quadrat_Test$statistic, digits = 2), ", p =" , round(Chi_Quadrat_Test$p.value, digits = 3), ". Cramer's V betraegt ", round(Cramers_V, digits = 2), ". Das entspricht nach Cohen (1988) einem " , Cramer_Effekt, " Effekt.", sep="")


  ########Erstellen einer Kontinzentabelle


  Vektor_Label_1 <-as.vector (get_labels(subdataset[1]))
  Vektor_Label_1b <- unlist(Vektor_Label_1, use.names = FALSE)
  Zeilennamen <- c(Vektor_Label_1b)


  Vektor_Label_2 <-as.vector (get_labels(subdataset[2]))
  Vektor_Label_2b <- unlist(Vektor_Label_2, use.names = FALSE)
  Spaltennamen <- c(Vektor_Label_2b)

  Kreuztabelle <- eval(parse(text= Crosstable_Befehl))

  rownames(Kreuztabelle) <- c(Zeilennamen, "Total")

  Kreuztabelle <- Kreuztabelle [,-1]


  result <-Kreuztabelle %>%
    kbl(  col.names = Spaltennamen,  align = "c", digits = 2, caption = Tabellenueberschrift ) %>%
    kable_classic_2(full_width = F, html_font = "Cambria") %>%
    footnote(general = Tabellentext
    )


  # Hier wird jetzt noch eine Tabelle erzeugt, die direkt als Word-Dokument vorliegt.

  # Hier weren jetzt noch Tabellenueberschrift, Tabellentext, und die Spaltennamen fuer die Word-Tabelle erzeugt.

  Spaltennamen_Word  <- c(Verwendete_Variablen[1], Spaltennamen)
  Spaltennamen_Word_Zwischenschritt <- paste(Spaltennamen_Word[1:length(Spaltennamen_Word)-1], "|")
  Spaltennamen_Word_Zwischenschritt_2 <- c(Spaltennamen_Word_Zwischenschritt, Spaltennamen_Word [length(Spaltennamen_Word)], collapse = " ")
  Spaltennamen_Word_final <- paste(Spaltennamen_Word_Zwischenschritt_2, collapse = " ")

  # Hier fuege ich jetzt die Zeilennamen, in denen die Variablennamen drinstehen, noch als extra Spalte hinzu.
  Kreuztabelle$Kategorie <- as.character(row.names(Kreuztabelle))
  # Jetzt kommt die letzte Spalte noch nach ganz vorne

  Kreuztabelle <- as.data.frame(Kreuztabelle)


  Kreuztabelle <- Kreuztabelle %>% select(Kategorie, everything())


  # Hier brauche ich einen Vektor, der Anzeigt, wie viele Spalten meine letztendliche Tabelle hat.
  Tabellen_Spaltenzahl<- c(rep(1,length(Spaltennamen_Word)))

  # Hier muss ich jetzt festlegen, wie viel breiter die erste Spalte sein soll. ICh sage mal 3x so breit
  Tabellen_Spaltenzahl[1] <- 3


  # Hier wird die Post-Hoc-Tabelle jetzt in Word gebaut und gespeichert.
  Kreuztabelle %>%
    rtf_title(title = Tabellenueberschrift, text_format = c("i"), text_justification = c("l"))%>%
    rtf_footnote(cell_justification = c("l"),footnote = Tabellentext, text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", text_justification = c("l")) %>%
    rtf_body(cell_justification = c("l"), as_colheader = FALSE,  col_rel_width = Tabellen_Spaltenzahl,  text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", border_last = "single", border_first = "single") %>%
    rtf_colheader(cell_justification = c("l"), col_rel_width = Tabellen_Spaltenzahl, colheader = Spaltennamen_Word_final , text_font_size = 12 , border_bottom = "single", border_left = "", border_right = "", border_top = "", ) %>%
    rtf_page(orientation = "landscape", border_last = "", border_first = "single") %>%
    rtf_encode() %>%
    write_rtf("Word_Tabelle_Haeufigkeitstabelle.rtf")


  Hinweis_Haeufingkeitstabelle <- c("Die Hauefingkeitstabelle mit dem Chi-Quadrat-Test finden Sie unter dem Namen Word_Tabelle_Haeufigkeitstabelle.rtf in Ihrem Working-Directory.")






  return(list(result,  Hinweis_Haeufingkeitstabelle))


}
