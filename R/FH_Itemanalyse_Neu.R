#' Eine Funktion, die bestehende R-Pakete zusammenschnürt um eine Itemanalyse als Word-Tabelle auszugeben und den Konstruktmittelwert zu errechnen.
#' @param data als (data.frame), voreingestellt ist "dataset". Hier muss der Datensatz angegeben werden.
#' @param variables als Charakter-Vektor mit der Länge >=2. Voreingestellt ist "Konstrukt_Items". Hier muessen alle Variablen angegeben werden, mit denen die Itemanalyse durchgeführt werden soll.
#' @param min_x als Zahl. Hier muss die minimal moegliche Itemantwort angeben. Bei einer Skala von 1-5 ist das die 1.
#' @param max_x als Zahl. Hier muss die maximal moegliche Itemantwort angeben. Bei einer Skala von 1-5 ist das die 5.
#' @param tabnumber  als Zahl. Voreingestellt ist "Tabellennummer". Das ist die Tabellennummer, die dann in der Tabellenueberschrift der Itemanalyse steht.
#' @param construct_name als Charakter-Vektor mit der Länge 1. Voreingestellt ist "Konstrukt_X". Hier den Namen des Konstruktes angeben, fuer das die Itemanalyse durchgeführt werden soll.
#' @return Hauptsaechlich wird eine Word-Datei erstellt. Die Word-Datei "Word_Tabelle_Itemanalyse.rtf" beinhaltet die Ergebnisse der Itemanalyse. Wenn der Befehl als Objekt gespeichert wird, also bspw. dataset <- FH_Itemanalyse (), dann befindet sich in diesem Datensatzes zusätzlich eine neue Variable, und zwar der Konstruktmittelwert.
#' @import dplyr
#' @import knitr
#' @import kableExtra
#' @import expss
#' @import difNLR
#' @import r2rtf
#' @import ShinyItemAnalysis
#' @import sjmisc
#' @import data.table
#' @import psych
#' @export

FH_Itemanalyse <- function(data = dataset, variables=Konstrukt_Items, min_x , max_x,  tabnumber = Tabellennummer, construct_name = "Konstrukt_X"){

  subdataset <- as.data.frame(subset(data, select = variables)) # Alle Variablen, ueber die die Itemanalyse durchgefuehrt werden soll, werden jetzt in einem Subdatensatz gespeichert



  ###### Berechnung der Itemanalyse f?r das Konstrukt Lernkultur#######


  #hier berechne ich den Cronbach's alpha-Wert f?r die gesamte Skala
  Alpha_Wert <- psych::alpha(subdataset, use = "complete.obs") # Hier gehen nur die TEilnehmer ein, die ALLE Items des Konstrukt beantwortet haben
  Alpha_Wert <- Alpha_Wert$total$raw_alpha
  Alpha_Wert <- round(Alpha_Wert, digits = 2)
  # Dieser Wert steht jetzt im "Objekt" Alpha_Wert


  # Berechnung der Trennsch?rfe und Itemschwierigkeit
  Itemanalyse_Konstrukt <- Copy_Shiny_Itemanalysis(subdataset, maxscore = max_x, minscore = min_x )
  Itemanalyse_Konstrukt <-select(Itemanalyse_Konstrukt, "Difficulty", "Mean", "SD", "RIR", "Alpha.drop")
  # Es gibt jetzt das Objekt Itemanalyse_Konstrukt, in dem alle relevanten Daten stehen


  ##### Ausgabe der Daten in einer formatierten Tabelle

  Spaltennamen_Itemanalyse <- c("p", "M", "SD", "r_it", "alpha_OhneItem") # Hier werden die Spalten der Tabelle umbenannt. Das muss eigentlich nicht ge?ndert werden


  Text_Cronbachs_alpha_Wert <- paste("Cronbach's alpha der Gesamtskala ", construct_name, " ist ",Alpha_Wert, '. "p" = Itemschwierigkeit, "M" = Mittelwert, "SD" = Standardabweichung, "rit" = Trennschaerfe, "alphaOhneItem" = Cronbachs alpha Wert der Rest-Skala, wenn das jeweilige Item geloescht wird.', sep="") # Hier wird der Text, der unter der Tabelle steht generiert.
  Tabellenueberschrift <- paste("Tabelle", tabnumber, ": Itemanalyse des Konstrukts", construct_name ) # Hier wird der Text, der unter der Tabelle steht generiert.

  # Hier wird die Tabelle gebaut.
 result <- Itemanalyse_Konstrukt %>%
    kbl(col.names = Spaltennamen_Itemanalyse, align = "c", digits = 2, caption = Tabellenueberschrift) %>%
    kable_classic_2(full_width = F, html_font = "Cambria") %>%
    footnote(general = Text_Cronbachs_alpha_Wert
    )

  # Die Tabelle kann jetzt so exportiert und verwendet werden.





  # Hier wird jetzt noch eine Tabelle erzeugt, die direkt als Word-Dokument vorliegt.

  # Hier weren jetzt noch Tabellen?berschrift, Tabellentext, und die Spaltennamen f?r die Word-Tabelle erzeugt.

  Spaltennamen_Word  <- c("Variablenname", Spaltennamen_Itemanalyse)
  Spaltennamen_Word_Zwischenschritt <- paste(Spaltennamen_Word[1:length(Spaltennamen_Word)-1], "|")
  Spaltennamen_Word_Zwischenschritt_2 <- c(Spaltennamen_Word_Zwischenschritt, Spaltennamen_Word [length(Spaltennamen_Word)], collapse = " ")
  Spaltennamen_Word_final <- paste(Spaltennamen_Word_Zwischenschritt_2, collapse = " ")

  # Hier runde ich alle Variablen in der Tabelle

  Itemanalyse_Konstrukt <- round(Itemanalyse_Konstrukt, digits = 2)

  # Hier f?ge ich jetzt die Zeilennamen, in denen die Variablennamen drinstehen, noch als extra Spalte hinzu.
  Itemanalyse_Konstrukt$Itemname <- as.character(row.names(Itemanalyse_Konstrukt))
  # Jetzt kommt die letzte Spalte noch nach ganz vorne
  Itemanalyse_Konstrukt <- Itemanalyse_Konstrukt[c(6,1,2,3,4,5)]


  # Hier wird die Itemanalyse-Tabelle jetzt in Word gebaut und gespeichert.
  Itemanalyse_Konstrukt %>%
    rtf_title(title = Tabellenueberschrift, text_format = c("i"), text_justification = c("l"))%>%
    rtf_footnote(cell_justification = c("l"),footnote = Text_Cronbachs_alpha_Wert, text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", text_justification = c("l")) %>%
    rtf_body(cell_justification = c("l"), as_colheader = FALSE,  col_rel_width = c(3,1,1,1,1,1),  text_font_size = 12, border_bottom = "", border_left = "", border_right = "", border_top = "", border_last = "single", border_first = "single") %>%
    rtf_colheader(cell_justification = c("l"), col_rel_width = c(3,1,1,1,1,1), colheader = Spaltennamen_Word_final , text_font_size = 12 , border_bottom = "single", border_left = "", border_right = "", border_top = "", ) %>%
    rtf_page(orientation = "landscape", border_last = "", border_first = "single") %>%
    rtf_encode() %>%
    write_rtf("Word_Tabelle_Itemanalyse.rtf")


Hinweis_Itemanalyse <- c("Die Tabelle mit den Ergebnissen der multiplen Regression finden Sie unter dem Namen Word_Tabelle_Itemanalyse.rtf in Ihrem Working-Directory. Diese können Sie in Word uebernehmen und dort ggf. anpassen.")




  ######### Automatisches bilden des Konstruktmittelwerts ##############
  Hilfsvariable <- paste (colnames(subdataset) , collapse= " ")
   data[[paste0("Konstrukt_",construct_name)]] <- rowMeans(subdataset, na.rm = TRUE) %>%
    `var_lab<-`(paste("Mittelwert der Items:", Hilfsvariable) )

  # Hier wird im original-Datensatz (data) eine neue Variable angelegt, die immer "Konstrukt_Konstruktname" hei?t.
  # In unserem Beispiel also "Konstrukt_Lernkultur"
  # Diese neue Variable setzt sich immer aus den Items zusammen, die im Subdatensatz "subdataset" aufgef?hrt sind.
  # d.h. wenn man mehrere Itemanalysen hintereinander macht, sollte man beachten, dass die vorherigen Werte ?berschrieben werden,
  # wenn man den Konstruktnamen beibeh?lt.
  # Zus?tzlich wird ein Variablenlabel geschrieben, dass beinhaltet aus welchen Items der Konstruktmittelwert gebildet wird.

 Hinweis_Konstruktmittelwert <- c("Wenn der Befehl als Objekt gespeichert wird, also bspw. dataset <- FH_Itemanalyse (), dann befindet sich in diesem Datensatzes zusätzlich eine neue Variable, und zwar der Konstruktmittelwert.")




#print(Hinweis_Itemanalyse)
#print(Hinweis_Konstruktmittelwert)
#print(result)

   #return(data)
  return(list(result,  Hinweis_Itemanalyse, Hinweis_Konstruktmittelwert, data))





}

