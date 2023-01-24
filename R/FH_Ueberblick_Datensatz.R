#' Eine Funktion, die bestehende R-Pakete zusammenschnuert um einen Ueberblick ueber einen geladenen .sav-Datensatz zu geben.
#' @param data als (data.frame), voreingestellt ist "dataset". Hier muss der Datensatz angegeben werden.
#' @return in Viewer wird ein Überblick über den Datensatz angezeigt.
#' @import dplyr
#' @import knitr
#' @import kableExtra
#' @import expss
#' @export

FH_Ueberblick_Datensatz_sav <- function(data = dataset){


  ############# Ueberblick ueber Datensatz ##########

  data <-as.data.frame(data)  # Sicherheitshalber wird der Datensatz noch einmal in ein data.frame transformiert.


  #Ueberblick ueber alle Variablen im Datensatz, dies dient dazu, um einmal zu schauen, ob man alles hat.
  Ueberblick_Datensatz <- info(data)
  Ueberblick_Datensatz <- Ueberblick_Datensatz [,-2]
  t_Ueberblick_Datensatz <- transpose(Ueberblick_Datensatz)
  colnames(t_Ueberblick_Datensatz) <- rownames(Ueberblick_Datensatz)
  rownames(t_Ueberblick_Datensatz) <- colnames(Ueberblick_Datensatz)

  result <- Ueberblick_Datensatz %>%
    kbl() %>%
    kable_styling()
  # Hier bekommt man eine ganz nette Ueberischt ueber alle Variablen im Datensatz, sogar mit minimaler deskriptiver Statistik


  # Auflistung aller Variablennamen im Datensatz. Das macht es ggf. einfacher zu sehen, wie die Variablen heissen, die in die Itemanalyse aufgenommen werden sollen.
  Variablennamen <- variable.names(data)

print(Variablennamen)
  return(result)

  # return(list(result, Variablennamen))


}


