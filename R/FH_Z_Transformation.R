#' Eine Funktion, die bestehende R-Pakete zusammenschnürt um Variablen zu z-Transformieren
#' @param data als (data.frame), voreingestellt ist "dataset". Hier muss der Datensatz angegeben werden.
#' @param variables als Charakter-Vektor mit der Länge  >= 1. Voreingestellt ist "Zu_Transformieren". Hier muessen die Variablen angegeben werden, die z-transformiert werden sollen.
#' @return z-transformierte Variablen. Der Befehl muss als Objekt gespeichert werden, also bspw. dataset <- FH_Z_Transformation (). Die z-transformierte(n) Variable(n) befinden sich gemeinsam mit allen anderen Variablen Ihres Datensatzes im neu angelegten Objekt (neues oder ueberschriebenes data.frame).
#' @import dplyr
#' @export

FH_Z_Transformation <- function(data = dataset, variables=Zu_Transformieren ){

  subdataset <- data

  Z_Transformationsbefehl <- paste("subdataset$Z", variables , "<- scale(subdataset$" ,  variables , ")", sep ="")
  eval(parse(text= Z_Transformationsbefehl))


  Hinweis_ZTransformation <- c("Die z-transformierte(n) Variable(n) befinden sich gemeinsam mit allen anderen Variablen Ihres Datensatzes im neu angelegten Objekt (neues oder ueberschriebenes data.frame).")


  return(subdataset)


}

