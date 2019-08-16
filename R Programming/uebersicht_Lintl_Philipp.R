
uebersicht <- function(daten){
  # Falls Eingabeparameter kein data.frame, soll die Funktion abbrechen.
  stopifnot(is.data.frame(daten))
  # Speichert Klasse, Anzahl Beobachtungen, Anzahl Variablen, Anzahl NA'S, 
  objectclass    <- class(daten)
  count_observ   <- nrow(daten)
  count_var      <- ncol(daten)
  count_na       <- sum(sapply(daten, function(y) sum(is.na(y))))
  # Anzahl aller Werte (alle Felder des data.frames)
  overall_values <- count_observ*count_var
  # Speichert gerundeten Anteil der NA's an overall_values auf 2 Stellen gerundet
  prop_na        <- signif(count_na/overall_values,digits = 2)
  # paste ermoeglicht bessere Ausgabe als sprintf
  s1 <- "Das Objekt gehoert der Klasse"
  s2 <- "an und beinhaltet"
  s3 <- "Beobachtungen von"
  s4 <- "Variablen."
  s5 <- "Prozent der Eintraege sind fehlende Werte"
  ausgabe <- paste(s1,objectclass,s2,count_observ,s3,count_var,s4,prop_na*100,s5)
  # Funktion fuer numerische Plots
  numeric_plot <- function(num_var,var_name){
    boxplot(num_var,main=var_name)
  }
  # Funtion fuer Faktorplots
  factor_plot <- function(factor_var,var_name){
    # 'table', da Haeufigkeiten gebraucht werden, cex.names wegen der Uebersicht
    # lichkeit
    barplot(table(factor_var),cex.names = 0.4,main=var_name)
  }
  # Schleife, die durch par(ask=TRUE) count_var viele Plots erstellt
  for(i in 1:count_var){
    # Uebersichtlicher als im kleinen Plotfenster
    x11()
    par(ask=TRUE)
    # Nimmt Variable, prueft die Klasse und nimmt Variablennamen fuer Plottitel
    if((class(daten[,i]))=="numeric"){
      # Abrufen der Hilfsfunktion fuer numerische Variablen
      numeric_plot(daten[,i],variable.names(daten)[i])
    }else if((class(daten[,i]))=="factor"){
      # Abrufen der Hilfsfunktion fuer Faktor-Variablen
      factor_plot(daten[,i],variable.names(daten)[i])
    }
    i=i+1
  }
  # Ausgabe ist oben erstelltes Objekt 'ausgabe'
  return(ausgabe) 
}