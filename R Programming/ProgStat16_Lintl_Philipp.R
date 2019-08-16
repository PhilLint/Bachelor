# Haus?bung Programmieren mit Statistischer Software - SS 2016
# Moritz Berger, Eva Endres


#----------------------------------------------------------------
# Nachname:Lintl
# Vorname: Philipp
# Matrikelnummer: 11358321
# Studiengang: Statistik
# Pr?fungsordnung: Bachelor 2010


#----------------------------------------------------------------
# Aufgabe 1
#a) Einlesen
#rm(list=ls())
# Persoenlicher Pfad
setwd("C:\\Users\\lintl\\Documents\\GitHub\\Bachelor\\R Programming")
#getwd()

library(foreign)
allbus <- read.spss("ZA5240_v2-1-0.sav", to.data.frame = TRUE ,
                    use.value.labels = TRUE)
#b)
# Subset mit ausgewaehlten Spalten/Variablen
teil.allbus <- subset(allbus,select = c(V14,V17,V19,V27,V225,
                                        V275,V277))
# Kure Uebersicht mit Hilfe der ersten 10 Zeilen
head(teil.allbus)
# Namenzuweisungen englisch
names(teil.allbus) <- c("INT_USAGE","LAZY","YOGA","SPORT","HEALTH"
                        ,"HEIGHT","WEIGHT")

#c)
# Variablenueberblick inklusive Variablentyp
str(teil.allbus)
rownumber <- nrow(teil.allbus) # Anzahl Beobachtungen = 3471 
rownumber
colnumber <- ncol(teil.allbus) # Anzahl Variablen = 7 (aus b)
colnumber
#(FaktorVariablen), 5-P Zusammenfassung (Nummerische Variablen),
#Anzahlen NA's
summary(teil.allbus)

#d)
# 5 Auspr?gungen
aus_yoga <- nlevels(teil.allbus$YOGA) 
aus_yoga
# Auspaegungen: Sehr gut, Gut, Zufriedenstellend, Weniger Gut, Schlecht
aus_health <- levels(teil.allbus$HEALTH)
aus_health
#e)
# sapply fuehrt sum(is.na()) (-> Summe aller NA'S einer Variable) fuer alle 
# Variablen von data= teil.allbus aus.
na_count <- sapply(teil.allbus, function(y) sum(is.na(y)))
# Uebersichtlicher, wenn als data.frame, also vertikal angeordnet dargestellt.
na_count <- as.data.frame(na_count)
na_count
#f)
# Speichert alle Groe?en in Meter
m <- teil.allbus$HEIGHT / 100
# Speichert alle Einzelgewichte
kg <- teil.allbus$WEIGHT
# Wendet BMI Formel auf beiden soeben erstellten Vektoren an und fuegt neue
# Variable BMI an Teildatensatz teil.allbus an.
teil.allbus$BMI <- kg / (m * m)

#g)
# Bildet einen Teildatensatz aus Reihen, in denen BMI!=NA, es werden also alle
# NA's ausgelassen.
# Neue Laenge: 3419 -> 52 NA's wurden entfernt
teil.allbus <- subset(teil.allbus, subset = !is.na(BMI))
nrow(teil.allbus)
# 5-P Zusammenfassung von BMI mitenthalten.
summary(teil.allbus)

#h)
# Laden notwendig, sonst Funktionen aus ggplot nicht ausfuehrbar
library(ggplot2)
# Bessere Darstellung in gro?em Fenster
x11()
# Boxplot, varwidth=TRUE regelt die Breite der Boxen proportional (Wurzel) zur 
# Anzahl an Beobachtungen.
ggplot(teil.allbus) +
  aes(x = SPORT,y = BMI,colour = SPORT) +
  geom_boxplot(varwidth = TRUE)

#i)
# Teildatensatz aus teil.allbus mit HEALTH=="SEHR GUT" und SPORT=="TAEGLICH"
subset_i <- subset(teil.allbus,subset = (HEALTH == "SEHR GUT") &
                     (SPORT == "TAEGLICH"))
# 5-P Zusammenfassung aus Uebersichtsgruenden.
summary(subset_i)
# Speichert Mittelwert des Gewichts aus Teildatensatz subset_i 
average_weight <- mean(subset_i$WEIGHT)
average_weight

#j)
# apply angewendet auf die ersten 4 Spalten ([,1:4]) von teil.allbus
# MARGIN=2: Funktion wird auf Spalten angewendet, FUN=table: absoluten Haeufig
# leiten der einzelnen Variablen wir als Kreuztabelle abgespeichert.
kreuztabelle <- apply(teil.allbus[,1:4],2,FUN = table)
kreuztabelle
#k)
# Um Ueberpruefen zu koennen, ob erstellte Datei wirklich in demselben Ordner 
# abgespeichert wird.
# file.create("uebersicht_Lintl_Philipp.R")
# nicht noch einmal aufrufen, da sonst meine Syntaxdatei ueberschrieben wird,
# nur zur Dokumentation, um zu zeigen, wie ich die Datei erstellt habe.
# Veraendern der Datei
# file.edit("uebersicht_Lintl_Philipp.R")
#l)
# siehe uebersicht_Lintl_Philipp.R
#m)
# Sucht in aktuellem Verzeichnis, in dem auch ProgStat16_Lintl_Philipp.R liegt, 
# nach "uebersicht_Lintl_Philipp.R" und hat durch 'source' Befehl Zugriff auf
# die Funktion uebersicht(), die anschlie?end fuer teil.allbus ausgefuehrt wird
source("uebersicht_Lintl_Philipp.R")
uebersicht(teil.allbus)
#----------------------------------------------------------------
# Aufgabe 2
#a)rm(list=ls())
analyze_text <- function(text) {
  # Entfernt Leerzeichen, Zeilenspruenge und speichert dies in einem String
  string_only_cha <- gsub("[[:space:]]","",text)
  # Anzahl Character
  num_char <- nchar(string_only_cha)
  # Entfernt alle Zeichen, die nicht Buchstaben sind
  string_only_letters <-
    gsub("[[:space:],[:punct:],[:digit:]]","",text)
  # Anzahl Buchstaben
  num_letters <- nchar(string_only_letters)
  # Entfernt alle Zeichen, die nicht Ziffern sind
  string_only_digits <- gsub("[^[:digit:]]","",text)
  # Anzahl Ziffern 
  num_digits <- nchar(string_only_digits)
  # Entfernt alle Zeichen, die nicht Satzzeichen sind
  string_only_signs <- gsub("[^[:punct:]]","",text)
  num_signs <- nchar(string_only_signs)
  # Macht aus einem durchgehenden String einzelne Charobjekte, die man dann 
  # auf Haeufigkeiten untersuchen kann.
  string_lone_letters <- strsplit(string_only_letters,"")
  # Speichert Anzahl Buchstaben sinkend in einer Tabelle (data.frame)
  if(lengths(string_lone_letters)==0){
    table_decreasing_letters <- "Im Text kommen keine Buchstaben vor"  
  }else{
    table_decreasing_letters <-
      as.data.frame(sort(table(string_lone_letters),decreasing = TRUE))
  }
  # Analog mit Satzzeichen und Ziffern
  string_lone_signs <- strsplit(string_only_signs,"")
  if(lengths(string_lone_signs)==0){
    table_decreasing_signs <- "Im Text kommen keine Satzzeichen vor"
  }else{
    table_decreasing_signs <-
      as.data.frame(sort(table(string_lone_signs),decreasing = TRUE))
  }
  string_lone_digits <- strsplit(string_only_digits,"")
  if(lengths(string_lone_digits)==0){
    table_decreasing_digits <-"Im Text kommen keine Zahlen vor"
  }else{
    table_decreasing_digits <-
      as.data.frame(sort(table(string_lone_digits),decreasing = TRUE))
  }
  # Auszugebende List aus oben erstellten Objekten
  final_output_list <- list(
    "Gesamtzahl Zeichen" = num_char,
    "Gesamtzahl Buchstaben" = num_letters,
    "Gesamtzahl Ziffern" = num_digits,
    "Gesamtzahl Satzzeichen" = num_signs,
    "Tabelle vorkommender Buchstaben absteigend nach Haeufigkeit" =
      table_decreasing_letters,
    "Tabelle vorkommender Satzzeichen absteigend nach Haeufigkeit" =
      table_decreasing_signs,
    "Tabelle vorkommender Ziffern absteigend nach Haeufigkeiten" =
      table_decreasing_digits
  )
  # Fuegt der urspruenglichen Klasse (list) eine zusaetzliche, die wie in der
  # Angabe gewuenscht, textAnalyze hei?t.
  class(final_output_list) <- c(class(final_output_list),"textAnalyze")
  final_output_list
}
#b)
# Eingabeparameter ist der zu analysierende Text
print.textAnalyze <- function(text_print) {
  # Speichert Ergebnis der Funktion analyze_text() (Liste)
  final_list <- analyze_text(text_print)
  # Printet alle Listenelemente nacheinander
  for(i in 1:length(final_list)){
    print(final_list[i])
  }
}
#c)
# Kopie der Texte aus der Angabe
text_a <-
  "Ich wusste wohl, mein Brutus, dass, als ich das, was die geistreichsten und
gelehrtesten Philosophen in griechischer Sprache behandelt hatten, in lateinischer
wiedergab, meine Arbeit mancherlei Tadel finden wurde. Denn manchen und nicht gerade
ungelehrten Mannern gefallt das Philosophiren uberhaupt nicht; andere wollen eine
massige Tatigkeit hier wohl gestatten, aber meinen, dass man nicht so grossen Fleiss
und so viele Muhe darauf verwenden durfe. Auch giebt es Manner, die, mit den Schriften
der Griechen vertraut, die lateinischen verachten und sagen, dass sie ihre Muhe
lieber auf jene verwenden mogen. Endlich werden auch Einige mich vermuthlich an andere
Wissenschaften verweisen, weil diese Art von Schriftstellerei, trotz des Scharfsinns,
doch nach ihrer Meinung meiner Person und Wurde nicht gezieme."

text_b <-
  "D1353 M1TT31LUNG Z31GT D1R, ZU W3LCH3N GRO554RT1G3N L315TUNG3N UN53R G3H1RN F43H1G
15T! 4M 4NF4NG W4R 35 51CH3R NOCH 5CHW3R, D45 ZU L353N, 483R M1TTL3W31L3 K4NN5T DU
D45 W4HR5CH31NL1ICH 5CHON G4NZ GUT L353N, OHN3 D455 35 D1CH W1RKL1CH 4N5TR3NGT. D45
L315T3T D31N G3H1RN M1T 531N3R 3NORM3N L3RNF43HIGKEIT. 8331NDRUCK3ND, OD3R?"

# Ausfuehren der Print.Methode fuer Text a 
print.textAnalyze(text_a)

# Ausfuehren der Print.Methode fuer Text b
print.textAnalyze(text_b)
#----------------------------------------------------------------
# Aufgabe 3
# Datensatzgenerierung mit n Beobachtungen, aus multivariater 
# Normalverteilung (dim r),mit Mittelwertsvektor mu
# und Kovarianzmatrix Sigma
# Output: Matrix dim n X r <- 
## Aufgabe 3 (a)
# Notwendig, da sonst nicht mit multivariaten Normalverteilungen umgegangen
# werden kann.
#install.packages("mvtnorm")
library(mvtnorm)

simulation.data <- function(n,mu,Sigma){
  # length(mu)==1 testet, ob eingegebener Erwartungswert ein Vektor der Laenge 1
  # es also eine Normalverteilung einer Variable betrifft, oder einer Multivariaten
  # Normalverteilung betrifft. Ansonsten wuerde Aufgabe f) nicht funktionieren,
  # weil es sich hierbei nicht um eine multivariate NNormalverteilung handelt.
  if((length(mu)==1)&&(is.data.frame(Sigma)==FALSE)){
    simulated_matrix <- as.data.frame(rnorm(n,mu,Sigma))
  }
  else {
    simulated_matrix <- rmvnorm(n,mu,Sigma)  
  }
  return(simulated_matrix)
}
## Aufgabe 3 (b)
# Erstellt f?r jede Spalte aus data, p-Werte mit Hilfe 
# eines zweiseitigen T-Tests (alpha) und vergleicht empirischen 
# mit wahrem Mittelwert
simulation.test <- function(data,mu,alpha){
  p_value <- NULL
  # Fuehrt einen zweiseitigen t-Test fuer die Spalten von data durch und speichert
  # den entstehenden p-Wert in dem Objekt p_value an der Stelle i also 1. Stelle 
  # fuer p-Wert des t-Tests auf die Daten der 1. Spalte.
  for(i in 1:ncol(data)){
    resulted_test <-t.test(data[,i],alternative="two.sided",
                           mu=mu[i],paired=FALSE,conf.level=1-alpha)
    p_value[i] <- resulted_test$p.value 
  }
  return(p_value)
}  
## Aufgabe 3 (c)
# Funktion, die Funktionen aus a),b) n_rep mal ausf?hrt!
simulation.repeat <- function(n.rep,n,mu,Sigma,alpha){
  # Erstellt eine 0er Matrix mit n_rep Zeilen und length(mu) Spalten, da es 
  # length(mu) Spalten gibt, fuer die jeweils n_rep p-Werte ausgerechnet
  # werden.
  p_value_matrix <- matrix(0,ncol=length(mu),nrow=n.rep)
  for(i in 1:n.rep){
    # Erstellt einen simulierten Datensatz beruhend auf n, mu, Sigma
    data <- simulation.data(n,mu,Sigma)
    # Fuehrt den Test durch und speichert in jeder Zeile die p-Werte, und zwar
    # genau so viele, wie es in data Spalten gibt, also fuer jede Spalte ein 
    # p-Wert.
    test_value <- simulation.test(data,mu,alpha)
    # Speichert ergebene p-Werte in einer Zeile ab, insgesamt wird dieser 
    # Vorgang n.rep mal durchgef?hrt.
    p_value_matrix[i,] <- test_value
  }
  return(p_value_matrix)
} 

## Aufgabe 3 (d)
# Funktion, die f?r p_value_matrix zeilenweise ?berpr?ft, ob mindestens ein 
# signifikantes Ergebnis vorliegt Output: Anteil der Testdurchl?ufe mit 
# mindestens einem sign 
simulation.result <- function(p.values,alpha){ 
  more_than_one_sign <- 0
  # Es werden in der aeu?eren For-Schleife die Zeilenschritte geregelt, in der 
  # inneren die Spaltenschritte. Insgesamt prueft diese Funktion, ob sich 
  # mindestens ein signifikanter p-Wert in der Zeile befindet (sign_counter>=1)
  for(i in 1:nrow(p.values)){
    sign_counter<-0
    for(j in 1:ncol(p.values)){
      if(p.values[i,j]<alpha){
        sign_counter<-sign_counter+1
      }
    }
    
    if(sign_counter>=1){
      # Ist dies wie hier der Fall, wird der aeu?ere Zaehler more_than_one_sign
      # um eins erh?rt, also die Anzahl an Testdurchlaeufen (Zeilen) mit mindes
      # tens einem signifikanten P-Wert (p-Wert kleiner als alpha)
      more_than_one_sign <- more_than_one_sign+1
    }
  }
  # Anteil der Testdurchlaeufe mit mindestens einem signifikanten Ergebnis an 
  # Anzahl aller Testurchlaeufe n.rep
  amount_sign <- more_than_one_sign/n.rep
  return(amount_sign)
}
## Aufgabe 3 (e)
simulation <- function(n.sim,n.rep,n,mu,Sigma,alpha){
  simulation_result <- NULL
  for(i in 1:n.sim){
    # Speichert das Ergebnis der Funktion simulation.repeat ab, um dieses dann
    # in simulation.result wieder aufrufen zu koennen.
    result_sim_repeat <- simulation.repeat(n.rep,n,mu,Sigma,alpha)
    # Je nachdem wie gro? n.sim ist, wird die Simulation dementsprechend oft
    # durchgefuehrt. 
    simulation_result[i] <-simulation.result(result_sim_repeat,alpha)
  }
  return(simulation_result)
}

## Aufgabe 3 (f)
Sigma_f <- 1
mu_f    <- 0
n.rep   <-200
n.sim   <- 100
n       <- 100
alpha   <- 0.05
# Zur Nachvollziehbarkeit als Seed meine Matrikelnummer gewaehlt. 
set.seed(11358321)
# Ausfuehren der Funktion simulation. Abspeichern im Objekt simulation_3_f
simulation_3_f <-simulation(n.sim,n.rep,n,mu_f,Sigma_f,alpha)
simulation_3_f
#install.packages("ggplot2")
#library(ggplot2)
# Histogramm der Anteile an signifikanten Simulationsergebnissen.
x11()
#par(mfrow=c(1,2))
boxplot(simulation_3_f,ylim=c(0.01,0.1),
        main="Anteile an signifikanten Testergebnissen je Simulation")
#hist(simulation_3_f, breaks=15,col="aquamarine",main = "H?ufigkeitshistogramm",freq = F, 
#xlab="",ylab="Dichte",xlim=c(0,0.1))
lines(density(simulation_3_f),col="darkorchid1",lty=1,lwd=2)
## Aufgabe 3 (g)
Sigma_g <- diag(nrow=5)
mu_g    <- c(0,0,0,0,0)
set.seed(11358321)
simulation_3_g <- simulation(n.sim,n.rep,n,mu_g,Sigma_g,alpha)
simulation_3_g
mean(simulation_3_g)

x11()
# Boxplot geeignet. 
boxplot(main="",simulation_3_g,ylim=c(0.15,0.35))
abline(h=mean(simulation_3_g),col="red",lty="dashed",lwd=2)
abline(h=0.226,col="darkgreen",lwd=2)
text(y=0.35,x=.7,"theoretischer Mittelwert=0.226",col="darkgreen",cex=.9)
mittelwert <- sprintf("simulierter Mittelwert=%.4f",mean(simulation_3_g))
text(y=0.32,x=.7,mittelwert,col="red",cex=.9)

# Die Simulation erstellt einen Datensatz, der auf Zufallsereignissen beruht.
# Dementsprechend ergeben sich je nach seed andere Datensaetze und somit 
# andere Testergebnisse. Au?erdem wird an vielen Stellen gerundet(?)
#----------------------------------------------------------------
# Aufgabe 4
# Idee: Zwei 12x12 Matrizen, die horizontal/vertikal angeben, welche Schiffe in 
# Setzrichtung noch Platz haben. Eine Funktion generiert dann zufaellig einen
# Startpunkt basierend auf den noch moeglichen Stellen. Wenn dies funktioniert,
# wird beginnend von diesem Punkt aus das Schiff nach rechts/unten (horizontal/
# vertikal) gelegt.
# rm(list=ls())
combat_zone <- function(n5,n4,n3,n2) {
  combat_field_horizontal <- matrix(0,ncol = 12,nrow = 12)
  combat_field_vertical   <- matrix(0,ncol = 12,nrow = 12)
  
  # ?u?ere Spalten/Reihen mit 1ern besetzt, um Rand zu markieren.
  combat_field_horizontal[1,]  <- 1
  combat_field_horizontal[12,] <- 1
  combat_field_horizontal[,1]  <- 1
  combat_field_horizontal[,12] <- 1
  
  combat_field_vertical[1,]  <- 1
  combat_field_vertical[12,] <- 1
  combat_field_vertical[,1]  <- 1
  combat_field_vertical[,12] <- 1
  
  # Schiffsl?ngen abgespeichert, um damit sp?ter arbeiten zu k?nnen.
  length_n5 <-5
  length_n4 <-4
  length_n3 <-3
  length_n2 <-2
  
  # Horizontalmatrix wird von rechs nach links befuelllt: Denn festgelgte Setz-
  # richtung ist von rechts nach links. Keine gr??ere Zaehlzahl als 5, da das 
  # groe?te Schiff auf 5 Felder gelegt wird.
  filling_c_f_horizontal <- function(combat_field_horizontal){
    # Ein Zaehler zaehlt die vorhandenen 0en, startend innerhalb des Randes 
    # und geht dann in 1er Schritten nach oben und befuellt die Matrix dement-
    # sprechend
    for(i in 2:11){
      counter_horizontal <- 0
      for(j in 2:11){
        if(combat_field_horizontal[i,j]==0){
          counter_horizontal <- counter_horizontal+1
          combat_field_horizontal[i,j] <- counter_horizontal
        }
        if(counter_horizontal>5){
          combat_field_horizontal[i,j] <- 5
        }
      }  
    }
    combat_field_horizontal
  }
  
  # Horizontalmatrix wird befuellt, Vertikalmatrix ist die trasponierte davon.
  combat_field_horizontal <-filling_c_f_horizontal(combat_field_horizontal)  
  combat_field_vertical   <-t(combat_field_horizontal)  
  
  # 'direction.random' generiert zufaellig eine Richtung, in der gelegt weren 
  # soll.
  direction.random <- function() {
    direction <- sample(2,1)
    if (direction == 1) {
      end_direction <- "vertical"
    } else if (direction == 2) {
      end_direction <- "horizontal"
    }
    end_direction
  }
  
  # Diese Funktion prueft, ob es ueberhaupt noch passende Felder fuer die zuvor
  # zufaellig ausgewaehlte Richtung und Laenge gibt. Wenn also num_nx==0, dann 
  # gibt es kein einziges passendes Feld mehr, dementsprechend ist das Legen
  # des Schiffs der Laenge length_nx nicht moeglich.
  positioning_possible <- function(length_nx,current_field_horizontal,
                                   current_field_vertical,direction_random){
    if(direction_random=="horizontal"){
      num_nx <-length(which((current_field_horizontal>=length_nx)&
                              (current_field_horizontal<6)))
    } else if(direction_random=="vertical"){
      num_nx <-length(which((current_field_vertical>=length_nx)&
                              (current_field_vertical<6)))
    }
    if(num_nx==0){
      result <- FALSE
    }else {
      result <- TRUE
    }
    result
  }
  
  # 'position_random_horizontal' w?hlt je nach eingegebener L?nge einen 
  # passenden Startpunkt aus. Output ist im Format 'c(i,j)', also ein Vektor mit
  # Zeilen und Spaltenangabe.
  position_random_horizontal <- function (length_nx,combat_field_horizontal) {
    num_nx <-NULL
    # Zaehlt Anzahl an Felder, die passen. Also mit einer Zahl gefuellt sind,
    # die kleiner als 6 und groe?er gleich der Schifflaenge sind.
    num_nx <-length(which((combat_field_horizontal>=length_nx)&
                            (combat_field_horizontal<6)))
    # Zieht eine Zufallszahl aus num_nx
    xth_nx <- sample(1:num_nx,1)
    counter_hor_nx <- 0
    # Sucht dieses Feld, indem ein Counter jedes passende Feld zaehlt und bei 
    # der gezogenen Zahl xth_nx stoppt und dessen Reihe und Spalte in Vektor
    # form ausgibt.
    for(i in 2:11){
      for(j in 2:11){
        if((combat_field_horizontal[i,j]>=length_nx)&
           (combat_field_horizontal[i,j]<6)){
          counter_hor_nx <- counter_hor_nx+1 
          if(counter_hor_nx==xth_nx){
            num_row <- i
            num_col <- j        
          }
        }
      }
    }
    ship_start_position <- c(num_row,num_col)
    ship_start_position
  }
  
  # Analog fuer ein vertikal zu legendes Feld
  position_random_vertical <- function(length_nx,combat_field_vertical){
    num_nx <- NULL
    num_nx <-length(which((combat_field_vertical>=length_nx)&(
      combat_field_vertical<6)))
    yth_nx <- sample(1:num_nx,1)
    counter_ver_nx <- 0
    for(i in 2:11){
      for(j in 2:11){
        if((combat_field_vertical[i,j]>=length_nx)&(
          combat_field_vertical[i,j]<6)){
          counter_ver_nx <- counter_ver_nx+1 
          if(counter_ver_nx==yth_nx){
            num_row <- i
            num_col <- j        
          }
        }
      }
    }     
    ship_start_position <- c(num_row,num_col)  
    ship_start_position
  }
  
  # Diese Funktion nimmt die zuvor zufaellig bestimmte Startposition und legt je
  # nach Richtung und Laenge des Schiffs, 0er 
  # For-Schleifen: 1) Legt 0er, die das Schiff markieren
  #                2) Legt 6er rund um das Schiff, die den Schiffsrand markieren
  # Je nach Richtung wird das Vertical/Horizontal Spielfeld ver?ndert
  set_ships_horizontal <- function(ship_start_position,length_nx,
                                   combat_field_horizontal){
    num_row <- ship_start_position[1]
    num_col <- ship_start_position[2]
    # Legt beginnend bei der Startposition (deshalb i in 0) 0er 
    # in horizontale Richtung bis length_nx-1, da das Schiff sonst um ein Feld
    # zu lang waere.
    for(i in 0:(length_nx-1)){
      combat_field_horizontal[num_row,num_col-i] <- 0
    }
    # 6er werden rund um das Schiff gelegt, also eine Reihe darueber und darunt
    # er, sowie an den Ecken. Damit kann man sp?ter garantieren, dass keine 
    # Schiffe aneinander sto?en oder anecken.
    for(i in -1:length_nx){
      combat_field_horizontal[num_row+1,num_col-i] <- 6
      combat_field_horizontal[num_row-1,num_col-i] <- 6 
    }
    combat_field_horizontal[num_row,num_col-length_nx] <- 6
    combat_field_horizontal[num_row,num_col+1] <- 6
    combat_field_horizontal
  }
  # Analoges gilt f?r die Vertikalmatrix, lediglich mit dem Unterschied, dass
  # nun in die Spalten gelegt wird und deshalb die Zaehler der For-Schleifen
  # in field[+i,], also dem Spaltenzugriff hinzugezaehlt wird.
  set_ships_vertical <- function(ship_start_position,length_nx,
                                 combat_field_vertical){
    num_row <- ship_start_position[1]
    num_col <- ship_start_position[2]
    for(i in 0:(length_nx-1)){
      combat_field_vertical[num_row-i,num_col] <- 0
    }
    for(i in -1:length_nx){
      combat_field_vertical[num_row-i,num_col+1] <- 6
      combat_field_vertical[num_row-i,num_col-1] <- 6 
    }
    combat_field_vertical[num_row-length_nx,num_col] <- 6
    combat_field_vertical[num_row+1,num_col] <- 6
    combat_field_vertical
  }
  
  colours_ship <- function(length_nx){
    if(length_nx==5){
      colours <- "cyan"
    }else if(length_nx==4){
      colours <- "blue"
    }else if(length_nx==3){
      colours <- "green"
    }else if(length_nx==2){
      colours <- "red"
    }
    colours
  }
  # Diese Funktion erstellt ein Plottfeld aehnlich zu dem aus der Angabe
  draw_combat_zone <- function(overall_ship_positions_vertical,
                               overall_ship_positions_horizontal,
                               overall_ship_lengths_vertical, 
                               overall_ship_lengths_horizontal){
    
    # Neuer Plot
    plot.new()
    # Limits festlegen, x- und y-Achse sollen sich im Nullpunkt schneiden
    # 'ylim': 'rev(range)' ermoeglicht es, die y-Achse zu spiegeln, und somit
    # oben mit der ersten Zeile zu beginnen. 
    plot.window(xlim=c(0,10), ylim=rev(range(c(0,10))), xaxs="i", yaxs="i")
    # Quadratische Felder
    par(pty="s")
    # Gestricheltes Raster hinzuf?gen (Angabe)
    grid(10,10)
    # Box um das Spielfeld zeichnen
    box()
    
    
    draw_all_ships_horizontal <- function(overall_ship_positions_horizontal,
                                         overall_ship_lengths_horizontal){
      # xpos = Spalte der Matrix
     for(i in 1:length(overall_ship_lengths_horizontal)){
      ship_start_position <- unlist(overall_ship_positions_horizontal[i])
      xpos <- ship_start_position[2]
      # ypos = Reihe der Matrix
      ypos <- ship_start_position[1]  
      length_nx <- overall_ship_lengths_horizontal[i]
      colour <- colours_ship(length_nx)
      for(k in 0:(length_nx-1)){
        # Fuegt das Schiff der Laenge length_nx in das Plottfeld hinzu.
        # xpos+k-1-.5: -1: Da in Koordinaten der Matrizen der Rand mit 
        # innbegriffen ist. -.5: Zentriert den Kreis im Feld
        symbols(xpos-k-1-.5,ypos-1-.5,circles=.175, add=TRUE,
                bg=colour, fg=NULL, inches=FALSE)
      }
      # Analog zu horizontal, nur For-Schleife hier auf ypos angewendet, da vertikal
      # gelegt wird und somit in der Reihe nach unten gegangen wird.
     }
      }
      draw_all_ships_vertical <- function(overall_ship_positions_vertical,
                                          overall_ship_lengths_vertical){
        
        for(i in 1:length(overall_ship_lengths_vertical)){
          ship_start_position <- unlist(overall_ship_positions_vertical[i])
          length_nx <- overall_ship_lengths_vertical[i]
          colour <- colours_ship(length_nx)
          xpos <- ship_start_position[2]
          ypos <- ship_start_position[1]
         for(k in 0:(length_nx-1)){
            symbols(xpos-1-.5,ypos-1-k-.5,circles=.175, add=TRUE, bg=colour,
                    fg=NULL, inches=FALSE)
        }
       }
      }
      
      draw_all_ships_horizontal(overall_ship_positions_horizontal,
                                overall_ship_lengths_horizontal)
      draw_all_ships_vertical(overall_ship_positions_vertical,
                                overall_ship_lengths_vertical)
      
      
    }
    
    # Nach Legen eines Schiffs muessen die Felderzahlen neu bestimmt werden, da 
    # nun manche Felder belegt sind und somit weniger Schiffe Platz haben.
    shift_numbers_horizontal <- function(combat_field_horizontal){
      # Sucht Reihe fuer Reihe ausgehend von rechts nach links nach 0en/1en/6en
      for(i in 2:11){
        counter_horizontal <-1
        for(j in 2:11){
          # Setzt den Zaehler bei einer 0 (Schiff) immer wieder auf 1
          if(combat_field_horizontal[i,j]==0){
            counter_horizontal <-1 
          }
          # Setzt den Zaehler bei einer 1 (Spielfeldrand) auf 1, somit kann hier 
          # noch kein Schiff gelegt werden, 
          if(combat_field_horizontal[i,j]==1){
            counter_horizontal <- 1
          }
          # Zaehler auf 0 gesetzt, da erst zwei Felder links vom Schiffsrand 
          # erneut ein 2er Schiff gelegt werden kann.
          if(combat_field_horizontal[i,j]==6){
            counter_horizontal <- 0
          }
          # Wenn aktuelles Feld keine 0/1/6, soll Zaehler erhoeht werden und 
          # Zaehlerwert ins Feld gelegt werden.
          if((combat_field_horizontal[i,j]!=1)&
             (combat_field_horizontal[i,j]!=0)&
             (combat_field_horizontal[i,j]!=6)){
            counter_horizontal <- counter_horizontal+1
            combat_field_horizontal[i,j] <- counter_horizontal
          }
          # Selbst wenn der Zaehler >5 wird, soll eine 5 ins Feld gelegt werden,
          # da keine groe?eren Schiffe vorhanden sind.
          if(counter_horizontal>5){
            combat_field_horizontal[i,j] <- 5
          }    
        }  
      }
      combat_field_horizontal
    }
    # Analog zu horizontal, nur combat_field_vertical[j,i], da man jetzt von unten
    # nach oben geht.
    shift_numbers_vertical <- function(combat_field_vertical){
      for(i in 2:11){
        counter_vertical <-1
        for(j in 2:11){
          if(combat_field_vertical[j,i]==0){
            counter_vertical <-1
          }
          if(combat_field_vertical[j,i]==1){
            counter_vertical <- 1
          }
          if(combat_field_vertical[j,i]==6){
            counter_vertical <- 0
          }
          if((combat_field_vertical[j,i]!=1)&
             (combat_field_vertical[j,i]!=0)&
             (combat_field_vertical[j,i]!=6)){
            counter_vertical <- counter_vertical+1
            combat_field_vertical[j,i] <- counter_vertical
          }
          if(counter_vertical>5){
            combat_field_vertical[j,i] <- 5
          }    
        }  
      }
      combat_field_vertical
    }
    # Nachdem ein vertikales Schiff gelegt wurde, muessen Zahlen in der Horizontal
    # matrix ebenfalls angepasst werden. Deshalb werden alle 6en/0en aus der 
    # Vertikalmatrix in die Horizontalmatrix mit Hilfe des Befehls 'replace'
    # ?bertragen. 
    transfer_zeroes_ones_horizontal <- function(combat_field_vertical,
                                                combat_field_horizontal){
      combat_field_horizontal<- replace(combat_field_horizontal,
                                        combat_field_vertical==6,6)  
      combat_field_horizontal<- replace(combat_field_horizontal,
                                        combat_field_vertical==0,0)  
      transferred_field <- combat_field_horizontal
      transferred_field
    }
    # Analog nach Legen eines horizontalen Schiffs, wird diese Funktion angewendet
    transfer_zeroes_ones_vertical <- function(combat_field_vertical,
                                              combat_field_horizontal){
      combat_field_vertical<- replace(combat_field_vertical,
                                      combat_field_horizontal==6,6)  
      combat_field_vertical<- replace(combat_field_vertical,
                                      combat_field_horizontal==0,0)  
      transferred_field <- combat_field_vertical
      transferred_field
    }
    
    # Fuehrt alle bisher eingefuehrten Hilfsfunktionen nach eingegebenen Paramet
    # ern aus.
    battleship_field <- function(combat_field_horizontal,combat_field_vertical,
                                 n5,n4,n3,n2){
      # Zaehlt erfolglose Versuche, Schiffe zu legen, wird in ?u?erer For-Schleife
      # erhoeht.
      tries_counter <- 1
      # Regelt Durchlaeufe der inneren FOr Schleife
      sum_len <- sum(c(n5,n4,n3,n2))
      # Vektor, der Schiffslaengen je nach eingegebener Wiederholung nx
      # wiederholt.
      lengths_boats <-c(rep.int(5,times=n5),rep.int(4,times=n4),
                        rep.int(3,times=n3),rep.int(2,times=n2)) 
    
      # Logischer Vektor, er TRUE wird, falls Schiffsetzen funktioniert,
      logical_setship <- NULL
      error_message <- NULL
      successful_message <-NULL
      for(i in 1:10){
        # Einfuehrung der ZwischenRichtungsmatrix, sodass die urspr
        # uenglichen Richtungsmatrizen nicht wieder neu eingefuehrt
        # werden muessen.
        current_field_horizontal <- combat_field_horizontal
        current_field_vertical   <- combat_field_vertical
        # Zaehlt die erfolgreich gelegten Schiffe, je erfolgreichem 
        # Versuch um eins erhoeht.
        counter_set_ship <- 0
        
        overall_ship_positions_vertical <- NULL
        overall_ship_positions_horizontal <- NULL
        overall_ship_lengths_vertical <- NULL
        overall_ship_lengths_horizontal <- NULL
  
        for(j in 1:sum_len){
          # Zugriff auf aktuelle Schiffslaenge
          length_current <- lengths_boats[j] 
          # Erstellen einer Zufallsrichung fuer jedes Schiff neu
          direction_random <- direction.random()
          # Testet, ob Schiff der Laenge length_current in Richtung
          # direction_random gelegt werden kann, basieren auf
          # veraenderten Richtungsmatrizen.
          logical_setship <- positioning_possible(length_current,
                                                  current_field_horizontal,
                                                  current_field_vertical,
                                                  direction_random)
          # Falls Schiffesetzen nicht erfolgreich, wird der Zaehler tries_counter
          # um 1 erhoeht und je nach Richtung eine Fehlermeldung ausgegeben, die
          # bei benoetigten Versuchen >1 oder Platzieren nicht moeglich mit aus
          # gegeben werden.
          if(logical_setship==FALSE){
            tries_counter <- tries_counter+1
            if(direction_random=="horizontal"){
              # Fehlermeldung, die je nach Versuch an dieser Stelle eine Fehler
              # meldung ausgiebt
              error_message[i] <- 
                sprintf("Error: Das Boot der Laenge %i hat in horizontaler Richtung keinen Platz mehr!"
                        ,length_current)
              
              break
            }else if(direction_random=="vertical"){
              error_message[i] <- 
                sprintf("Error: Das Boot der Laenge %i hat in vertikaler Richtung keinen Platz mehr!"
                        ,length_current)
              
              break
            }
          }
          # Falls Schiffsetzen erfolgreich, wird je nach Richtung einmal diese
          # Prozedur ausgefuehrt.
          else if(logical_setship==TRUE){
            if(direction_random=="horizontal"){
              # Eine zufaellige Startposition fuer ein horizintal zu legendes
              # Schiff
              ship_start_position <- position_random_horizontal(
                length_current,current_field_horizontal)
              overall_ship_positions_horizontal[j] <- list(ship_start_position)
              overall_ship_lengths_horizontal[j]   <- length_current
              # Schiff wird in die Horizontalmatrix gelegt
              current_field_horizontal <- set_ships_horizontal(
                ship_start_position,length_current,current_field_horizontal)  
              # Zahlen werden angepasst
              current_field_horizontal <- shift_numbers_horizontal(
                current_field_horizontal)
              # 0en/6en werden in Vertikalmatrix uebertragen
              current_field_vertical <- transfer_zeroes_ones_vertical(
                current_field_vertical,current_field_horizontal)
              # Zahlen in Vertikalmatrix werden dementsprechend angepasst
              current_field_vertical <- shift_numbers_vertical(
                current_field_vertical)
              # Dieses Objekt ist nur wichtig, falls es die 10. Aktion war, bzw.
              # falls danach kein Schiff mehr gelegt werden muss. Es ist bewusst
              # Diese Matrix stellt damit die finale Matrix dar
              # Schiffzaehler wird nach jedem Schiffsetzen um 1 erhoeht, so dass
              # nach 10 erfolgreichen Versuchen beendet werden kann.
              counter_set_ship <- counter_set_ship +1
              # Diese Hilfsfunktion fuegt die Schiffe in das Plotfeld hinzu
            }
            # Analog fuer die Richtung vertical
            else if(direction_random=="vertical"){
              ship_start_position <- position_random_vertical(
                length_current,current_field_vertical)
              overall_ship_positions_vertical[j] <- list(ship_start_position)
              overall_ship_lengths_vertical[j]   <- length_current
              current_field_vertical<-set_ships_vertical(
                ship_start_position,length_current,current_field_vertical)
              current_field_vertical <-shift_numbers_vertical(
                current_field_vertical)
              current_field_horizontal <- transfer_zeroes_ones_horizontal(
                current_field_vertical,current_field_horizontal)
              current_field_horizontal <- shift_numbers_horizontal(
                current_field_horizontal)
              counter_set_ship <- counter_set_ship +1
            }
            # Egal welche Richtung als letztes aufgerufen wird, dieses Objekt
            # ==sum_len, falls jedes Schiff gelegt werden konnte.
            final_logical_setship<-counter_set_ship
          }
        }
        
        if(final_logical_setship==sum_len){
          # tries_counter wird hierbei fuer die benoetigten Versuche verwendet.
          overall_ship_positions_vertical <- Filter(Negate(is.null), 
                                                    overall_ship_positions_vertical)
          overall_ship_positions_horizontal <- Filter(Negate(is.null), 
                                                      overall_ship_positions_horizontal)
          
          overall_ship_lengths_horizontal<-  overall_ship_lengths_horizontal[!(
            is.na( overall_ship_lengths_horizontal))]
          overall_ship_lengths_vertical <-  overall_ship_lengths_vertical[!(
          is.na( overall_ship_lengths_vertical))]
          
          draw_combat_zone(overall_ship_positions_vertical,
                           overall_ship_positions_horizontal,
                           overall_ship_lengths_vertical, 
                           overall_ship_lengths_horizontal)
          successful_message <- sprintf("Benoetigte Versuche: %i",tries_counter)
          # Endausgabe wird aus der Angabe der benoetigten Versuche und de jeweil
          # igen Fehlermeldungen bei erfolglosem Schifflegen, gebildet.
          ausgabe <- c(successful_message,error_message)
          return(ausgabe)
        }
        if (tries_counter==10){
          # Falls die Anzahl erfolgloser Versuche ==10, ist die Platzierung ge
          # scheitert.
          ausgabe<-c("Error: Platzierung der Schiffe nicht moeglich!",
                     error_message) 
          # Plotfenster soll geleert werden, falls Schiffesetzen nicht moeglich
          # war.
          dev.off()
          return(ausgabe)
        }
      }
    }
    # Ausfuehren der uebergeordneten Funktion die alle Hilfsfunktionen aufruft
    battleship_field(combat_field_horizontal,combat_field_vertical,n5,n4,n3,n2)
  }
## Aufgabe 4 (b)
  # Abspeichern drei erfolgreich geplotteter Spielfelder
  #getwd()
seed1 <- 123
seed2 <- 4567
seed3 <- 1234567

pdf(file="Spielfeld_1.pdf")
set.seed(seed1)
combat_zone(1,2,3,4)
dev.off()

pdf(file="Spielfeld_2.pdf")
set.seed(seed2)
combat_zone(1,2,3,4)
dev.off()

pdf(file="Spielfeld_3.pdf")
set.seed(seed3)
combat_zone(1,2,3,4)
dev.off()