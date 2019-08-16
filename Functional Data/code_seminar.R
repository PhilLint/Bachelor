################################################################################
library(fda)
library(fds)
library(ggplot2)
library(lattice)
library(fields)
library(plyr)
library(dplyr)
################################################################################
# AUFBEREITUNG #
# setwd("C://Users//Phillin//Desktop//UNI//LMU//5. Semester//Seminar")
# Funktion für Achsenbeschriftung der FPCA PLots
source(".//plot.pca.fd.corr.R")
# Einlesen
italy_data <- read.csv(".//AirQualityUCI.csv", header=T, sep=";")

# 11.03.1004 - 11.03.2005
italy_data <- italy_data[-c(1:6,9343:9741),c(1,2,3,8,10)]
italy_data$Date <- as.Date(italy_data$Date,format("%d/%m/%Y"))
italy_data$day <-  as.numeric(italy_data$Date-italy_data$Date[1]) +1 

# Monatsvariable 
italy_data$Month <- format(as.Date(italy_data$Date), "%m")

# Wochentage für Plots Wochenende / Werktage
italy_data$weekday <- weekdays(as.Date(italy_data$Date,'%d-%m-%Y'))

# days with NA CO.GT. (-200) entfernen (NA)
italy_data <- italy_data[-which((italy_data$Time == "04.00.00")),]
days_w_NA <- italy_data[which((italy_data$CO.GT. %in% c("-200","-200,0"))),]$day
italy_data_wo_na <- italy_data[-c(which(italy_data$day %in% days_w_NA)),]
italy_data_wo_na$CO.GT.<- as.numeric(italy_data_wo_na$CO.GT.)
italy_data_wo_na$Time<- as.numeric(italy_data_wo_na$Time)-2

# 282 Tage / Beobachtungen
length(italy_data_wo_na$day)/23
################################################################################
# Darstellung der Daten
col=tim.colors(length(unique(italy_data_wo_na$day)))
# Farbspektrum nach der Tage
# plot(rep(1, times=282), col=col, lwd=3)  

png("xyplot_italy.png",height=768,width=1536,res=120)
xyplot(CO.GT. ~ Time,groups=day,col=col,
       scales=list(x=list(cex=1.5), y=list(cex=1.5)),
       xlab=list(label="Time", cex=1.5),
       ylab=list(label="CO concentration", cex=1.5),
       lwd=1,t="b",pch=19,data=italy_data_wo_na, ylim=c(0,120), xlim=c(0,23))
dev.off()
################################################################################
# 1. Basis
nbasis_8 <- 8
splinebasis_8 <- create.bspline.basis(c(0,23),nbasis = nbasis_8, norder = norder)
# plot(splinebasis_8)

# 2 . Basis
nbasis_10 <- 10
norder <- 4
splinebasis_10 <- create.bspline.basis( c(0,23), nbasis_10, norder )
# plot(splinebasis)

# 3. Basis
nbasis_12 <- 12
splinebasis_12 <- create.bspline.basis( c(0,23), nbasis = nbasis_12, norder = norder)
# plot(splinebasis_12)

# 4. Basen
nbasis_14 <- 14
splinebasis_14 <- create.bspline.basis(c(0,23),nbasis = nbasis_14, norder = norder)
# plot(splinebasis_14)

# 5. Basen
nbasis_20 <- 20
splinebasis_20 <- create.bspline.basis(rangeval = c(0,23), nbasis_20, norder = norder)

# Matrix für Data2fd Funktion
tage_matrix <- split(italy_data_wo_na$CO.GT.,italy_data_wo_na$day)
tage_matrix <- as.data.frame(tage_matrix)
tage_matrix <- as.matrix(tage_matrix)

# Objekt, das Data2fd gebraucht 
row <- nrow(italy_data_wo_na)
repi <- seq(from=1, to=row, by=23)

# speichert Monat und Wochentag jeder Beobachtung
months <- italy_data_wo_na$Month[repi]
weekdays <- italy_data_wo_na$weekday[repi]

# Stundenobjekt 
hours <- c(0:3,5:23)

# Listenobjekt für Data2fd
new_italy <- list(Co =tage_matrix, zeit = hours, months=italy_data_wo_na$Month[repi],weekday=
                  italy_data_wo_na$weekday[repi])

# FD Objekterzeugung
fdnames = list("Uhrzeit" = hours,
               "Tag" = colnames(new_italy$Co),
               "Wochentag" = new_italy$weekday)

italy_fd_8  <- Data2fd(new_italy$Co ,basisobj =  splinebasis_8, fdnames=fdnames)
italy_fd_10 <- Data2fd(new_italy$Co ,basisobj =  splinebasis_10, fdnames=fdnames)
italy_fd_12 <- Data2fd(new_italy$Co ,basisobj =  splinebasis_12, fdnames=fdnames)
italy_fd_14 <- Data2fd(new_italy$Co ,basisobj =  splinebasis_14, fdnames=fdnames)
italy_fd_20 <- Data2fd(new_italy$Co ,basisobj =  splinebasis_20, fdnames=fdnames)

# Farben Wochentage
farben <- tim.colors(length(unique(italy_data_wo_na$weekday)))
farbvektor <- as.factor(dplyr::recode(as.character(italy_data_wo_na$weekday[repi]),
                                      "Montag" = farben[1],  "Dienstag" = farben[1],
                                      "Mittwoch" = farben[1], "Donnerstag" = farben[1],
                                      "Freitag" = farben[1], "Samstag" = farben[7],
                                      "Sonntag" = farben[7]))

# Mean Plots  
png("mean_CO_12.png",height=768,width=768,res=120)
par(mfrow=c(1,1))
plot(mean.fd(italy_fd_12), ylim=c(0,110), main="12 Basisfunctions",
     ylab = "CO concentration", xlab = "Time")
dev.off()

# Präsentation (deutsch)
png("mean_CO_praes.png",height=768,width=768,res=120)
par(mfrow=c(1,1))
plot(mean.fd(italy_fd_12), ylim=c(0,100), main="",
     ylab="", xlab = "Uhrzeit")
dev.off()

# Mean Vergleich
png("mean_CO_comp.png",height=768,width=768,res=120)
par(mfrow=c(2,2))
plot(mean.fd(italy_fd_8), ylim=c(0,50), main="8 Basisfunctions",
     ylab = "CO concentration", xlab = "Time")
plot(mean.fd(italy_fd_10), ylim=c(0,50), main="10 Basisfunctions",
     ylab = "CO concentration", xlab = "Time")
plot(mean.fd(italy_fd_12),  main="12 Basisfunctions",
     ylab = "CO concentration", ylim=c(0,50), xlab = "Time")
plot(mean.fd(italy_fd_20),  main="20 Basisfunctions",
     ylab = "CO concentration", ylim=c(0,50), xlab = "Time")
dev.off()

# Fitvergleich
png("fit_comp.png",height=768,width=768,res=120)
par(mfrow=c(2,2))
plot.fd(italy_fd_8, ylim=c(0,100), main="8 Basisfunctions",
     ylab = "CO concentration", xlab = "Time", col=col)
plot.fd(italy_fd_10, ylim=c(0,100), main="10 Basisfunctions",
        ylab = "CO concentration", xlab = "Time", col=col)
plot.fd(italy_fd_12, ylim=c(0,110), main="12 Basisfunctions",
        ylab = "CO concentration", xlab = "Time", col=col)
plot.fd(italy_fd_20, ylim=c(0,110), main="20 Basisfunctions",
        ylab = "CO concentration", xlab = "Time", col=col)
dev.off()

png("plot_fd_20_thesis.png",height=768,width=768,res=120)
plot.fd(italy_fd_20, ylim=c(0,100),
        ylab = "CO concentration", xlab = "Time", col=col)
dev.off()

png("plot_fd_10.png",height=768,width=768,res=120)
plot.fd(italy_fd_10, main = "10 Basisfunctions", col=col, ylim=c(0,100),xlim=c(0,23),
        ylab = "CO concentration",xlab = "Time")
dev.off()

png("plot_fd_8.png",height=768,width=768,res=120)
plot.fd(italy_fd_8, main = "8 Basisfunctions", ylim=c(0,100),
        ylab = "CO concentration",xlab = "Time")
dev.off()

png("plot_fd_10_praes.png",height=768,width=768,res=120)
plot.fd(italy_fd, main = "10 B-spline Basisfunktionen", col=col, ylim=c(0,100),xlim=c(0,23),
        ylab = "CO Konzentration",xlab = "Uhrzeit")
dev.off()

png("plot_fd_12.png",height=768,width=768,res=120)
plot.fd(italy_fd_12, main = "12 Basisfunctions", col=col,ylim=c(0,110), 
        ylab = "CO concentration",xlab = "Time")
dev.off()

png("plot_fd_14.png",height=768,width=768,res=120)
plot.fd(italy_fd_14, main = "14 Basisfunctions",col=col, ylim=c(0,100),
        ylab = "CO concentration",xlab = "Time")
dev.off()

png("plot_fd_20.png",height=768,width=768,res=120)
plot.fd(italy_fd_20, main = "20 Basisfunktionen", col=col, 
        ylab = "CO Konzentration",xlab = "Uhrzeit")
dev.off()

## Anhangsplots - Wochenende
png("plot_fd_12_weekend.png",height=768,width=1536,res=120)
plot.fd(italy_fd_12, main = "12 Basisfunctions", col=farbvektor, 
        ylab = "CO concentration",xlab = "Time")
legend(x=0, y=100,c("week", "weekend"), col =c("black","red"), lwd = c(2,2) ,bty="n")
dev.off()

# FPCA
italy_pca_8  <- pca.fd(italy_fd_8, nharm = 4)
italy_pca_10 <- pca.fd(italy_fd_10, nharm = 4)
italy_pca_12 <- pca.fd(italy_fd_12, nharm = 10)
italy_pca_14 <- pca.fd(italy_fd_14, nharm = 4)
italy_pca_20 <- pca.fd(italy_fd_20, nharm = 4)

# Alle Hauptkomponenten +- Plot 12 Basisfunktionen
png("pca_1.png",height=768,width=768,res=120)
plot.pca.fd.corr(italy_pca_12, harm = 1, xlab="Time", ylab="CO concentration")
dev.off()

png("pca_2.png",height=768,width=768,res=120)
plot.pca.fd.corr(italy_pca_12, harm = 2, xlab="Time", ylab="CO concentration")
dev.off()

png("pca_3.png",height=768,width=768,res=120)
plot.pca.fd.corr(italy_pca_12, harm = 3, xlab="Time", ylab="CO concentration")
dev.off()

png("pca_4.png",height=768,width=768,res=120)
plot.pca.fd.corr(italy_pca_12, harm = 4, xlab="Time", ylab="CO concentration")
dev.off()

# ersten vier Eigenfunktionen
png("pca_eigen.png",height=768,width=768,res=120)
plot(italy_pca_12$harmonics[c(1,2,3,4)], lwd=2, col=c("black","red","darkgreen","blue"), 
     xlab="Time", ylab="weights", main = expression(paste("Eigenfunctions ", phi[1] - phi[4])))
legend(x=0.5, y=-0.2,c("PC1", "PC2", "PC3", "PC4"), lty=c(1,2,3,4), 
       col =c("black","red","darkgreen","blue"), lwd = c(2,2,2,2) ,bty="n")
dev.off()

par(mfrow=c(1,1))
plot(italy_pca_20$harmonics, lwd=2, col=c("black","red","darkgreen","blue"), 
     xlab="Time", ylab="weights",
     main = expression(paste("Eigenfunctions ", phi[1] - phi[4])))
legend(x=0.5, y=-0.2,c("PC1", "PC2", "PC3", "PC4"), lty=c(1,2,3,4), 
       col =c("black","red","darkgreen","blue"), lwd = c(2,2,2,2) ,bty="n")

png("pca_score.png",height=768,width=768,res=120)
plot(italy_pca_12$scores, xlab = "1st pc Score", ylab = "2nd pc Score", lwd=1.75)
abline(h=0, lty=2)
abline(v=0, lty=2)
dev.off()

png("pca_score_praes.png",height=768,width=768,res=120)
plot(italy_pca$scores, xlab = "1. HK Score", ylab = "2. HK Score", lwd=2)
abline(h=0, lty=2)
abline(v=0, lty=2)
dev.off()

png("scree.png",height=768,width=768,res=120)
plot(y=italy_pca_12$varprop,x=c(1:10), type = "b",ylab = "amount of variation",
     xlab = "Principal Components",xaxt="n", ylim = c(0,1), main = "" )
axis(side = 1, at=c(1:10))
dev.off()

png("scree_praes.png",height=768,width=768,res=120)
plot(y=italy_pca_12$varprop,x=c(1:4), type = "b",ylab = "Anteil an Gesamtvarianz",
     xlab = "Hauptkomponenten",xaxt="n", main="Screeplot",las=1 )
axis(side = 1, at=c(1:4))
dev.off()

## Anhangsplots Scores Wochenende
png("pca_score_weekend.png",height=768,width=768,res=120)
plot(italy_pca_12$scores, xlab = "1st pc Score", ylab = "2nd pc Score", 
     col=farbvektor, lwd=1.75)
abline(h=0, lty=2)
abline(v=0, lty=2)
legend(x=90, y=-40,c("week", "weekend"), col =c("black","red"),pch=c(1,1),pt.lwd=2,bty="n")
dev.off()

## Anhangsplots Scores Wochenende
png("pca_score_wochenende.png",height=768,width=768,res=120)
plot(italy_pca_12$scores, xlab = "1. HK Score", ylab = "2. HK Score", main="",
     col=farbvektor, lwd=1.75)
abline(h=0, lty=2)
abline(v=0, lty=2)
legend(x=85, y=-40,c("Wochentag", "Wochenende"), col =c("black","red"),pch=c(1,1),pt.lwd=2)
dev.off()

# Alle Hauptkomponenten 20 Basisfunktionen
plot.pca.fd(italy_pca_20, harm=c(1,2))
plot.pca.fd(italy_pca_20, harm=c(3,4))

png("pca_20_eigen.png",height=768,width=768,res=120)
plot(italy_pca_20$harmonics, lwd=2, col=c("black","red","green","blue"))
legend(x=1, y=-0.2,c("PC1", "PC2", "PC3", "PC4"), lty=c(1,2,3,4), 
       col =c("black","red","green","blue"), lwd = c(2,2,2,2) ,bty="n")
dev.off()

plot(italy_pca_20$scores, xlab = "1. HK Score", ylab = "2. HK Score",
     col=farbvektor, lwd=2)
plot(italy_pca_20$scores, xlab = "1. HK Score", ylab = "2. HK Score",
     lwd=2)
plot(y=italy_pca_20$varprop,x=c(1:4), type = "b",
     ylab = "Anteil an Gesamtvarianz", xlab = "Hauptkomponenten",xaxt="n" )
axis(side = 1 )

# smoothing FPCA
argvals <- c(0:3,5:23)

# FPCA - 12 Basisfunktionen
LfdObj <- int2Lfd(max(0, 4-2))

# Verschiedene Lambdas
fdPar_20_1 <- fdPar(splinebasis_20, LfdObj, lambda = 1)
fdPar_20_0 <- fdPar(splinebasis_20, LfdObj, lambda = 0)
fdPar_20_001 <- fdPar(splinebasis_20, LfdObj, lambda = 0.01)
fdPar_20_05 <- fdPar(splinebasis_20, LfdObj, lambda = 0.5)
fdPar_20_10 <- fdPar(splinebasis_20, LfdObj, lambda = 10)
fdPar_20_100000 <- fdPar(splinebasis_20, LfdObj, lambda = 100000)

# geglättete FPCA
italy_pca_sm_20_1    <- pca.fd(italy_fd_20, nharm = 4, harmfdPar =fdPar_20_1 )
italy_pca_sm_20_0    <- pca.fd(italy_fd_20, nharm = 4, harmfdPar =fdPar_20_0 )
italy_pca_sm_20_001    <- pca.fd(italy_fd_20, nharm = 4, harmfdPar =fdPar_20_001 )
italy_pca_sm_20_05    <- pca.fd(italy_fd_20, nharm = 4, harmfdPar =fdPar_20_05 )
italy_pca_sm_20_10    <- pca.fd(italy_fd_20, nharm = 4, harmfdPar =fdPar_20_10)
italy_pca_sm_20_100000 <- pca.fd(italy_fd_20, nharm = 4, harmfdPar =fdPar_20_100000 )

# Lambda Vergleich
png("pca_sm_20_comparison_1.png",height=1000,width=1538, res=120)
par(mfrow=c(2,3))
plot(italy_pca_sm_20_0$harmonics, lwd=2, col=c("black","red","darkgreen","blue"),
     ylim=c(-0.55,0.55), xlab="Time", 
     ylab="weights", main = expression(paste(lambda, " = 0")))
legend(x=0, y=-0.2,c("PC1", "PC2", "PC3", "PC4"), lty=c(1,2,3,4), 
       col =c("black","red","green","blue"), lwd = c(1,1,1,1) ,bty="n", cex=.5)
plot(italy_pca_sm_20_001$harmonics, lwd=2, col=c("black","red","darkgreen","blue"),
     ylim=c(-0.55,0.55), xlab="Time", 
     ylab="weights", main = expression(paste(lambda, " = 0.01")))
plot(italy_pca_sm_20_05$harmonics, lwd=2, col=c("black","red","darkgreen","blue"),
     ylim=c(-0.55,0.55), xlab="Time", 
     ylab="weights", main = expression(paste(lambda, " = 0.5")))
plot(italy_pca_sm_20_1$harmonics, lwd=2, col=c("black","red","darkgreen","blue"),
     ylim=c(-0.55,0.55), xlab="Time", 
     ylab="weights", main = expression(paste(lambda, " = 1")))
plot(italy_pca_sm_20_10$harmonics, lwd=2, col=c("black","red","darkgreen","blue"),
     ylim=c(-0.55,0.55), xlab="Time", 
     ylab="weights", main = expression(paste(lambda, " = 10")))
plot(italy_pca_sm_20_100000$harmonics, lwd=2, col=c("black","red","darkgreen","blue"),
     ylim=c(-0.55,0.55), xlab="Time", 
     ylab="weights", main = expression(paste(lambda, " = 100000")))
dev.off()

## Alt (noch nicht klar, ob zu löschen)

png("pca_sm_20_05.png",height=768,width=768,res=120)
plot(italy_pca_sm_20_05$harmonics, lwd=2, col=c("black","red","darkgreen","blue"),
     main="lambda = 0.5",ylim=c(-0.5,0.5))
dev.off()

png("pca_eigen_praes.png",height=768,width=768,res=120)
plot(italy_pca_12$harmonics, lwd=2, col=c("black","red","darkgreen","blue"),
     main="12 Basisfunktionen",ylim=c(-0.5,0.5))
dev.off()

png("pca_sm_20_comparison_2_1.png",height=768,width=768,res=120)
par(mfrow=c(1,1))
plot(italy_pca_sm_20_10$harmonics, lwd=2, col=c("black","red","darkgreen","blue"),
     main="lambda = 10",ylim=c(-0.5,0.5))
legend(x=0, y=-0.2,c("PC1", "PC2", "PC3", "PC4"), lty=c(1,2,3,4), 
       col =c("black","red","green","blue"), lwd = c(1,1,1,1) ,bty="n", cex=.5)
dev.off()

png("pca_sm_20_comparison_2_2.png",height=768,width=768,res=120)
par(mfrow=c(1,1))
plot(italy_pca_sm_20_100000$harmonics, lwd=2, col=c("black","red","darkgreen","blue"),
     main="lambda = 100000",ylim=c(-0.5,0.5))
dev.off()

png("pca_sm_20_comparison_3_1.png",height=768,width=768,res=120)
par(mfrow=c(1,1))
plot(italy_pca_sm_20_05$harmonics, lwd=2, col=c("black","red","darkgreen","blue"),
     main="lambda = 0.5",ylim=c(-0.5,0.5))
legend(x=0, y=-0.2,c("PC1", "PC2", "PC3", "PC4"), lty=c(1,2,3,4), 
       col =c("black","red","green","blue"), lwd = c(1,1,1,1) ,bty="n", cex=.5)
dev.off()
