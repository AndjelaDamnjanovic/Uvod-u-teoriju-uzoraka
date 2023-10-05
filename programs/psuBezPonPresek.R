data<-read.csv("R/datasetClean.csv")

#stvarna vrednost totala indikatora preseka
t=sum(data$intersect_ind)

populacija<-c(data$intersect_ind)
populacija

N <-length(populacija)
N

#moramo odabrati veoma veliki broj jedinki da vrednost choose(N,n) ne bi tezila beskonacnosti
n<-1884800

set.seed(123)
uzorak<-sample(populacija, size=n, replace=FALSE)
uzorak

#posto su sve verovatnoce iste, a podaci veliki, necemo praviti vektor verovatnoca, vec cemo 
#samo jednom izracunati tu verovatnocu i nadalje je koristiti
P_uzorka=1/choose(N,n)
P_uzorka

t_ocene<-N*mean(uzorak)

#ispisujemo predvidjenu vrednost
t_ocene

#a zatim i realnu vrednost
t
#primetimo da je razlika izmedju ocenjene i realne vrednosti veoma mala

#recunamo ocenu disperzije ocene
ocena_D_Y_sr <- (N - n) * var(uzorak) / (N * n) # ocena disperzije ocene Y_sr
# a sada ocenjujemo i vrednost totala
ocena_D_Y <- N^2*ocena_D_Y_sr
ocena_D_Y

#racunao sada i disperziju ocene
sigma <-sd(uzorak)
D_Y <- N^2*sigma^2*(N-n)/(N*n)
D_Y

cat("Uzorkovanje bez ponavljanja - intersect_ind:\nOcenjena vrednost:", file="~/R/psuBPpresek.txt")
capture.output( N*mean(uzorak), file="~/R/psuBPpresek.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output(t, file="~/R/psuBPpresek.txt", append=TRUE)
cat("Ocena disperzije ocene: ", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output(ocena_D_Y, file="~/R/psuBPpresek.txt", append=TRUE)
cat("Disperzija ocene: ", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output(D_Y, file="~/R/psuBPpresek.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output(n, file="~/R/psuBPpresek.txt", append=TRUE)

#pokusajmo sada isto to za jos neke vrednosti n
n<-1884780
set.seed(123)
uzorak<-sample(populacija, size=n, replace=FALSE)
uzorak
P_uzorka=1/choose(N,n)
P_uzorka

t_ocene2<-N*mean(uzorak)

#ispisujemo predvidjenu vrednost
t_ocene2

#a zatim i realnu vrednost
t
#ocekivano, vrednost ocene se pogorsala sa smanjenjem obima uzorka, ali je i 
# dalje veoma dobra

#racunamo ocenu disperzije ocene
ocena_D_Y_sr <- (N - n) * var(uzorak) / (N * n) # ocena disperzije ocene Y_sr
# a sada ocenjujemo i vrednost totala
ocena_D_Y <- N^2*ocena_D_Y_sr
ocena_D_Y

#racunao sada i disperziju ocene
sigma <-sd(uzorak)
D_Y <- N^2*sigma^2*(N-n)/(N*n)
D_Y

cat("\nUzorkovanje bez ponavljanja - intersect_ind:\nOcenjena vrednost:", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output( N*mean(uzorak), file="~/R/psuBPpresek.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output(t, file="~/R/psuBPpresek.txt", append=TRUE)
cat("Ocena disperzije ocene: ", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output(ocena_D_Y, file="~/R/psuBPpresek.txt", append=TRUE)
cat("Disperzija ocene: ", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output(D_Y, file="~/R/psuBPpresek.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output(n, file="~/R/psuBPpresek.txt", append=TRUE)


#probajmo da jos malo spustimo n
n<-1884770
set.seed(123)
uzorak<-sample(populacija, size=n, replace=FALSE)
uzorak
P_uzorka=1/choose(N,n)
P_uzorka

t_ocene3<-N*mean(uzorak)

#ispisujemo predvidjenu vrednost
t_ocene3

#a zatim i realnu vrednost
t
# ova ocena je, kao sto se moze videti, bliza stvarnoj vrednosti od prosle, ali
# je prva i dalje najbliza

#recunamo ocenu disperzije ocene
ocena_D_Y_sr <- (N - n) * var(uzorak) / (N * n) # ocena disperzije ocene Y_sr
# a sada ocenjujemo i vrednost totala
ocena_D_Y <- N^2*ocena_D_Y_sr
ocena_D_Y

#racunao sada i disperziju ocene
sigma <-sd(uzorak)
D_Y <- N^2*sigma^2*(N-n)/(N*n)
D_Y

cat("\nUzorkovanje bez ponavljanja - intersect_ind:\nOcenjena vrednost:", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output( N*mean(uzorak), file="~/R/psuBPpresek.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output(t, file="~/R/psuBPpresek.txt", append=TRUE)
cat("Ocena disperzije ocene: ", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output(ocena_D_Y, file="~/R/psuBPpresek.txt", append=TRUE)
cat("Disperzija ocene: ", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output(D_Y, file="~/R/psuBPpresek.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output(n, file="~/R/psuBPpresek.txt", append=TRUE)


#pokusajmo i dalje da smanjimo n
n<-1884760
set.seed(123)
uzorak<-sample(populacija, size=n, replace=FALSE)
uzorak
P_uzorka=1/choose(N,n)
P_uzorka

t_ocene4<-N*mean(uzorak)

#ispisujemo predvidjenu vrednost
t_ocene4
#a zatim i realnu vrednost
t
# ova ocena je bliza stvarnoj vrednosti od prethodne 2, sto je zanimljivo s obzirom
# da je broj jedinki u uzorku manji

#recunamo ocenu disperzije ocene
ocena_D_Y_sr <- (N - n) * var(uzorak) / (N * n) # ocena disperzije ocene Y_sr
# a sada ocenjujemo i vrednost totala
ocena_D_Y <- N^2*ocena_D_Y_sr
ocena_D_Y

#racunao sada i disperziju ocene
sigma <-sd(uzorak)
D_Y <- N^2*sigma^2*(N-n)/(N*n)
D_Y

cat("\nUzorkovanje bez ponavljanja - intersect_ind:\nOcenjena vrednost:", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output( N*mean(uzorak), file="~/R/psuBPpresek.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output(t, file="~/R/psuBPpresek.txt", append=TRUE)
cat("Ocena disperzije ocene: ", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output(ocena_D_Y, file="~/R/psuBPpresek.txt", append=TRUE)
cat("Disperzija ocene: ", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output(D_Y, file="~/R/psuBPpresek.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/psuBPpresek.txt", append=TRUE)
capture.output(n, file="~/R/psuBPpresek.txt", append=TRUE)


#napomenimo da vise nismo u stanju da smanjimo n a da pri tome dobijemo rezultat, pa cemo se ovde zaustaviti
