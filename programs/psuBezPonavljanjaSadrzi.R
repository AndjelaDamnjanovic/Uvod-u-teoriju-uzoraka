data<-read.csv("R/datasetClean.csv")

#stvarna vrednost totala indikatora sadrzanja
t=sum(data$contains_ind)

#izdvajamo obelezje od interesa
populacija<-c(data$contains_ind)
populacija

N<-length(populacija)
N

#moramo odabrati veoma veliki broj jedinki da vrednost choose(N,n) ne bi tezila beskonacnosti
n<-1884800

set.seed(123)
uzorak<-sample(populacija, size=n, replace=FALSE)
uzorak

#posto su sve verovatnoce iste, a podaci veliki, necemo praviti vektor verovatnoca, vec cemo 
#samo jednom iyracunati tu verovatnocu i nadalje je koristiti
P_uzorka=1/choose(N,n)
P_uzorka

t_ocene<-N*mean(uzorak)

#ispisujemo predvidjenu vrednost
t_ocene

#a zatim i realnu vrednost
t
#ocena je veoma bliska stvarnoj vrednosti, ali primetimo da je veca razlika izmedju ocene
# i stvarne vrednosti ovde nego u prethodom slucaju

#recunamo ocenu disperzije ocene
ocena_D_Y_sr <- (N - n) * var(uzorak) / (N * n) # ocena disperzije ocene Y_sr
# a sada ocenjujemo i vrednost totala
ocena_D_Y <- N^2*ocena_D_Y_sr
ocena_D_Y

#racunao sada i disperziju ocene
sigma <-sd(uzorak)
D_Y <- N^2*sigma^2*(N-n)/(N*n)
D_Y

#ispisujemo dobijene vrednosti u fajl
cat("Uzorkovanje bez ponavljanja - contains_ind:\nOcenjena vrednost:", file="~/R/psuBPsadrzi.txt")
capture.output( N*mean(uzorak), file="~/R/psuBPsadrzi.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output(t, file="~/R/psuBPsadrzi.txt", append=TRUE)
cat("Ocena disperzije ocene: ", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output(ocena_D_Y, file="~/R/psuBPsadrzi.txt", append=TRUE)
cat("Disperzija ocene: ", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output(D_Y, file="~/R/psuBPsadrzi.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output(n, file="~/R/psuBPsadrzi.txt", append=TRUE)

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
#primetimo da je ocena jednaka totalu

#recunamo ocenu disperzije ocene
ocena_D_Y_sr <- (N - n) * var(uzorak) / (N * n) # ocena disperzije ocene Y_sr
# a sada ocenjujemo i vrednost totala
ocena_D_Y <- N^2*ocena_D_Y_sr
ocena_D_Y

#racunao sada i disperziju ocene
sigma <-sd(uzorak)
D_Y <- N^2*sigma^2*(N-n)/(N*n)
D_Y


cat("\nUzorkovanje bez ponavljanja - contains_ind:\nOcenjena vrednost:", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output( N*mean(uzorak), file="~/R/psuBPsadrzi.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output(t, file="~/R/psuBPsadrzi.txt", append=TRUE)
cat("Ocena disperzije ocene: ", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output(ocena_D_Y, file="~/R/psuBPsadrzi.txt", append=TRUE)
cat("Disperzija ocene: ", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output(ocena_D_Y, file="~/R/psuBPsadrzi.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output(n, file="~/R/psuBPsadrzi.txt", append=TRUE)

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
#ocena se dosta pogorsala u odnosu na prethodne, ali je i dalje veoma blizu stvarnoj vrednosti
#recunamo ocenu disperzije ocene
ocena_D_Y_sr <- (N - n) * var(uzorak) / (N * n) # ocena disperzije ocene Y_sr
# a sada ocenjujemo i vrednost totala
ocena_D_Y <- N^2*ocena_D_Y_sr
ocena_D_Y

#racunao sada i disperziju ocene
sigma <-sd(uzorak)
D_Y <- N^2*sigma^2*(N-n)/(N*n)
D_Y


cat("\nUzorkovanje bez ponavljanja - contains_ind:\nOcenjena vrednost:", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output( N*mean(uzorak), file="~/R/psuBPsadrzi.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output(t, file="~/R/psuBPsadrzi.txt", append=TRUE)
cat("Ocena disperzije ocene: ", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output(ocena_D_Y, file="~/R/psuBPsadrzi.txt", append=TRUE)
cat("Disperzija ocene: ", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output(D_Y, file="~/R/psuBPsadrzi.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output(n, file="~/R/psuBPsadrzi.txt", append=TRUE)

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
#zanimljivo, i ovde je ocena bolje od prethodne iako je u prethodni uzorak uzeto vise jedinki

#recunamo ocenu disperzije ocene
ocena_D_Y_sr <- (N - n) * var(uzorak) / (N * n) # ocena disperzije ocene Y_sr
# a sada ocenjujemo i vrednost totala
ocena_D_Y <- N^2*ocena_D_Y_sr
ocena_D_Y

#racunao sada i disperziju ocene
sigma <-sd(uzorak)
D_Y <- N^2*sigma^2*(N-n)/(N*n)
D_Y


cat("\nUzorkovanje bez ponavljanja - contains_ind:\nOcenjena vrednost:", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output( N*mean(uzorak), file="~/R/psuBPsadrzi.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output(t, file="~/R/psuBPsadrzi.txt", append=TRUE)
cat("Ocena disperzije ocene: ", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output(ocena_D_Y, file="~/R/psuBPsadrzi.txt", append=TRUE)
cat("Disperzija ocene: ", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output(D_Y, file="~/R/psuBPsadrzi.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/psuBPsadrzi.txt", append=TRUE)
capture.output(n, file="~/R/psuBPsadrzi.txt", append=TRUE)
#napomenimo da vise nismo u stanju da smanjimo n a da pri tome dobijemo rezultat, pa cemo se ovde zaustaviti