data<-read.csv("R/datasetClean.csv")

#желимо да оценимо рецимо ниво неуређености протеина:

#stvarna vrednost nivoa neuredjenosti
t=sum(data$disorder_content)

populacija<-c(data$disorder_content)
populacija

N<-length(populacija)
N

#moramo odabrati veoma veliki broj jedinki da vrednost choose(N,n) ne bi tezila beskonacnosti
n<-1884800

set.seed(123)
uzorak<-sample(populacija, size=n, replace=FALSE)
uzorak

#posto su sve verovatnoce iste, a podaci veliki, necemo praviti vektor verovatnoca,
#vec cemo samo jednom izracunati tu verovatnocu i nadalje je koristiti
P_uzorka=1/choose(N,n)
P_uzorka

t_ocene<-N*mean(uzorak)

#ispisujemo ocenjenu vrednost
t_ocene

#a zatim i realnu vrednost
t
#vidimo da su vrednosti veoma bliske, no, to smo i ocekivali s obzirom da smo uzeli veliki
# udeo pocetnog skupa u uzorak

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
cat("Uzorkovanje bez ponavljanja - neuredjenost proteina:\nOcenjena vrednost:", file="~/R/psuBPneuredjenost.txt")
capture.output( N*mean(uzorak), file="~/R/psuBPneuredjenost.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/psuBPneuredjenost.txt", append=TRUE)
capture.output(t, file="~/R/psuBPneuredjenost.txt", append=TRUE)
cat("Ocena disperzije ocene: ", file="~/R/psuBPneuredjenost.txt", append=TRUE)
capture.output(ocena_D_Y, file="~/R/psuBPneuredjenost.txt", append=TRUE)
cat("Disperzija ocene: ", file="~/R/psuBPneuredjenost.txt", append=TRUE)
capture.output(D_Y, file="~/R/psuBPneuredjenost.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/psuBPneuredjenost.txt", append=TRUE)
capture.output(n, file="~/R/psuBPneuredjenost.txt", append=TRUE)

#pogledajmo kako se ponasa nasa ocena kada smanjimo obim uzorka

n<-1884780
set.seed(123)
uzorak<-sample(populacija, size=n, replace=FALSE)
uzorak
P_uzorka=1/choose(N,n)
P_uzorka

t_ocene2<-N*mean(uzorak)

#ispisujemo ocenjenu vrednost
t_ocene2

#a zatim i realnu vrednost
t
#vidimo da je procena i u ovom slucaju veoma dobra

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
cat("\nUzorkovanje bez ponavljanja - neuredjenost proteina:\nOcenjena vrednost:", file="~/R/psuBPneuredjenost.txt", append = TRUE)
capture.output( N*mean(uzorak), file="~/R/psuBPneuredjenost.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/psuBPneuredjenost.txt", append=TRUE)
capture.output(t, file="~/R/psuBPneuredjenost.txt", append=TRUE)
cat("Ocena disperzije ocene: ", file="~/R/psuBPneuredjenost.txt", append=TRUE)
capture.output(ocena_D_Y, file="~/R/psuBPneuredjenost.txt", append=TRUE)
cat("Disperzija ocene: ", file="~/R/psuBPneuredjenost.txt", append=TRUE)
capture.output(D_Y, file="~/R/psuBPneuredjenost.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/psuBPneuredjenost.txt", append=TRUE)
capture.output(n, file="~/R/psuBPneuredjenost.txt", append=TRUE)

#probajmo da jos malo spustimo n
n<-1884770
set.seed(123)
uzorak<-sample(populacija, size=n, replace=FALSE)
uzorak
P_uzorka=1/choose(N,n)
P_uzorka

t_ocene3<-N*mean(uzorak)

#ispisujemo ocenjenu vrednost
t_ocene3

#a zatim i realnu vrednost
t
#primetimo da se ova ocena razlikuje malo vise od prethodnih, no, i dalje je veoma dobra

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
cat("\nUzorkovanje bez ponavljanja - neuredjenost proteina:\nOcenjena vrednost:", file="~/R/psuBPneuredjenost.txt", append = TRUE)
capture.output( N*mean(uzorak), file="~/R/psuBPneuredjenost.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/psuBPneuredjenost.txt", append=TRUE)
capture.output(t, file="~/R/psuBPneuredjenost.txt", append=TRUE)
cat("Ocena disperzije ocene: ", file="~/R/psuBPneuredjenost.txt", append=TRUE)
capture.output(ocena_D_Y, file="~/R/psuBPneuredjenost.txt", append=TRUE)
cat("Disperzija ocene: ", file="~/R/psuBPneuredjenost.txt", append=TRUE)
capture.output(D_Y, file="~/R/psuBPneuredjenost.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/psuBPneuredjenost.txt", append=TRUE)
capture.output(n, file="~/R/psuBPneuredjenost.txt", append=TRUE)

#pokusajmo i dalje da smanjimo n
n<-1884760
set.seed(123)
uzorak<-sample(populacija, size=n, replace=FALSE)
uzorak
P_uzorka=1/choose(N,n)
P_uzorka

t_ocene4<-N*mean(uzorak)

# ispisujemo ocenjenu vrednost
t_ocene4
# a zatim i realnu vrednost
t
# ova ocena je, zanimljivo, najbliza stvarnoj vrednosti do sad

#recunamo ocenu disperzije ocene
ocena_D_Y_sr <- (N - n) * var(uzorak) / (N * n) # ocena disperzije ocene Y_sr
# a sada ocenjujemo i vrednost ocene disperzije totala
ocena_D_Y <- N^2*ocena_D_Y_sr
ocena_D_Y

#racunao sada i disperziju ocene
sigma <-sd(uzorak)
D_Y <- N^2*sigma^2*(N-n)/(N*n)
D_Y

#ispisujemo dobijene vrednosti u fajl
cat("\nUzorkovanje bez ponavljanja - neuredjenost proteina:\nOcenjena vrednost:", file="~/R/psuBPneuredjenost.txt", append = TRUE)
capture.output( N*mean(uzorak), file="~/R/psuBPneuredjenost.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/psuBPneuredjenost.txt", append=TRUE)
capture.output(t, file="~/R/psuBPneuredjenost.txt", append=TRUE)
cat("Ocena disperzije ocene: ", file="~/R/psuBPneuredjenost.txt", append=TRUE)
capture.output(ocena_D_Y, file="~/R/psuBPneuredjenost.txt", append=TRUE)
cat("Disperzija ocene: ", file="~/R/psuBPneuredjenost.txt", append=TRUE)
capture.output(D_Y, file="~/R/psuBPneuredjenost.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/psuBPneuredjenost.txt", append=TRUE)
capture.output(n, file="~/R/psuBPneuredjenost.txt", append=TRUE)
# napomenimo da vise nismo u stanju da smanjimo n a da pri tome dobijemo rezultat,
# pa cemo se ovde zaustaviti
