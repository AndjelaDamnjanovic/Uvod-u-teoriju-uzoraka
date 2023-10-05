data=read.csv("R/datasetClean.csv")
populacija <- data$contains_ind
N <-length(populacija)

#opet delimo podatke prvo u stratume
jedinke_1 <-c(data$contains_ind[data$contains_ind==1])
jedinke_2 <-c(data$contains_ind[data$contains_ind==0])

# sada pravimo klastere-pocnimo sa 5 klastera iste velicine:
klaster1 <- c(jedinke_1[1: round(length(jedinke_1)*0.2)])
klaster2 <- c(jedinke_1[round((length(jedinke_1)*0.2+1)) : round((length(jedinke_1)*0.4))])
klaster3 <- c(jedinke_1[round((length(jedinke_1)*0.4+1)): round((length(jedinke_1)*0.6))])
klaster4 <- c(jedinke_1[round((length(jedinke_1)*0.6+1)): round((length(jedinke_1)*0.8))])
klaster5 <- c(jedinke_1[round((length(jedinke_1)*0.8+1)): (length(jedinke_1))])

klaster1 <-append(klaster1, c(jedinke_2[1: round(length(jedinke_2)*0.2)]))

klaster2 <-append(klaster2, c(jedinke_2[round((length(jedinke_2)*0.2+1)) : round((length(jedinke_2)*0.4))]))

klaster3 <-append(klaster3, c(jedinke_2[round((length(jedinke_2)*0.4+1)): round((length(jedinke_2)*0.6))]))

klaster4 <-append(klaster4, c(jedinke_2[round((length(jedinke_2)*0.6+1)): round((length(jedinke_2)*0.8))]))

klaster5 <-append(klaster5, c(jedinke_2[round((length(jedinke_2)*0.8+1)): (length(jedinke_2))]))

totali <-c(sum(klaster1), sum(klaster2), sum(klaster3), sum(klaster4), sum(klaster5))
velicine <-c(length(klaster1), length(klaster2), length(klaster3), length(klaster4), length(klaster5))

M <- length(populacija) # ukupan br jedinki
N <- 5 # broj klastera
n <- 3 # broj klastera koje mi biramo
set.seed(123)
uzorak <-sample(N, n, replace = F)
uzorak
# racunamo total na svakom klasteru koji je izvucen u uzorak
t_l_na_uzorku <-c((totali[uzorak]))
t_l_na_uzorku

#ocenjujemo total
t_ocena <- N * mean(t_l_na_uzorku)
t_ocena

#i ispisujemo stvarnu vrednost totala
t <-sum(populacija)
t
#kao sto vidimo, rezultati su veoma dobri

#racunamo sada ocenu disperzije ocene psu bez ponavljanja
ocena_D_t_ocena <- N ^ 2 * (1 - n / N) * var(t_l_na_uzorku) / n
ocena_D_t_ocena

#ispis u fajl
cat("Klasterovanje- jednaka velicina klastera - indikator sadrzanja:\nOcenjena vrednost:", file="~/R/klasterovanjeSadrzi.txt")
capture.output( t_ocena, file="~/R/klasterovanjeSadrzi.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/klasterovanjeSadrzi.txt", append=TRUE)
capture.output(t, file="~/R/klasterovanjeSadrzi.txt", append=TRUE)
cat("Ocena disperzije ocene: ", file="~/R/klasterovanjeSadrzi.txt", append=TRUE)
capture.output(ocena_D_t_ocena, file="~/R/klasterovanjeSadrzi.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/klasterovanjeSadrzi.txt", append=TRUE)
capture.output(sum(velicine[uzorak]), file="~/R/klasterovanjeSadrzi.txt", append=TRUE)


#probajmo sada, kao i malopre, da podelimo jedinke u 5 klastera nejednakih velicina
klaster1 <- c(jedinke_1[1: round(length(jedinke_1)*0.4)])
klaster2 <- c(jedinke_1[round((length(jedinke_1)*0.4+1)) : round((length(jedinke_1)*0.7))])
klaster3 <- c(jedinke_1[round((length(jedinke_1)*0.7+1)): round((length(jedinke_1)*0.9))])
klaster4 <- c(jedinke_1[round((length(jedinke_1)*0.9+1)): round((length(jedinke_1)*0.95))])
klaster5 <- c(jedinke_1[round((length(jedinke_1)*0.95+1)): (length(jedinke_1))])

klaster1 <-append(klaster1, c(jedinke_2[1: round(length(jedinke_2)*0.4)]))

klaster2 <-append(klaster2, c(jedinke_2[round((length(jedinke_2)*0.4+1)) : round((length(jedinke_2)*0.7))]))

klaster3 <-append(klaster3, c(jedinke_2[round((length(jedinke_2)*0.7+1)): round((length(jedinke_2)*0.9))]))

klaster4 <-append(klaster4, c(jedinke_2[round((length(jedinke_2)*0.9+1)): round((length(jedinke_2)*0.95))]))

klaster5 <-append(klaster5, c(jedinke_2[round((length(jedinke_2)*0.95+1)): (length(jedinke_2))]))

totali <-c(sum(klaster1), sum(klaster2), sum(klaster3), sum(klaster4), sum(klaster5))

M <- length(populacija) # ukupan br jedinki
N <- 5 # broj klastera
n <- 3 # broj klastera koje mi biramo
set.seed(123)
uzorak <-sample(N, n, replace = F)
uzorak
# racunamo total na svakom klasteru koji je izvucen u uzorak
t_l_na_uzorku <-c((totali[uzorak]))
t_l_na_uzorku


#probajmo Horvitz-Thompsonovu ocenu
duzine <-c(length(klaster1), length(klaster2), length(klaster3), length(klaster4), length(klaster5))
M_l <- duzine[uzorak]
M_l

psi <- M_l / M # verovatnoće izbora klastera
pi <- 1 - (1 - psi) ^ n # verovatnoće uključenja klastera u uzorak klastera

#nalazimo ocenu totala Horvitz-Thompsonovom ocenom
t_Y_ht <-sum(totali[uzorak]/pi)
t_Y_ht

#i ispisujemo stvarnu vrednost
t<- sum(populacija)
t
# vidimo da su rezultati istog reda velicine, ali da su dosta razliciti

#probajmo sada da ocenimo total koristeci formulu za PSU bez ponavljanja
t_ocena <- N * mean(t_l_na_uzorku)
t_ocena
# i ovde je ocena dosta bliza nego u slucaju Horvitz-Thompsona

#racunamo sada ocenu disperzije ocene psu bez ponavljanja
ocena_D_t_ocena <- N ^ 2 * (1 - n / N) * var(t_l_na_uzorku) / n
ocena_D_t_ocena

#ispis u fajl
cat("\nKlasterovanje- nejednaka velicina klastera - indikator sadrzanja:\nOcenjena vrednost- Horvitz-Thompsonova ocena:", file="~/R/klasterovanjeSadrzi.txt", append=TRUE)
capture.output( t_Y_ht, file="~/R/klasterovanjeSadrzi.txt", append=TRUE)
cat("Ocena totala - psu bez ponavljanja: ", file="~/R/klasterovanjeSadrzi.txt", append=TRUE)
capture.output(t, file="~/R/klasterovanjeSadrzi.txt", append=TRUE)
cat("Ocena disperzije ocene - psu bez ponavljanja: ", file="~/R/klasterovanjeSadrzi.txt", append=TRUE)
capture.output(ocena_D_t_ocena, file="~/R/klasterovanjeSadrzi.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/klasterovanjeSadrzi.txt", append=TRUE)
capture.output(t_ocena, file="~/R/klasterovanjeSadrzi.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/klasterovanjeSadrzi.txt", append=TRUE)
capture.output(sum(velicine[uzorak]), file="~/R/klasterovanjeSadrzi.txt", append=TRUE)
