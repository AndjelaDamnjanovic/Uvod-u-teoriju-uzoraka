data<-read.csv("R/datasetClean.csv")
populacija <- c(data$intersect_ind)
N <-length(populacija)

# prvo delimo podatke na stratume, no ovaj put imamo samo 2 stratuma: onaj koji
# sadrzi vrednosti 1 i onaj koji sadrzi vrednosti 0
stratum_1 <-c(data$intersect_ind[data$intersect_ind==1])
N_1 <-length(stratum_1)
N_1
stratum_0 <-c(data$intersect_ind[data$intersect_ind==0])
N_0 <-length(stratum_0)


#ovde biramo br jedinki iz svakog stratuma-prvo cemo uzeti po pola jedinki iz svakog stratuma i pogledati rezultate
n_1 <- round(N_1/2)
n_0 <- round(N_0/2)

set.seed(123)
#ovde vadimo uzorke BEZ PONAVLJANJA
uzorak_1 <- sample(stratum_1, n_1, replace = F)
uzorak_0 <- sample(stratum_0, n_0, replace = F)

#ocena totala
t_str <-
  (N_1 * mean(uzorak_1) + N_0 * mean(uzorak_0))
t_str

#i ispisujemo stvarnu vrednost totala
t <-sum(populacija)
#vidimo da je ocena totala jednaka totalu


#ocena disperzije dobijene ocene
S_2_1 <- var(uzorak_1)
S_2_0 <- var(uzorak_0)

ocena_D_t_str <-
  N_0 ^ 2 * S_2_0 / n_0 * (1 - n_0 / N_0) +
  N_1 ^ 2 * S_2_1 / n_1 * (1 - n_1 / N_1)

ocena_D_t_str
# ocena disperzije je ocekivano 0

n <-sum(n_1, n_0)

#ispisujemo rezultate u fajl
cat("Stratifikovani uzorak, uzorkovanje bez ponavljanja - intersect_ind:\nOcenjena vrednost:", file="~/R/StratifikovaniPresek.txt")
capture.output( t_str, file="~/R/StratifikovaniPresek.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/StratifikovaniPresek.txt", append=TRUE)
capture.output(t, file="~/R/StratifikovaniPresek.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/StratifikovaniPresek.txt", append=TRUE)
capture.output(n, file="~/R/StratifikovaniPresek.txt", append=TRUE)
cat("Ocena disperzije ocene: ", file="~/R/StratifikovaniPresek.txt", append=TRUE)
capture.output(ocena_D_t_str, file="~/R/StratifikovaniPresek.txt", append=TRUE)

#ovde nema potrebe da dalje povecavamo obim uzorka, jer je i 50% jedinki dovoljno precizno
# U OVOM SLUCAJU VIDIMO DA JE STRATIFIKOVAN UZORAK BOLJI OD PSU 