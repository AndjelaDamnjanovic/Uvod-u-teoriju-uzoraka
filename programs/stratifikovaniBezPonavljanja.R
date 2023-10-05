data<-read.csv("R/datasetClean.csv")
populacija <- c(data$disorder_content)
N <-length(populacija)

# prvo delimo podatke na stratume
#podelicemo ih prema vrednosti prvo u 5 stratuma:
# prvi stratum ce biti vrednosti        [0,0.2)
# drugi stratum ce obuhvatati vrednosti [0.2, 0.4)
# treci stratom ce sadryati vrednosti   [0.4, 0.6)
# cetvrti stratum                       [0.6, 0.8)
# peti stratum                          [0.8,1]

stratum_1 <-c()
stratum_2 <-c()
stratum_3 <-c()
stratum_4 <-c()
stratum_5 <-c()

#ponovo delimo petlju na 2 dela, jednoj petlji treba oko 30min da se izvrsi
for (i in 1: (N/2)){
  current_val=data$disorder_content[i]
  if (current_val<0.2){
    stratum_1 <-append(stratum_1, current_val)
  }else if(current_val>=0.2 && current_val<0.4){
    stratum_2 <-append(stratum_2, current_val)
  }else if(current_val>=0.4 && current_val<0.6){
    stratum_3 <-append(stratum_3, current_val)
  }else if(current_val>=0.6 && current_val<0.8){
    stratum_4 <-append(stratum_4, current_val)
  }else{
    stratum_5 <-append(stratum_5, current_val)
  }
}

for (i in (N/2+1):N){
  current_val=data$disorder_content[i]
  if (current_val<0.2){
    stratum_1 <-append(stratum_1, current_val)
  }else if(current_val>=0.2 && current_val<0.4){
    stratum_2 <-append(stratum_2, current_val)
  }else if(current_val>=0.4 && current_val<0.6){
    stratum_3 <-append(stratum_3, current_val)
  }else if(current_val>=0.6 && current_val<0.8){
    stratum_4 <-append(stratum_4, current_val)
  }else{
    stratum_5 <-append(stratum_5, current_val)
  }
}

N_1 <- length(stratum_1)
N_2 <- length(stratum_2)
N_3 <- length(stratum_3)
N_4 <- length(stratum_4)
N_5 <- length(stratum_5)

#ovde biramo br jedinki iz svakog stratuma-prvo cemo uzeti po pola jedinki iz svakog stratuma i pogledati rezultate
n_1 <- round(N_1/2)
n_2 <- round(N_2/2)
n_3 <- round(N_3/2)
n_4 <- round(N_4/2)
n_5 <- round(N_5/2)

set.seed(234)
#ovde vadimo uzorke BEZ PONAVLJANJA
uzorak_1 <- sample(stratum_1, n_1, replace = F)
set.seed(234)
uzorak_2 <- sample(stratum_2, n_2, replace = F)
set.seed(234)
uzorak_3 <- sample(stratum_3, n_3, replace = F)
set.seed(234)
uzorak_4 <- sample(stratum_4, n_4, replace = F)
set.seed(234)
uzorak_5 <- sample(stratum_5, n_5, replace = F)

#ocena totala
t_str <-
  (N_1 * mean(uzorak_1) + N_2 * mean(uzorak_2)+ N_3*mean(uzorak_3)+ N_4*mean(uzorak_4)+N_5*mean(uzorak_5))
t_str

#i ispisujemo stvarnu vrednost totala
t <-sum(populacija)
t
#vidimo da je ocena totala veoma blizu stvarnoj vrednosti


#ocena disperzije dobijene ocene
S_2_1 <- var(uzorak_1)
S_2_2 <- var(uzorak_2)
S_2_3 <- var(uzorak_3)
S_2_4 <- var(uzorak_4)
S_2_5 <- var(uzorak_5)

ocena_D_t_str <-
  N_1 ^ 2 * S_2_1 / n_1 * (1 - n_1 / N_1) +
  N_2 ^ 2 * S_2_2 / n_2 * (1 - n_2 / N_2) +
  N_3 ^ 2 * S_2_3 / n_3 * (1 - n_3 / N_3) +
  N_4 ^ 2 * S_2_4 / n_4 * (1 - n_4 / N_4) +
  N_5 ^ 2 * S_2_5 / n_5 * (1 - n_5 / N_5) 
  
ocena_D_t_str

#ispis u fajl
cat("Stratifikovano uzorkovanje - neuredjenost proteina:\nOcenjena vrednost:", file="~/R/stratifikovaniNeuredjenost.txt")
capture.output( t_str, file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
capture.output(t, file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
cat("Ocena disperzije ocene: ", file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
capture.output(ocena_D_t_str, file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
capture.output(sum(n_1, n_2, n_3, n_4, n_5), file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)

#posto smo mogli da vidimo da su vrednost totala i ocena totala istog reda velicine i veoma
# bliske, pokusacemo da bolju ocenu dobijemo tako sto cemo povecati broj jedinki koji
# se uzima iz svakog stratuma

#ovde biramo br jedinki iz svakog stratuma-uzmimo sada, na primer, 75% jedinki iz svakog stratuma
n_1 <- round(N_1* (3/4))
n_2 <- round(N_2* (3/4))
n_3 <- round(N_3* (3/4))
n_4 <- round(N_4* (3/4))
n_5 <- round(N_5* (3/4))

set.seed(234)
#ovde vadimo uzorke BEZ PONAVLJANJA
uzorak_1 <- sample(stratum_1, n_1, replace = F)
set.seed(234)
uzorak_2 <- sample(stratum_2, n_2, replace = F)
set.seed(234)
uzorak_3 <- sample(stratum_3, n_3, replace = F)
set.seed(234)
uzorak_4 <- sample(stratum_4, n_4, replace = F)
set.seed(234)
uzorak_5 <- sample(stratum_5, n_5, replace = F)

#ocena totala
t_str <-
  (N_1 * mean(uzorak_1) + N_2 * mean(uzorak_2)+ N_3*mean(uzorak_3)+ N_4*mean(uzorak_4)+N_5*mean(uzorak_5))
t_str

#i ispisujemo stvarnu vrednost totala
t <-sum(populacija)
t
#vidimo da smo dobili malo losiju ocenu, ali opet dovoljno blizu

#ocena disperzije dobijene ocene
S_2_1 <- var(uzorak_1)
S_2_2 <- var(uzorak_2)
S_2_3 <- var(uzorak_3)
S_2_4 <- var(uzorak_4)
S_2_5 <- var(uzorak_5)

ocena_D_t_str <-
  N_1 ^ 2 * S_2_1 / n_1 * (1 - n_1 / N_1) +
  N_2 ^ 2 * S_2_2 / n_2 * (1 - n_2 / N_2) +
  N_3 ^ 2 * S_2_3 / n_3 * (1 - n_3 / N_3) +
  N_4 ^ 2 * S_2_4 / n_4 * (1 - n_4 / N_4) +
  N_5 ^ 2 * S_2_5 / n_5 * (1 - n_5 / N_5) 

ocena_D_t_str

#ispis u fajl
cat("\nStratifikovano uzorkovanje - neuredjenost proteina:\nOcenjena vrednost:", file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
capture.output( t_str, file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
capture.output(t, file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
cat("Ocena disperzije ocene: ", file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
capture.output(ocena_D_t_str, file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
capture.output(sum(n_1, n_2, n_3, n_4, n_5), file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)

# probajmo jos malo da uvecamo procenat jedinki koje ulaze u izbor
n_1 <- round(N_1* 0.9)
n_2 <- round(N_2* 0.9)
n_3 <- round(N_3* 0.9)
n_4 <- round(N_4* 0.9)
n_5 <- round(N_5* 0.9)

set.seed(234)
#ovde vadimo uzorke BEZ PONAVLJANJA
uzorak_1 <- sample(stratum_1, n_1, replace = F)
set.seed(234)
uzorak_2 <- sample(stratum_2, n_2, replace = F)
set.seed(234)
uzorak_3 <- sample(stratum_3, n_3, replace = F)
set.seed(234)
uzorak_4 <- sample(stratum_4, n_4, replace = F)
set.seed(234)
uzorak_5 <- sample(stratum_5, n_5, replace = F)

#ocena totala
t_str <-
  (N_1 * mean(uzorak_1) + N_2 * mean(uzorak_2)+ N_3*mean(uzorak_3)+ N_4*mean(uzorak_4)+N_5*mean(uzorak_5))
t_str

#i ispisujemo stvarnu vrednost totala
t<-sum(populacija)
t
# rezultati se i dalje poboljsavaju
#ocena disperzije dobijene ocene
S_2_1 <- var(uzorak_1)
S_2_2 <- var(uzorak_2)
S_2_3 <- var(uzorak_3)
S_2_4 <- var(uzorak_4)
S_2_5 <- var(uzorak_5)

ocena_D_t_str <-
  N_1 ^ 2 * S_2_1 / n_1 * (1 - n_1 / N_1) +
  N_2 ^ 2 * S_2_2 / n_2 * (1 - n_2 / N_2) +
  N_3 ^ 2 * S_2_3 / n_3 * (1 - n_3 / N_3) +
  N_4 ^ 2 * S_2_4 / n_4 * (1 - n_4 / N_4) +
  N_5 ^ 2 * S_2_5 / n_5 * (1 - n_5 / N_5) 

ocena_D_t_str

#ispis u fajl

cat("\nStratifikovano uzorkovanje - neuredjenost proteina:\nOcenjena vrednost:", file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
capture.output( t_str, file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
capture.output(t, file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
cat("Ocena disperzije ocene: ", file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
capture.output(ocena_D_t_str, file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
capture.output(sum(n_1, n_2, n_3, n_4, n_5), file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)

# da bismo mogli da uporedimo rezultate sa PSU bez ponavljanja, pokusacemo da uzorkujemo tako
# da suma izabranih jedinki iz svih strtauma bude n=1884760 (najmanja vrednost za n koju
# smo uzeli za psu)
n_2 <- round(N_2* 0.9999)
n_3 <- round(N_3* 0.9999)
n_4 <- round(N_4* 0.9999)
n_5 <- round(N_5* 0.9999)
n_1 <- round(1884760- sum(N_2, N_3, N_4, N_5))

set.seed(234)
#ovde vadimo uzorke BEZ PONAVLJANJA
uzorak_1 <- sample(stratum_1, n_1, replace = F)
set.seed(234)
uzorak_2 <- sample(stratum_2, n_2, replace = F)
set.seed(234)
uzorak_3 <- sample(stratum_3, n_3, replace = F)
set.seed(234)
uzorak_4 <- sample(stratum_4, n_4, replace = F)
set.seed(234)
uzorak_5 <- sample(stratum_5, n_5, replace = F)

#ocena totala
t_str <-
  (N_1 * mean(uzorak_1) + N_2 * mean(uzorak_2)+ N_3*mean(uzorak_3)+ N_4*mean(uzorak_4)+N_5*mean(uzorak_5))
t_str

#i ispisujemo stvarnu vrednost totala
t <-sum(populacija)
t
# ocena je sada veoma blizu stvarne vrednosti, ali opet je losija u odnosu na psu
# bez ponavljanja


#ocena disperzije dobijene ocene
S_2_1 <- var(uzorak_1)
S_2_2 <- var(uzorak_2)
S_2_3 <- var(uzorak_3)
S_2_4 <- var(uzorak_4)
S_2_5 <- var(uzorak_5)

ocena_D_t_str <-
  N_1 ^ 2 * S_2_1 / n_1 * (1 - n_1 / N_1) +
  N_2 ^ 2 * S_2_2 / n_2 * (1 - n_2 / N_2) +
  N_3 ^ 2 * S_2_3 / n_3 * (1 - n_3 / N_3) +
  N_4 ^ 2 * S_2_4 / n_4 * (1 - n_4 / N_4) +
  N_5 ^ 2 * S_2_5 / n_5 * (1 - n_5 / N_5) 

ocena_D_t_str

# vidimo da se ovde ocene i stvarna vrednost totala poklapaju
cat("\nStratifikovano uzorkovanje - neuredjenost proteina:\nOcenjena vrednost:", file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
capture.output( t_str, file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
capture.output(t, file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
cat("Ocena disperzije ocene: ", file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
capture.output(ocena_D_t_str, file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)
capture.output(sum(n_1, n_2, n_3, n_4, n_5), file="~/R/stratifikovaniNeuredjenost.txt", append=TRUE)

