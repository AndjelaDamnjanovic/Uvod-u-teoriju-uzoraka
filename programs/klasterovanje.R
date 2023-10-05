data=read.csv("R/datasetClean.csv")
populacija <- data$disorder_content
N <-length(populacija)

# prvo delimo jedinke u nizove na osnovu njihove vrednosti, kao u slucaju kada smo imali
# stratifikovan uzorak
stratum_1 <-c()
stratum_2 <-c()
stratum_3 <-c()
stratum_4 <-c()
stratum_5 <-c()

#opet pokrecemo 2 for petlje da razvrstamo jedinke
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

#provera da li su sve jedinke rasporedjene tacno jednom
sum(length(stratum_1), length(stratum_2), length(stratum_3), length(stratum_4), length(stratum_5))

#pocnimo istrazivanje sa, recimo, 5 klastera, u nih cemo rasporediti jedinke koje smo
# dobili pri razdvajanju u stratume kako bi svaki klaster sto bolje oslikavao celu populaciju
# podelicemo jedinke tako da prvi klaster sadrzi 40% svih jedinki, drugi 30%, treci 20%, 
# a cetvrti i peti po 5%
klaster1 <- c(stratum_1[1: round(length(stratum_1)*0.4)])
klaster2 <- c(stratum_1[round((length(stratum_1)*0.4+1)) : round((length(stratum_1)*0.7))])
klaster3 <- c(stratum_1[round((length(stratum_1)*0.7+1)): round((length(stratum_1)*0.9))])
klaster4 <- c(stratum_1[round((length(stratum_1)*0.9+1)): round((length(stratum_1)*0.95))])
klaster5 <- c(stratum_1[round((length(stratum_1)*0.95+1)): (length(stratum_1))])

klaster1 <-append(klaster1, c(stratum_2[1: round(length(stratum_2)*0.4)]))
klaster1 <-append(klaster1, c(stratum_3[1: round(length(stratum_3)*0.4)]))
klaster1 <-append(klaster1, c(stratum_4[1: round(length(stratum_4)*0.4)]))
klaster1 <-append(klaster1, c(stratum_5[1: round(length(stratum_5)*0.4)]))

klaster2 <-append(klaster2, c(stratum_2[round((length(stratum_2)*0.4+1)) : round((length(stratum_2)*0.7))]))
klaster2 <-append(klaster2, c(stratum_3[round((length(stratum_3)*0.4+1)) : round((length(stratum_3)*0.7))]))
klaster2 <-append(klaster2, c(stratum_4[round((length(stratum_4)*0.4+1)) : round((length(stratum_4)*0.7))]))
klaster2 <-append(klaster2, c(stratum_5[round((length(stratum_5)*0.4+1)) : round((length(stratum_5)*0.7))]))

klaster3 <-append(klaster3, c(stratum_2[round((length(stratum_2)*0.7+1)): round((length(stratum_2)*0.9))]))
klaster3 <-append(klaster3, c(stratum_3[round((length(stratum_3)*0.7+1)): round((length(stratum_3)*0.9))]))
klaster3 <-append(klaster3, c(stratum_4[round((length(stratum_4)*0.7+1)): round((length(stratum_4)*0.9))]))
klaster3 <-append(klaster3, c(stratum_5[round((length(stratum_5)*0.7+1)): round((length(stratum_5)*0.9))]))

klaster4 <-append(klaster4, c(stratum_2[round((length(stratum_2)*0.9+1)): round((length(stratum_2)*0.95))]))
klaster4 <-append(klaster4, c(stratum_3[round((length(stratum_3)*0.9+1)): round((length(stratum_3)*0.95))]))
klaster4 <-append(klaster4, c(stratum_4[round((length(stratum_4)*0.9+1)): round((length(stratum_4)*0.95))]))
klaster4 <-append(klaster4, c(stratum_5[round((length(stratum_5)*0.9+1)): round((length(stratum_5)*0.95))]))

klaster5 <-append(klaster5, c(stratum_2[round((length(stratum_2)*0.95+1)): (length(stratum_2))]))
klaster5 <-append(klaster5, c(stratum_3[round((length(stratum_3)*0.95+1)): (length(stratum_3))]))
klaster5 <-append(klaster5, c(stratum_4[round((length(stratum_4)*0.95+1)): (length(stratum_4))]))
klaster5 <-append(klaster5, c(stratum_5[round((length(stratum_5)*0.95+1)): (length(stratum_5))]))
totali <-c(sum(klaster1), sum(klaster2), sum(klaster3), sum(klaster4), sum(klaster5))
klasteri <-c(length(klaster1), length(klaster2), length(klaster3), length(klaster4), length(klaster5))
#biramo koji ce klasteri uci u uzorak
M <- length(populacija) # ukupan br jedinki
N <- 5 # broj klastera
n <- 3 # broj klastera koje mi biramo
set.seed(123)
uzorak <-sample(N, n, replace = F)
uzorak
# racunamo total na svakom klasteru koji je izvucen u uzorak
t_l_na_uzorku <-c((totali[uzorak]))
t_l_na_uzorku


duzine <-c(length(klaster1), length(klaster2), length(klaster3), length(klaster4), length(klaster5))
M_l <- duzine[uzorak]
M_l

psi <- M_l / M # verovatnoće izbora klastera
pi <- 1 - (1 - psi) ^ n # verovatnoće uključenja klastera u uzorak klastera

#nalazimo ocenu totala Horvitz-Thompsonovom ocenom
t_Y_ht <-sum(totali[uzorak]/pi)
t_Y_ht

#i ispisujemo stvarnu vrednost
t <-sum(populacija)
t

#ocenjujemo total populacije ocenom PSU bez ponavljanja
t_ocena <- N * mean(t_l_na_uzorku)
t_ocena

#kao sto vidimo, ocena totala i stvarna vrednost totala su istog reda velicina, ali se ipak
# dosta razlikuju

#racunamo sada ocenu disperzije ocene psu bez ponavljanja
ocena_D_t_ocena <- N ^ 2 * (1 - n / N) * var(t_l_na_uzorku) / n
ocena_D_t_ocena

cat("Klasterovanje - neuredjenost proteina:\nOcenjena vrednost- Horvitz-Thompson:", file="~/R/klasterovanjeNeuredjenost.txt")
capture.output( t_Y_ht, file="~/R/klasterovanjeNeuredjenost.txt", append=TRUE)
cat( "Ocena totala- psu bez ponavljanja: ", file="~/R/klasterovanjeNeuredjenost.txt", append=TRUE)
capture.output(t_ocena, file="~/R/klasterovanjeNeuredjenost.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/klasterovanjeNeuredjenost.txt", append=TRUE)
capture.output(t, file="~/R/klasterovanjeNeuredjenost.txt", append=TRUE)
cat("Ocena disperzije ocene - psu bez ponavljanja: ", file="~/R/klasterovanjeNeuredjenost.txt", append=TRUE)
capture.output(ocena_D_t_ocena, file="~/R/klasterovanjeNeuredjenost.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/klasterovanjeNeuredjenost.txt", append=TRUE)
capture.output(sum(klasteri[uzorak]), file="~/R/klasterovanjeNeuredjenost.txt", append=TRUE)

#probajmo da drugacije rasporedimo jedinke u klastere, tj da svaki klaster bude iste velicine
klaster1 <- c(stratum_1[1: round(length(stratum_1)*0.2)])
klaster2 <- c(stratum_1[round((length(stratum_1)*0.2+1)) : round((length(stratum_1)*0.4))])
klaster3 <- c(stratum_1[round((length(stratum_1)*0.4+1)): round((length(stratum_1)*0.6))])
klaster4 <- c(stratum_1[round((length(stratum_1)*0.6+1)): round((length(stratum_1)*0.8))])
klaster5 <- c(stratum_1[round((length(stratum_1)*0.8+1)): (length(stratum_1))])

klaster1 <-append(klaster1, c(stratum_2[1: round(length(stratum_2)*0.2)]))
klaster1 <-append(klaster1, c(stratum_3[1: round(length(stratum_3)*0.2)]))
klaster1 <-append(klaster1, c(stratum_4[1: round(length(stratum_4)*0.2)]))
klaster1 <-append(klaster1, c(stratum_5[1: round(length(stratum_5)*0.2)]))

klaster2 <-append(klaster2, c(stratum_2[round((length(stratum_2)*0.2+1)) : round((length(stratum_2)*0.4))]))
klaster2 <-append(klaster2, c(stratum_3[round((length(stratum_3)*0.2+1)) : round((length(stratum_3)*0.4))]))
klaster2 <-append(klaster2, c(stratum_4[round((length(stratum_4)*0.2+1)) : round((length(stratum_4)*0.4))]))
klaster2 <-append(klaster2, c(stratum_5[round((length(stratum_5)*0.2+1)) : round((length(stratum_5)*0.4))]))

klaster3 <-append(klaster3, c(stratum_2[round((length(stratum_2)*0.4+1)): round((length(stratum_2)*0.6))]))
klaster3 <-append(klaster3, c(stratum_3[round((length(stratum_3)*0.4+1)): round((length(stratum_3)*0.6))]))
klaster3 <-append(klaster3, c(stratum_4[round((length(stratum_4)*0.4+1)): round((length(stratum_4)*0.6))]))
klaster3 <-append(klaster3, c(stratum_5[round((length(stratum_5)*0.4+1)): round((length(stratum_5)*0.6))]))

klaster4 <-append(klaster4, c(stratum_2[round((length(stratum_2)*0.6+1)): round((length(stratum_2)*0.8))]))
klaster4 <-append(klaster4, c(stratum_3[round((length(stratum_3)*0.6+1)): round((length(stratum_3)*0.8))]))
klaster4 <-append(klaster4, c(stratum_4[round((length(stratum_4)*0.6+1)): round((length(stratum_4)*0.8))]))
klaster4 <-append(klaster4, c(stratum_5[round((length(stratum_5)*0.6+1)): round((length(stratum_5)*0.8))]))

klaster5 <-append(klaster5, c(stratum_2[round((length(stratum_2)*0.8+1)): (length(stratum_2))]))
klaster5 <-append(klaster5, c(stratum_3[round((length(stratum_3)*0.8+1)): (length(stratum_3))]))
klaster5 <-append(klaster5, c(stratum_4[round((length(stratum_4)*0.8+1)): (length(stratum_4))]))
klaster5 <-append(klaster5, c(stratum_5[round((length(stratum_5)*0.8+1)): (length(stratum_5))]))
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

#ocenjujemo total
t_ocena <- N * mean(t_l_na_uzorku)
t_ocena

#i ispisujemo stvarnu vrednost totala
t <-sum(populacija)
t
#kao sto vidimo, rezultati su i dalje istog reda velicine, ali je sada razlika izmedju 
# stvarne i ocenjene vrednosti drasticno manja

#racunamo sada ocenu disperzije ocene psu bez ponavljanja
ocena_D_t_ocena <- N ^ 2 * (1 - n / N) * var(t_l_na_uzorku) / n
ocena_D_t_ocena

cat("\nKlasterovanje - neuredjenost proteina:\nOcenjena vrednost- psu bez ponavljanja:", file="~/R/klasterovanjeNeuredjenost.txt", append=TRUE)
capture.output( t_ocena, file="~/R/klasterovanjeNeuredjenost.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/klasterovanjeNeuredjenost.txt", append=TRUE)
capture.output(t, file="~/R/klasterovanjeNeuredjenost.txt", append=TRUE)
cat("Ocena disperzije ocene - psu bez ponavljanja: ", file="~/R/klasterovanjeNeuredjenost.txt", append=TRUE)
capture.output(ocena_D_t_ocena, file="~/R/klasterovanjeNeuredjenost.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/klasterovanjeNeuredjenost.txt", append=TRUE)
capture.output(sum(klasteri[uzorak]), file="~/R/klasterovanjeNeuredjenost.txt", append=TRUE)
