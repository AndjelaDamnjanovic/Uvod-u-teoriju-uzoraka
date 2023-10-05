data=read.csv("R/datasetClean.csv")
populacija <- data$intersect_ind
N <-length(populacija)
n <- 314137
k=N/n
k

#nasumicno biramo jedan od brojeva
set.seed(123)
#izvlacimo jedinke u uzorak
sistematski_uzorci <- matrix(populacija, n, k, byrow = TRUE) # mogući uzorci su dati u kolonama
sistematski_uzorak <- sistematski_uzorci[, sample(1:ncol(sistematski_uzorci), 1)] # na slučajan način biramo
# jednu kolonu - uzorak
sistematski_uzorak

#ocena srednje vrednosti populacije
m_sis <- mean(sistematski_uzorak)
m_sis

#a sada ispisujemo i pravu srenju vrednost
m <-mean(populacija)
m
# vidimo da su ovde ocena i prava srednja vrednost bliske, kao i u prethodnom slucaju

#ispitujemo disperziju
sigma_2 <- var(as.numeric(sistematski_uzorci))
sigma_sis_2 <- 0
for(i in 1:k) {
  for (j in 1:n) {
    sigma_sis_2 <- sigma_sis_2 + (sistematski_uzorci[j, i] - mean(sistematski_uzorci[, i])) ^ 2
  }
}
sigma_sis_2 <- sigma_sis_2 / (k * (n - 1))
D_m_sis <- (N - 1) * sigma_2 / N - k * (n - 1) * sigma_sis_2 / N
D_m_sis
options(scipen=999)
#ispis u fajl
cat("Sistematsko uzorkovanje - indikator preseka:\nOcenjena vrednost:", file="~/R/sistematskiPresek.txt")
capture.output( m_sis, file="~/R/sistematskiPresek.txt", append=TRUE)
cat("Vrednost sredine: ", file="~/R/sistematskiPresek.txt", append=TRUE)
capture.output(m, file="~/R/sistematskiPresek.txt", append=TRUE)
cat("Disperzija sredine: ", file="~/R/sistematskiPresek.txt", append=TRUE)
capture.output(D_m_sis, file="~/R/sistematskiPresek.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/sistematskiPresek.txt", append=TRUE)
capture.output(length(sistematski_uzorak), file="~/R/sistematskiPresek.txt", append=TRUE)

#probajmo i ovde sa vecim uzorkom
n <- 628274
k=N/n
k

#nasumicno biramo jedan od brojeva
set.seed(123)
#izvlacimo jedinke u uzorak
sistematski_uzorci <- matrix(populacija, n, k, byrow = TRUE) # mogući uzorci su dati u kolonama
sistematski_uzorak <- sistematski_uzorci[, sample(1:ncol(sistematski_uzorci), 1)] # na slučajan način biramo
# jednu kolonu - uzorak
sistematski_uzorak

#ocena srednje vrednosti populacije
m_sis <- mean(sistematski_uzorak)
m_sis

#a sada ispisujemo i pravu srenju vrednost
m <-mean(populacija)
m
# opet dobijamo dosta bolje rezultate

#ispitujemo disperziju
sigma_2 <- var(as.numeric(sistematski_uzorci))
sigma_sis_2 <- 0
for(i in 1:k) {
  for (j in 1:n) {
    sigma_sis_2 <- sigma_sis_2 + (sistematski_uzorci[j, i] - mean(sistematski_uzorci[, i])) ^ 2
  }
}
sigma_sis_2 <- sigma_sis_2 / (k * (n - 1))
D_m_sis <- (N - 1) * sigma_2 / N - k * (n - 1) * sigma_sis_2 / N
D_m_sis
options(scipen=999)
#ispis u fajl
cat("\nSistematsko uzorkovanje - indikator preseka:\nOcenjena vrednost:", file="~/R/sistematskiPresek.txt", append=TRUE)
capture.output( m_sis, file="~/R/sistematskiPresek.txt", append=TRUE)
cat("Vrednost sredine: ", file="~/R/sistematskiPresek.txt", append=TRUE)
capture.output(m, file="~/R/sistematskiPresek.txt", append=TRUE)
cat("Disperzija sredine: ", file="~/R/sistematskiPresek.txt", append=TRUE)
capture.output(D_m_sis, file="~/R/sistematskiPresek.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/sistematskiPresek.txt", append=TRUE)
capture.output(length(sistematski_uzorak), file="~/R/sistematskiPresek.txt", append=TRUE)
