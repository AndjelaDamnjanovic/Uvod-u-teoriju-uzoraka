data<-read.csv("R/datasetClean.csv")

#stvarna vrednost nivoa neuredjenosti
t <-sum(data$disorder_content)

populacija<-c(data$disorder_content)
populacija

N <-length(populacija)
N

n <-10
set.seed(123)
uzorak<-sample(populacija, size=n, replace=TRUE)
uzorak

P_uzorka=1/N^n
P_uzorka

t_ocene<-N*mean(uzorak)

#ispisujemo predvidjenu vrednost
t_ocene

#ispisujemo i stvarnu vrednost
t

#ispisujemo dobijene rezultate u fajl
cat("Uzorkovanje sa ponavljanjem - stepen neuredjenosti:\nOcenjena vrednost:", file="~/R/psuSP.txt")
capture.output( N*mean(uzorak), file="~/R/psuSP.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/psuSP.txt", append=TRUE)
capture.output(t, file="~/R/psuSP.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/psuSP.txt", append=TRUE)
capture.output(n, file="~/R/psuSP.txt", append=TRUE)

#probajmo isto za malo vece n
n <-15
set.seed(123)
uzorak<-sample(populacija, size=n, replace=TRUE)
uzorak

P_uzorka=1/N^n
P_uzorka

t_ocene<-N*mean(uzorak)

#ispisujemo predvidjenu vrednost
t_ocene

#ispisujemo i stvarnu vrednost
t

#i ovo ispisujemo u fajl

cat("\nUzorkovanje sa ponavljanjem - stepen neuredjenosti:\nOcenjena vrednost:", file="~/R/psuSP.txt", append=TRUE)
capture.output( N*mean(uzorak), file="~/R/psuSP.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/psuSP.txt", append=TRUE)
capture.output(t, file="~/R/psuSP.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/psuSP.txt", append=TRUE)
capture.output(n, file="~/R/psuSP.txt", append=TRUE)

#uradimo isto i za ostala obelezja- prvo za indikator preseka
t <-sum(data$intersect_ind)

populacija<-c(data$intersect_ind)
populacija

N <-length(populacija)
N

n <-10
set.seed(123)
uzorak<-sample(populacija, size=n, replace=TRUE)
uzorak

P_uzorka=1/N^n
P_uzorka

t_ocene<-N*mean(uzorak)

#ispisujemo predvidjenu vrednost
t_ocene

#ispisujemo i stvarnu vrednost
t

#ispis u fajl
cat("\nUzorkovanje sa ponavljanjem - indikator preseka:\nOcenjena vrednost:", file="~/R/psuSP.txt", append=TRUE)
capture.output( N*mean(uzorak), file="~/R/psuSP.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/psuSP.txt", append=TRUE)
capture.output(t, file="~/R/psuSP.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/psuSP.txt", append=TRUE)
capture.output(n, file="~/R/psuSP.txt", append=TRUE)

#probajmo isto za malo vece n
n <-15
set.seed(123)
uzorak<-sample(populacija, size=n, replace=TRUE)
uzorak

P_uzorka=1/N^n
P_uzorka

t_ocene<-N*mean(uzorak)

#ispisujemo predvidjenu vrednost
t_ocene

#ispisujemo i stvarnu vrednost
t

#ispis
cat("\nUzorkovanje sa ponavljanjem - indikator preseka:\nOcenjena vrednost:", file="~/R/psuSP.txt", append=TRUE)
capture.output( N*mean(uzorak), file="~/R/psuSP.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/psuSP.txt", append=TRUE)
capture.output(t, file="~/R/psuSP.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/psuSP.txt", append=TRUE)
capture.output(n, file="~/R/psuSP.txt", append=TRUE)

#ponovimo isto i za indikator sadrzanja
t <-sum(data$contains_ind)

populacija<-c(data$contains_ind)
populacija

N <-length(populacija)
N

n <-10
set.seed(123)
uzorak<-sample(populacija, size=n, replace=TRUE)
uzorak

P_uzorka=1/N^n
P_uzorka

t_ocene<-N*mean(uzorak)

#ispisujemo predvidjenu vrednost
t_ocene

#ispisujemo i stvarnu vrednost
t

#ispis
cat("\nUzorkovanje sa ponavljanjem - indikator sadrzanja:\nOcenjena vrednost:", file="~/R/psuSP.txt", append=TRUE)
capture.output( N*mean(uzorak), file="~/R/psuSP.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/psuSP.txt", append=TRUE)
capture.output(t, file="~/R/psuSP.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/psuSP.txt", append=TRUE)
capture.output(n, file="~/R/psuSP.txt", append=TRUE)

#probajmo isto za malo vece n
n <-15
set.seed(123)
uzorak<-sample(populacija, size=n, replace=TRUE)
uzorak

P_uzorka=1/N^n
P_uzorka

t_ocene<-N*mean(uzorak)

#ispisujemo predvidjenu vrednost
t_ocene

#ispisujemo i stvarnu vrednost
t

#ispis
cat("\nUzorkovanje sa ponavljanjem - indikator sadrzanja:\nOcenjena vrednost:", file="~/R/psuSP.txt", append=TRUE)
capture.output( N*mean(uzorak), file="~/R/psuSP.txt", append=TRUE)
cat("Vrednost totala: ", file="~/R/psuSP.txt", append=TRUE)
capture.output(t, file="~/R/psuSP.txt", append=TRUE)
cat("Broj jedinki u uzorku: ", file="~/R/psuSP.txt", append=TRUE)
capture.output(n, file="~/R/psuSP.txt", append=TRUE)
