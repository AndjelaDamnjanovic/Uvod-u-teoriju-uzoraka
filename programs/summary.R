data<-read.csv("R/datasetClean.csv")

cat("Summary of the datasetClean:\n", file="~/R/summary.txt")
capture.output( summary(data), file="~/R/summary.txt", append=TRUE)

data<-read.csv("R/datasetCleanIndirect.csv")

cat("Summary of the datasetCleanIndirect:\n", file="~/R/summaryIndirect.txt")
capture.output( summary(data), file="~/R/summaryIndirect.txt", append=TRUE)
