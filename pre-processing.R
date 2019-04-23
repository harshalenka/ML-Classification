DataRaw <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/dermatology/dermatology.data", na.strings = c("?"))
names(DataRaw)<-c("erythema","scaling","definite.borders","itching","koebner.phenomenon","polygonal.papules","follicular.papules","oral.mucosal.involvement","knee.and.elbow.involvement","scalp.involvement","family.history","melanin.incontinence","eosinophils.in.the.infiltrate","PNL.infiltrate","fibrosis.of.the.papillary.dermis","exocytosis","acanthosis","hyperkeratosis","parakeratosis","clubbing.of.the.rete.ridges","elongation.of.the.rete.ridges","thinning.of.the.suprapapillary.epidermis","spongiform.pustule","munro.microabcess","focal.hypergranulosis","disappearance.of.the.granular.layer","vacuolisation.and.damage.of.basal.layer","spongiosis","saw.tooth.appearance.of.retes","follicular.horn.plug","perifollicular.parakeratosis","inflammatory.monoluclear.infiltrate","band.like.infiltrate","Age","class")

#Plotting

install.packages("mice")
library(mice)

install.packages("VIM")
library(mice)

aggr_plot <- aggr(DataRaw, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(DataRaw), cex.axis=.9, gap=1, ylab=c("Histogram of missing data","Pattern"))

DataRaw$Age[which(is.na(DataRaw$Age))]<-mean(na.omit(DataRaw[["Age"]]))

#correlation plot
install.packages("corrplot")
library(corrplot)
M <- cor(DataRaw[1:10])
corrplot(M, method="number")


# Data Distribution
hist(DataRaw$class, breaks=12, col="red")


#Separation of training set and target 
target<-DataRaw$class
DataRaw<-subset(DataRaw, select=-class)

#Preprocessing scaling data
install.packages("caret")
library(lattice)
library(ggplot2)
library(caret)
preprocessParams <- preProcess(DataRaw,method=c("scale"))
print(preprocessParams)
transformed<-predict(preprocessParams,DataRaw)
summary(transformed)
transformed$class<-target