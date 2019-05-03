donne=read.table("adult.data", header=TRUE)
donne=donne[,-c(3)]
donne=donne[apply(donne[,c(1,2)],1,function (x) !is.na(x)),]
head(donne)
summary(donne)
str(donne)
#pairs(donne)

donne$age.<-as.numeric(donne$age.)
donne$capital.loss.<-as.numeric(donne$capital.loss.)
donne$capital.gain.<-as.numeric(donne$capital.gain.)
donne$hoursperweek.<-as.numeric(donne$hoursperweek.)
