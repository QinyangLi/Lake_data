########### Original code by Teodor Alling

options(encoding = "ISO-8859-1")
################## Packages used:
library(plyr)


################## Read .csv files:
colclass<-c(NA,"factor",rep(NA,3),"numeric","numeric",rep(NA,14),"numeric","numeric","factor","factor","character","factor","factor",NA,NA,"factor","character",rep("numeric",3),NA,NA,"factor","character","factor","factor")

cl1<-read.csv('../Raw_data/chloro2_1941-1957 - Copy.csv',header=TRUE, colClasses=colclass)
cl2<-read.csv('../Raw_data/chloro2_1958-1988 - Copy.csv',header=TRUE, colClasses=colclass)
cl3<-read.csv('../Raw_data/chloro2_1989-2019 - Copy.csv',header=TRUE, colClasses=colclass)

################## First we need to join the three files together:
Reduce(intersect, list(colnames(cl3),colnames(cl2),colnames(cl1))) #This shows us that the three dataframes share 41 variables. This is good since each has 41 variables in it.
vars_cl<-c(colnames(cl3),colnames(cl2),colnames(cl1))
length(unique(vars_cl)) #It's 41 unique columns among the three dfs.

#Check if all classes are the same for variables between the three data frames:
unlist(lapply(cl1, class))==unlist(lapply(cl2, class)) #yes
unlist(lapply(cl1, class))==unlist(lapply(cl3, class)) #yes
unlist(lapply(cl3, class))==unlist(lapply(cl2, class)) #and yes

#Join: 
rawchl<-rbind(cl1,cl2,cl3)

#Note, rawchl is the unedited chlorophyta data. Some columns are unnecessary and can be removed if desired.

chl<-rawchl # Make copy and start trimming the copy instead of rawchl.
View(chl)
summary(chl$Unders?kningstyp) #15153 data points for v?xtplankton i sj?ar

chloro1941_2019 <- chl
save(chloro1941_2019, file = "../RData_files/chloro1941_2019.RData")

# load("../RData_files/chloro1941_2019.RData")
# View(chloro1941_2019)
# write.csv(chl,"Dropbox (MEEL)/QinyangLi/PhD/LakeData/LakeData_Qinyang/processed_files/algae1941-2019.csv")
