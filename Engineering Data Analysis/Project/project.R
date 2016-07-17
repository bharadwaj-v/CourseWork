crimecrimedataraw <-read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/00211/CommViolPredUnnormalizedData.txt",
                        header = FALSE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "",
                        stringsAsFactors = default.stringsAsFactors())
crime <-read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/00211/CommViolPredUnnormalizedData.txt",
                            header = FALSE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "",
                            na.strings="?",strip.white=TRUE,stringsAsFactors = default.stringsAsFactors())
#header of the dataset
header<-c('communityname','State','countyCode','communityCode','fold','pop','perHoush','pctBlack','pctWhite','pctAsian','pctHisp','pct12-21','pct12-29','pct16-24','pct65up','persUrban','pctUrban','medIncome','pctWwage','pctWfarm','pctWdiv','pctWsocsec','pctPubAsst','pctRetire','medFamIncome','perCapInc','whitePerCap','blackPerCap','NAperCap','asianPerCap','otherPerCap','hispPerCap','persPoverty','pctPoverty','pctLowEdu','pctNotHSgrad','pctCollGrad','pctUnemploy','pctEmploy','pctEmployMfg','pctEmployProfServ','pctOccupManu','pctOccupMgmt','pctMaleDivorc','pctMaleNevMar','pctFemDivorc','pctAllDivorc','persPerFam','pct2Par','pctKids2Par','pctKids-4w2Par','pct12-17w2Par','pctWorkMom-6','pctWorkMom-18','kidsBornNevrMarr','pctKidsBornNevrMarr','numForeignBorn','pctFgnImmig-3','pctFgnImmig-5','pctFgnImmig-8','pctFgnImmig-10','pctImmig-3','pctImmig-5','pctImmig-8','pctImmig-10','pctSpeakOnlyEng','pctNotSpeakEng','pctLargHousFam','pctLargHous','persPerOccupHous','persPerOwnOccup','persPerRenterOccup','pctPersOwnOccup','pctPopDenseHous','pctSmallHousUnits','medNumBedrm','houseVacant','pctHousOccup','pctHousOwnerOccup','pctVacantBoarded','pctVacant6up','medYrHousBuilt','pctHousWOphone','pctHousWOplumb','ownHousLowQ','ownHousMed','ownHousUperQ','ownHousQrange','rentLowQ','rentMed','rentUpperQ','rentQrange','medGrossRent','medRentpctHousInc','medOwnCostpct','medOwnCostPctWO','persEmergShelt','persHomeless','pctForeignBorn','pctBornStateResid','pctSameHouse-5','pctSameCounty-5','pctSameState-5','numPolice','policePerPop','policeField','policeFieldPerPop','policeCalls','policCallPerPop','policCallPerOffic','policePerPop2','racialMatch','pctPolicWhite','pctPolicBlack','pctPolicHisp','pctPolicMinority','pctPolicAsian','officDrugUnits','numDiffDrugsSeiz','policAveOT','landArea','popDensity','pctUsePubTrans','policCarsAvail','policOperBudget','pctPolicPatrol','gangUnit','pctOfficDrugUnit','policBudgetPerPop','murders','murdPerPop','rapes','rapesPerPop','robberies','robbbPerPop','assaults','assaultPerPop','burglaries','burglPerPop','larcenies','larcPerPop','autoTheft','autoTheftPerPop','arsons','arsonsPerPop','violentPerPop','nonViolPerPop')
#assigning header to the dataset
names(crime)<-header
#checking the number of missing values in each column
numberOfNA<-colSums(is.na(crime))

#removed variables that have more than 1000 NA's
crimeNew<-crime[,c(-3,-4,-104:-120,-124:-127,-129)] # consists of 123 variables

#removing records that have missing values
crimeNonNA<-crimeNew[complete.cases(crimeNew),] # consists of 1901 records
crimeNonNA<-crimeNonNA[,c(-1,-2,-3)]

pr.out<-prcomp (crimeNonNA[,c(-1,-2,-3)], scale =TRUE)
loadinMat<-pr.out$rotation
pr.var =pr.out$sdev ^2
pve=pr.var/sum(pr.var )
plot(pve , xlab=" Principal Component ", ylab=" Proportion of Variance Explained ", ylim=c(0,1) ,type="b")
plot(cumsum (pve ), xlab=" Principal Component ", ylab =" Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,type="b")
install.packages("leaps")
crime.fwd<-regsubsets(violentPerPop~.-communityname-State-fold,data=crimeNonNA,nvmax=20,method="forward")

remove<-c (22, 165, 1594,989)
crimeNew1<-crimeNew[c (-22,-165, -1594, -989),]
crimeNonNA1<-crimeNew1[complete.cases(crimeNew1),] # consists of 1901 records
crimeNonNA1<-crimeNonNA1[,c(-1,-2,-3)]
