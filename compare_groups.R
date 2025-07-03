library("readxl")

#Read in datasets
forregressiontest <- read_excel("forregressiontest.xlsx")
s4 <- read.csv("Table S4.csv") #synthetic

#Rename columns in S4
colnames(s4)[6] <- "D47ICDES"
colnames(s4)[8] <- "calcTiso"
colnames(s4)[7] <- "SE47"
colnames(s4)[9] <- "Terror"

#Categories from Figure 4
ben <- subset(d, Habitat=="Benthic, epifaunal"|Habitat=="Benthic, infaunal"|Habitat=="Benthic"|Habitat=="Benthic, epifaunal to shallow infaunal")
plan <- subset(d, Habitat!="Benthic, epifaunal"&Habitat!="Benthic, infaunal"&Habitat!="Benthic"&Habitat!="Benthic, epifaunal to shallow infaunal"&`Sample Name`!="CH75-18-16 G MENARDII") #plan
mixplan <- subset(d, Habitat=="Mixed-layer") #mix plan
mixben <- subset(d, Species=="Mixed benthic species") #mix ben
epiben <- subset(d, Habitat!="Benthic, epifaunal") #Epi Ben
infraben <- subset(d, Habitat=="Benthic, infaunal") # Infra Ben

#Additional categories from Figure 3
UCLA <- subset(d, Dataset=="UCLA"&`Sample Name`!="CH75-18-16 G MENARDII")
synthetic <- s4
allforams <- rbind(UCLA, ben, plan, mixplan, mixben, epiben, infraben)



