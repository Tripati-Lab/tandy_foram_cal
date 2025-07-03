library("readxl")
d <- read_excel("forregressiontest.xlsx")

#remove unused lines from dataframe
d <- d[!is.na(d$Dataset),]

#create more subgroups main one is Habitat2
d$Habitat2 <- ifelse(grepl('Benthic',d$Habitat), "Benthic", "Not Benthic")
d$NonBrei <- ifelse(grepl('Breitenbach',d$Dataset), "Breitenbach", "Not Breitenbach")
d$NonPia <- ifelse(grepl('Piasecki',d$Dataset), "Piasecki", "Not Piasecki")
d$NonMein <- ifelse(grepl('Meinicke',d$Dataset), "Meinicke", "Not Meinicke")
d$NonPeral <- ifelse(grepl('Peral',d$Dataset), "Peral", "Not Peral")
d$NonUCLA <- ifelse(grepl('UCLA',d$Dataset), "UCLA", "Not UCLA")
d$NonTrip <- ifelse(grepl('Tripati',d$Dataset), "Tripati", "Not Tripati")
d$BreiTripUCLA <- ifelse(grepl('Tripati',d$Dataset), "Tripati", "Not Tripati")


#new regressions that have extra hypothesis testing data
#ex below is looking at T vs D47 seeing what datasets are a significant predictor of D47 using UCLA in Dataset as the main case intercept, 
#change UCLA to Brietenbach etc for other cases as the main reference, it asks whether the other regression lines are the same or different 
#from UCLA, same as hypothesis testing
bylab <- lm(D47ICDES ~ calcTiso+relevel(factor(Dataset), ref = "UCLA"), data=subset(d, `Sample Name`!="CH75-18-16 G MENARDII"&Dataset!="Piasecki 2020"))
#prints the information with P values etc. 
summary(bylab)
byhab <- lm(D47ICDES ~ calcTiso+relevel(factor(Habitat), ref = "Benthic, infaunal"), data=subset(d, `Sample Name`!="CH75-18-16 G MENARDII"))
summary(byhab)
#dont have to set the relevel as there are only two cases in this test (benthic or non benthic)
byhab2 <- lm(D47ICDES ~ calcTiso+Habitat2, data=subset(d, `Sample Name`!="CH75-18-16 G MENARDII"))
summary(byhab2)
byocean <- lm(D47ICDES ~ calcTiso+relevel(factor(Ocean), ref = "Indian Ocean"), data=subset(d, `Sample Name`!="CH75-18-16 G MENARDII"))
summary(byocean)
bycarbsat <- lm(D47ICDES ~ calcTiso+carbsat, data=subset(d, `Sample Name`!="CH75-18-16 G MENARDII"))
summary(bycarbsat)
byspecies <- lm(D47ICDES ~ calcTiso+relevel(factor(Species), ref = "Cibicidoides"), data=subset(d, `Sample Name`!="CH75-18-16 G MENARDII"))
summary(byspecies)


byNonBrei <- lm(D47ICDES ~ calcTiso+NonBrei, data=subset(d, `Sample Name`!="CH75-18-16 G MENARDII"))
summary(byNonBrei)
byNonPia <- lm(D47ICDES ~ calcTiso+NonPia, data=subset(d, `Sample Name`!="CH75-18-16 G MENARDII"))
summary(byNonPia)
byNonMein <- lm(D47ICDES ~ calcTiso+NonMein, data=subset(d, `Sample Name`!="CH75-18-16 G MENARDII"))
summary(byNonMein)
byNonPeral <- lm(D47ICDES ~ calcTiso+NonPeral, data=subset(d, `Sample Name`!="CH75-18-16 G MENARDII"))
summary(byNonPeral)
byNonUCLA <- lm(D47ICDES ~ calcTiso+NonUCLA, data=subset(d, `Sample Name`!="CH75-18-16 G MENARDII"))
summary(byNonUCLA)
byNonTrip <- lm(D47ICDES ~ calcTiso+NonTrip, data=subset(d, `Sample Name`!="CH75-18-16 G MENARDII"))
summary(byNonTrip)


#this was me testing to see if there was a DCO3 value at which two different regression lines could be 
#comfortably made. I was unsuccessful here as the data is too scattered with no strong transition depth
library(segmented)

my.seg <- segmented(bycarbsat, seg.Z = ~ carbsat, psi = list(carbsat = c(0, 21)))

summary(my.seg)
my.seg$psi
slope(my.seg)


factor( factor(d$Dataset), ordered = FALSE )




