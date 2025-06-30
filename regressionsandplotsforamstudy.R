#before loading in datafile rename headers with symbols or long ones so they are easier to call, for names used see below
#load in files by importing foram dataset and anderson datasets
#renaming loaded foram file for easier call
d <- (forregressiontest)

#anderson data temperature into T^2
andersontemp <- (10^6 / ((andersondata$`Temperature �` +273.15)^2))
#calculate linear regression through anderson data
anderson <- lm(andersondata$`D47 I-CDES90` ~ andersontemp)
#look at the anderson linear regression info
print(anderson)

#all data regression and calls to view regression and stats, excluding menardii outlier
thisstudy <- lm(D47ICDES ~ calcTiso, data = d, `Sample Name`!="CH75-18-16 G MENARDII")
#create more subgroups to split up just benthic
d$Habitat2 <- ifelse(grepl('Benthic',d$Habitat), "Benthic", "Not Benthic")


#regressions through the different oceans and excluding different oceans
pacific <- lm(D47ICDES ~ calcTiso, data=subset(d, Ocean=="Pacific Ocean"))
notpacific <- lm(D47ICDES ~ calcTiso, data=subset(d, Ocean!="Pacific Ocean"&`Sample Name`!="CH75-18-16 G MENARDII"))
atlantic <- lm(D47ICDES ~ calcTiso, data=subset(d, Ocean=="Atlantic Ocean"&`Sample Name`!="CH75-18-16 G MENARDII"))
notatlantic <- lm(D47ICDES ~ calcTiso, data=subset(d, Ocean!="Atlantic Ocean"))
arctic <- lm(D47ICDES ~ calcTiso, data=subset(d, Ocean=="Arctic Ocean"))
notarctic <- lm(D47ICDES ~ calcTiso, data=subset(d, Ocean!="Arctic Ocean"&`Sample Name`!="CH75-18-16 G MENARDII"))
indian <- lm(D47ICDES ~ calcTiso, data=subset(d, Ocean=="Indian Ocean"))
notindian <- lm(D47ICDES ~ calcTiso, data=subset(d, Ocean!="Indian Ocean"&`Sample Name`!="CH75-18-16 G MENARDII"))
gulf <- lm(D47ICDES ~ calcTiso, data=subset(d, Ocean=="Gulf of Mexico"))
notgulf <- lm(D47ICDES ~ calcTiso, data=subset(d, Ocean!="Gulf of Mexico"&`Sample Name`!="CH75-18-16 G MENARDII"))
medsea <- lm(D47ICDES ~ calcTiso, data=subset(d, Ocean=="Mediterranean Sea"))
notmedsea <- lm(D47ICDES ~ calcTiso, data=subset(d, Ocean!="Mediterranean Sea"&`Sample Name`!="CH75-18-16 G MENARDII"))

#create a subset of the main dataframe that is just UCLA data excluing menardii outlier
UCLAData <- subset(d, Dataset=="UCLA"&`Sample Name`!="CH75-18-16 G MENARDII") #Dataset=="Tripati 2010"

#regressions by lab and excluding lab
UCLA <- lm(D47ICDES ~ calcTiso, data=subset(d, Dataset=="UCLA"&`Sample Name`!="CH75-18-16 G MENARDII"))
notUCLA <- lm(D47ICDES ~ calcTiso, data=subset(d, Dataset!="UCLA"&`Sample Name`!="CH75-18-16 G MENARDII"))
tripati <- lm(D47ICDES ~ calcTiso, data=subset(d, Dataset=="Tripati 2010"&`Sample Name`!="CH75-18-16 G MENARDII"))
nottripati<- lm(D47ICDES ~ calcTiso, data=subset(d, Dataset!="Tripati 2010"&`Sample Name`!="CH75-18-16 G MENARDII"))
peral <- lm(D47ICDES ~ calcTiso, data=subset(d, Dataset=="Peral 2018"))
notperal <- lm(D47ICDES ~ calcTiso, data=subset(d, Dataset!="Peral 2018"&`Sample Name`!="CH75-18-16 G MENARDII"))
meinicke <- lm(D47ICDES ~ calcTiso, data=subset(d, Dataset=="Meinicke 2019"))  
notmeinicke <- lm(D47ICDES ~ calcTiso, data=subset(d, Dataset!="Meinicke 2019"&`Sample Name`!="CH75-18-16 G MENARDII"))  
Breitenbach <- lm(D47ICDES ~ calcTiso, data=subset(d, Dataset=="Breitenbach 2018"))
notBreitenbach <- lm(D47ICDES ~ calcTiso, data=subset(d, Dataset!="Breitenbach 2018"&`Sample Name`!="CH75-18-16 G MENARDII"))
Piasecki <- lm(D47ICDES ~ calcTiso, data=subset(d, Dataset=="Piasecki 2020"))
notPiasecki <- lm(D47ICDES ~ calcTiso, data=subset(d, Dataset!="Piasecki 2020"&`Sample Name`!="CH75-18-16 G MENARDII"))
perandmein <- lm(D47ICDES ~ calcTiso, data=subset(d, Dataset=="Peral 2018"|Dataset=="Meinicke 2019"))
notperandmein <- lm(D47ICDES ~ calcTiso, data=subset(d, Dataset!="Peral 2018"&Dataset!="Meinicke 2019"&`Sample Name`!="CH75-18-16 G MENARDII"))

#carbonate saturation regressions by DCO3 horizon (over and under 21, and 0)
carbover21 <- lm(D47ICDES ~ calcTiso, data=subset(d, carbsat>=21.5&`Sample Name`!="CH75-18-16 G MENARDII"))
carbunder21 <- lm(D47ICDES ~ calcTiso, data=subset(d, carbsat<=21.5))
carbover0 <- lm(D47ICDES ~ calcTiso, data=subset(d, carbsat>=0&`Sample Name`!="CH75-18-16 G MENARDII"))
carbunder0 <- lm(D47ICDES ~ calcTiso, data=subset(d, carbsat<=0))

#regressions by habitat, excluding indiviual benthic habitats
benthic <- lm(D47ICDES ~ calcTiso, data=subset(d, Habitat=="Benthic, epifaunal"|Habitat=="Benthic, infaunal"|Habitat=="Benthic"|Habitat=="Benthic, epifaunal to shallow infaunal"))
notbenthic <- lm(D47ICDES ~ calcTiso, data=subset(d, Habitat!="Benthic, epifaunal"&Habitat!="Benthic, infaunal"&Habitat!="Benthic"&Habitat!="Benthic, epifaunal to shallow infaunal"&`Sample Name`!="CH75-18-16 G MENARDII"))
mixed <- lm(D47ICDES ~ calcTiso, data=subset(d, Habitat=="Mixed-layer"))   
notmixed <- lm(D47ICDES ~ calcTiso, data=subset(d, Habitat!="Mixed-layer"&`Sample Name`!="CH75-18-16 G MENARDII"))  
thermocline <- lm(D47ICDES ~ calcTiso, data=subset(d, Habitat=="Thermocline"&`Sample Name`!="CH75-18-16 G MENARDII"))
notthermocline <- lm(D47ICDES ~ calcTiso, data=subset(d, Habitat!="Thermocline"))

#type in name of regression in print and summary to see info about them
print(thisstudy)
summary(carbover21)

#compute confidence intervals for linear regressions
confint(carbover21, level=0.95)

#load some libraries for plotting, if you do not have installed, will need to be installed using: install.packages("")
library(ggplot2)
library(patchwork)
library(viridis)
library(rcartocolor)

#plots of full datasets in subgroups
#name of plot <-ggplot(nameofdataframe, aes(x=xvariable, y = y variable))
habitatplot <- ggplot(UCLAData, aes(x=calcTiso, y=D47ICDES)) +
  #plot line through your data subset 
  geom_smooth(data = subset(UCLAData, `Sample Name`!="CH75-18-16 G MENARDII"), method = lm,fullrange=TRUE, color = 'black', fill='grey74')+
  #add errorbars of your data
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  #Add second axis for temperature reference
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  #plot points and define how you want your color subset (in this case by habitat)
  geom_point(data = subset(UCLAData, `Sample Name`!="CH75-18-16 G MENARDII"), aes(x=calcTiso, y=D47ICDES, color=Habitat), size = 2) +
  #can play around with different color palettes, these are already made ones but you can also define your own!
  #scale_color_viridis(discrete = TRUE, option = "D")+
  #scale_color_brewer(palette = "Paired")
  scale_color_carto_d(palette = "Safe")+
  #many general graph themes available but this one is very clean
  theme_bw()+
  #add labels
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= expression(Delta[47]~ 'with Temperature - Habitat')) +
  #resize labels
  theme(plot.title = element_text(hjust=0.5, face='bold'))

oceanplot <- ggplot(d, aes(x=calcTiso, y=D47ICDES)) +
  geom_smooth(method = lm, fullrange=TRUE, color = 'black', fill='grey74')+
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  geom_point(data = d, aes(x=calcTiso, y=D47ICDES, color=Ocean), size = 2) +
  scale_color_carto_d(palette = "Safe")+
  theme_bw()+
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= expression(Delta[47]~ 'with Temperature - Ocean')) +
  theme(plot.title = element_text(hjust=0.5, face='bold'))

#same as above except instead of color as habitat, color is carbsat and shape is whether it is benthic or not!
carbplot <- ggplot(d, aes(x=calcTiso, y=D47ICDES)) +
  geom_smooth(data = subset(d, `Sample Name`!="CH75-18-16 G MENARDII"), method = lm, fullrange=TRUE, color = 'black', fill='grey74')+
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  #seperated points as the full range makes the carbsat auto color spread basically black and yellow 
  geom_point(data = d, aes(x=calcTiso, y=D47ICDES, color=carbsat, shape=Habitat2), size = 2) +
  #scale_colour_gradient2(limits = c(-35, 80))+
  scale_color_viridis(name = "Carbonate Ion \nSaturation", option = "B",limits = c(-35, 80))+
  #continued data that made scale funky
  geom_point(data = subset(d, carbsat>=80), aes(x=calcTiso, y=D47ICDES, shape=Habitat2), color = 'yellow', size = 3) +
  theme_bw()+
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= expression(Delta[47]~ 'with Temperature')) +
  theme(plot.title = element_text(hjust=0.5, face='bold'))

datasetplot <- ggplot(d, aes(x=calcTiso, y=D47ICDES)) +
  geom_smooth(method = lm, fullrange=TRUE, color = 'black', fill='grey74')+
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  geom_point(data = d, aes(x=calcTiso, y=D47ICDES, color=Dataset), size = 2) +
  scale_color_carto_d(palette = "Safe")+
  theme_bw()+
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= expression(Delta[47]~ 'with Temperature - Dataset')) +
  theme(plot.title = element_text(hjust=0.5, face='bold'))

#patchwork allows for figure arrangement but I've been playing around with other libraries too
carbplot+habitatplot+oceanplot+datasetplot+
  plot_annotation(tag_levels = 'A', title = expression(Delta[47]~ 'with Temperature'), theme = theme(plot.title = element_text(hjust = 0.5)))

#can just call the graphs if you want them plotted like below
datasetplot
habitatplot

#############
#now onto some 13 and 18 graphs
#basic 13C plot vs D47 with color as temperature
d13Cplot <- ggplot(d, aes(x=d13C, y=D47ICDES)) +
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey") +
  geom_errorbar(aes(xmin=d13C-SD13C, xmax=d13C+SD13C), colour="grey85") +
  geom_point(data = d, aes(x=d13C, y=D47ICDES, color=calcTiso),  size = 2) + #color = 'steelblue4',
  theme_bw()+
  labs(x=expression(delta^13~'C'), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= expression(Delta[47]~'vs'~delta^13~'C')) +
  theme(plot.title = element_text(hjust=0.5, face='bold'))

#basic 18O vs D47 plot with color as the d18O of sw
d18Oplot <- ggplot(d, aes(x=d18O, y=D47ICDES)) +
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey") +
  geom_errorbar(aes(xmin=d18O-SD18O, xmax=d18O+SD18O), colour="grey85") +
  geom_point(data = d, aes(x=d18O, y=D47ICDES, color=d18Osw), size = 2) +# color = 'steelblue4',
  theme_bw()+
  labs(x=expression(delta^18~'O'), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= expression(Delta[47]~'vs'~delta^18~'O')) +
  theme(plot.title = element_text(hjust=0.5, face='bold'))

#call plots
d18Oplot + d13Cplot + plot_annotation(tag_levels = 'A')

#plot of d18Osw vs residual D47 and color is temp
d18Osw <- ggplot(d, aes(x=d18Osw, y=residualsD47)) +
  #geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey") +
  geom_errorbar(aes(xmin=d18Osw-SE18Osw, xmax=d18Osw+SE18Osw), colour="grey85") +
  geom_point(data = d, aes(x=d18Osw, y=residualsD47, color=calcTiso), size = 2) +# color = 'steelblue4',
  theme_bw()+
  labs(x=expression(delta^18~'Osw'), y=expression(Delta[47] ~I-CDES[90] ~"(‰) residuals"), title= expression(Delta[47]~'vs'~delta^18~'O')) +
  theme(plot.title = element_text(hjust=0.5, face='bold'))
#call plot
d18Osw

#d18Osw vs temp
d18Oswtemp <- ggplot(d, aes(x=d18Osw, y=calcTiso)) +
  #geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey") +
  geom_errorbar(aes(xmin=d18Osw-SE18Osw, xmax=d18Osw+SE18Osw), colour="grey85") +
  geom_point(data = d, aes(x=d18Osw, y=calcTiso), size = 2) +# color = 'steelblue4',
  theme_bw()+
  labs(x=expression(delta^18~'Osw'), y="Temperature", title= expression('Temperature vs'~delta^18~'O')) +
  theme(plot.title = element_text(hjust=0.5, face='bold'))
d18Oswtemp

#Plots of subgroup on grey

#By Ocean
PO <- ggplot(d, aes(x=calcTiso, y=D47ICDES)) +
  geom_smooth(data = subset(d, `Sample Name`!="CH75-18-16 G MENARDII"), method = lm, fullrange=TRUE, color = 'black', fill='grey74')+
  geom_smooth(data = subset(d, Ocean=="Pacific Ocean"), aes(x=calcTiso, y=D47ICDES), method = lm, fullrange=TRUE, color = 'hotpink4', fill='hotpink')+
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey85") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  geom_point(color='grey', size = 2) +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  geom_point(data = subset(d, Ocean=="Pacific Ocean"), aes(x=calcTiso, y=D47ICDES, color = Dataset), size = 2) + #, color='azure4'
  theme_bw()+
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= 'Pacific Ocean') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))
  
ATO <- ggplot(d, aes(x=calcTiso, y=D47ICDES)) +
  geom_smooth(data = subset(d, `Sample Name`!="CH75-18-16 G MENARDII"), method = lm, fullrange=TRUE, color = 'black', fill='grey74')+
  geom_smooth(data = subset(d, Ocean=="Atlantic Ocean"), aes(x=calcTiso, y=D47ICDES), method = lm, fullrange=TRUE, color = 'hotpink4', fill='hotpink')+
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey85") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  geom_point(color='grey', size = 2) +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  geom_point(data = subset(d, Ocean=="Atlantic Ocean"), aes(x=calcTiso, y=D47ICDES, color = Dataset), size = 2) + #, color='coral2'
  theme_bw()+
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= 'Atlantic Ocean') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))

ARO <- ggplot(d, aes(x=calcTiso, y=D47ICDES)) +
  geom_smooth(data = subset(d, `Sample Name`!="CH75-18-16 G MENARDII"), method = lm, fullrange=TRUE, color = 'black', fill='grey74')+
  geom_smooth(data = subset(d, Ocean=="Arctic Ocean"), aes(x=calcTiso, y=D47ICDES), method = lm, fullrange=TRUE, color = 'hotpink4', fill='hotpink')+
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey85") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  geom_point(color='grey', size = 2) +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  geom_point(data = subset(d, Ocean=="Arctic Ocean"), aes(x=calcTiso, y=D47ICDES), size = 2, color='lightskyblue') +
  theme_bw()+
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= 'Arctic Ocean') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))

IO <- ggplot(d, aes(x=calcTiso, y=D47ICDES)) +
  geom_smooth(data = subset(d, `Sample Name`!="CH75-18-16 G MENARDII"), method = lm, fullrange=TRUE, color = 'black', fill='grey74')+
  geom_smooth(data = subset(d, Ocean=="Indian Ocean"), aes(x=calcTiso, y=D47ICDES), method = lm, fullrange=TRUE, color = 'hotpink4', fill='hotpink')+
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey85") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  geom_point(color='grey', size = 2) +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  geom_point(data = subset(d, Ocean=="Indian Ocean"), aes(x=calcTiso, y=D47ICDES), size = 2, color='darkgreen') +
  theme_bw()+
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= 'Indian Ocean') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))

GM <- ggplot(d, aes(x=calcTiso, y=D47ICDES)) +
  geom_smooth(data = subset(d, `Sample Name`!="CH75-18-16 G MENARDII"), method = lm, fullrange=TRUE, color = 'black', fill='grey74')+
  geom_smooth(data = subset(d, Ocean=="Gulf of Mexico"), aes(x=calcTiso, y=D47ICDES), method = lm, fullrange=TRUE, color = 'hotpink4', fill='hotpink')+
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey85") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  geom_point(color='grey', size = 2) +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  geom_point(data = subset(d, Ocean=="Gulf of Mexico"), aes(x=calcTiso, y=D47ICDES), size = 2, color='lightgoldenrod2') +
  theme_bw()+
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= 'Gulf of Mexico') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))

MS <- ggplot(d, aes(x=calcTiso, y=D47ICDES)) +
  geom_smooth(data = subset(d, `Sample Name`!="CH75-18-16 G MENARDII"), method = lm, fullrange=TRUE, color = 'black', fill='grey74')+
  geom_smooth(data = subset(d, Ocean=="Mediterranean Sea"), aes(x=calcTiso, y=D47ICDES), method = lm, fullrange=TRUE, color = 'hotpink4', fill='hotpink')+
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey85") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  geom_point(color='grey', size = 2) +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  geom_point(data = subset(d, Ocean=="Mediterranean Sea"), aes(x=calcTiso, y=D47ICDES), size = 2, color='darkslateblue') +
  theme_bw()+
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= 'Mediterranean Sea') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))

PO + ATO
PO + ATO + ARO + IO + GM + MS + plot_annotation(tag_levels = 'A')

#By Habitat

mixedP <- ggplot(d, aes(x=calcTiso, y=D47ICDES)) +
  geom_smooth(data = subset(d, `Sample Name`!="CH75-18-16 G MENARDII"), method = lm, fullrange=TRUE, color = 'black', fill='slategray2')+
  geom_smooth(data = subset(d, Habitat=="Mixed-layer"), aes(x=calcTiso, y=D47ICDES), method = lm, fullrange=TRUE, color = 'hotpink4', fill='hotpink')+
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey85") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  geom_point(color='grey', size = 2) +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  geom_point(data = subset(d, Habitat=="Mixed-layer"), aes(x=calcTiso, y=D47ICDES), size = 2, color='violetred') +
  theme_bw()+
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= 'Mixed-Layer') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))

ThermoP <- ggplot(d, aes(x=calcTiso, y=D47ICDES)) +
  geom_smooth(data = subset(d, `Sample Name`!="CH75-18-16 G MENARDII"), method = lm, fullrange=TRUE, color = 'black', fill='slategray2')+
  geom_smooth(data = subset(d, Habitat=="Thermocline"), aes(x=calcTiso, y=D47ICDES), method = lm, fullrange=TRUE, color = 'hotpink4', fill='hotpink')+
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey85") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  geom_point(color='grey', size = 2) +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  geom_point(data = subset(d, Habitat=="Thermocline"), aes(x=calcTiso, y=D47ICDES), size = 2, color='azure4') +
  theme_bw()+
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= 'Thermocline') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))

BenthicP <- ggplot(d, aes(x=calcTiso, y=D47ICDES)) +
  geom_smooth(data = subset(d, `Sample Name`!="CH75-18-16 G MENARDII"), method = lm, fullrange=TRUE, color = 'black', fill='slategray2')+
  geom_smooth(data = subset(d, Habitat=="Benthic, epifaunal"|Habitat=="Benthic, infaunal"|Habitat=="Benthic"|Habitat=="Benthic, epifaunal to shallow infaunal"), aes(x=calcTiso, y=D47ICDES), method = lm, fullrange=TRUE, color = 'hotpink4', fill='hotpink')+
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey85") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  geom_point(color='grey', size = 2) +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  geom_point(data = subset(d, Habitat=="Benthic, epifaunal"|Habitat=="Benthic, infaunal"|Habitat=="Benthic"|Habitat=="Benthic, epifaunal to shallow infaunal"), aes(x=calcTiso, y=D47ICDES), size = 2, color='lightskyblue') +
  theme_bw()+
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= 'Benthic') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))

mixedP + ThermoP + BenthicP + plot_layout(nrow = 2) + plot_annotation(tag_levels = 'A')

#by dataset
BreitP <- ggplot(d, aes(x=calcTiso, y=D47ICDES)) +
  geom_smooth(method = lm, fullrange=TRUE, color = 'black', fill='slategray2')+
  geom_smooth(data = subset(d, Dataset=="Breitenbach 2018"), aes(x=calcTiso, y=D47ICDES), method = lm, fullrange=TRUE, color = 'hotpink4', fill='hotpink')+
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey85") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  geom_point(color='grey', size = 2) +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  geom_point(data = subset(d, Dataset=="Breitenbach 2018"), aes(x=calcTiso, y=D47ICDES), size = 2, color='lightskyblue') + #
  theme_bw()+
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= 'Breitenbach 2018') +
  theme(plot.title = element_text(hjust=0.5, face='bold'))

MeinP <- ggplot(d, aes(x=calcTiso, y=D47ICDES)) +
  geom_smooth(method = lm, fullrange=TRUE, color = 'black', fill='slategray2')+
  geom_smooth(data = subset(d, Dataset=="Meinicke 2019"), aes(x=calcTiso, y=D47ICDES), method = lm, fullrange=TRUE, color = 'hotpink4', fill='hotpink')+
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey85") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  geom_point(color='grey', size = 2) +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  geom_point(data = subset(d, Dataset=="Meinicke 2019"), aes(x=calcTiso, y=D47ICDES), size = 2, color='coral2') + #
  theme_bw()+
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= 'Meinicke') +
  theme(plot.title = element_text(hjust=0.5, face='bold'))

PiaP <- ggplot(d, aes(x=calcTiso, y=D47ICDES)) +
  geom_smooth(method = lm, fullrange=TRUE, color = 'black', fill='slategray2')+
  geom_smooth(data = subset(d, Dataset=="Piasecki 2020"), aes(x=calcTiso, y=D47ICDES), method = lm, fullrange=TRUE, color = 'hotpink4', fill='hotpink')+
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey85") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  geom_point(color='grey', size = 2) +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  geom_point(data = subset(d, Dataset=="Piasecki 2020"), aes(x=calcTiso, y=D47ICDES), size = 2, color = 'darkgreen') + 
  theme_bw()+
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= 'Piasecki') +
  theme(plot.title = element_text(hjust=0.5, face='bold'))


PeralP <- ggplot(d, aes(x=calcTiso, y=D47ICDES)) +
  geom_smooth(method = lm, fullrange=TRUE, color = 'black', fill='slategray2')+
  geom_smooth(data = subset(d, Dataset=="Peral 2018"), aes(x=calcTiso, y=D47ICDES), method = lm, fullrange=TRUE, color = 'hotpink4', fill='hotpink')+
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey85") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  geom_point(color='grey', size = 2) +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  geom_point(data = subset(d, Dataset=="Peral 2018"), aes(x=calcTiso, y=D47ICDES), size = 2, color='lightgoldenrod2') + #
  theme_bw()+
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= 'Peral 2018') +
  theme(plot.title = element_text(hjust=0.5, face='bold'))

TripatiP <- ggplot(d, aes(x=calcTiso, y=D47ICDES)) +
  geom_smooth(method = lm, fullrange=TRUE, color = 'black', fill='slategray2')+
  geom_smooth(data = subset(d, Dataset=="Tripati 2010"), aes(x=calcTiso, y=D47ICDES), method = lm, fullrange=TRUE, color = 'hotpink4', fill='hotpink')+
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey85") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  geom_point(color='grey', size = 2) +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  geom_point(data = subset(d, Dataset=="Tripati 2010"), aes(x=calcTiso, y=D47ICDES), size = 2, color='darkslateblue') + #
  theme_bw()+
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= 'Tripati 2010') +
  theme(plot.title = element_text(hjust=0.5, face='bold'))

UCLAP <- ggplot(d, aes(x=calcTiso, y=D47ICDES)) +
  geom_smooth(method = lm, fullrange=TRUE, color = 'black', fill='slategray2')+
  geom_smooth(data = subset(d, Dataset=="UCLA"), aes(x=calcTiso, y=D47ICDES), method = lm, fullrange=TRUE, color = 'hotpink4', fill='hotpink')+
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey85") +
  geom_errorbar(aes(xmin=calcTiso-Terror, xmax=calcTiso+Terror), colour="grey85") +
  geom_point(color='grey', size = 2) +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  geom_point(data = subset(d, Dataset=="UCLA"), aes(x=calcTiso, y=D47ICDES), size = 2, color='azure4') + #
  #scale_color_viridis(option = "D")+
  theme_bw()+
  labs(x=expression(10^6~ '/' ~T^2), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= 'UCLA') +
  theme(plot.title = element_text(hjust=0.5, face='bold'))

BreitP + MeinP + PiaP+ PeralP+ TripatiP+UCLAP+plot_layout(nrow = 2) + plot_annotation(tag_levels = 'A')

#48
#jamie's function defined
fun.1 <- function(x) 0.1123 + 0.01971 * x  + 0.364 * x^2

p48 <- ggplot(data=subset(d, Habitat!="Benthic, epifaunal"&Habitat!="Benthic, infaunal"&Habitat!="Benthic"&Habitat!="Benthic, epifaunal to shallow infaunal"), aes(x=D47ICDES, y=D48)) +
  geom_smooth(method = lm, fullrange=TRUE, color = 'red', fill='grey74')+
  geom_errorbar(aes(ymin=D48-SE48, ymax=D48+SE48), width = 0, colour="grey") +
  geom_errorbar(aes(xmin=D47ICDES-SE47, xmax=D47ICDES+SE47), colour="grey85") +
  scale_x_continuous(sec.axis = sec_axis(~ sqrt(10^6 / . ) - 273.15, name = "Temperature in C"))+
  geom_point(data=subset(d, Habitat!="Benthic, epifaunal"&Habitat!="Benthic, infaunal"&Habitat!="Benthic"&Habitat!="Benthic, epifaunal to shallow infaunal"), aes(x=D47ICDES, y=D48, color=replicates), size = 2) +
  scale_color_viridis(name = "Replicate Count", option = "D", limits = c(1, 16))+#"Carbonate Ion \nSaturation",limits = c(-35, 80)
  #scale_color_carto_d(palette = "Safe")+
  stat_function(fun = fun.1)+xlim(0.55,0.7)+
  theme_bw()+
  labs(x=expression(Delta[47] ~"(‰)"), y=expression(Delta[48] ~"(‰)"), title= expression(Delta[48]~ 'with Temperature - No Benthics')) +
  theme(plot.title = element_text(hjust=0.5, face='bold'))

p48

d13Cplot <- ggplot(d, aes(x=d13C, y=D47ICDES)) +
  geom_errorbar(aes(ymin=D47ICDES-SE47, ymax=D47ICDES+SE47), width = 0, colour="grey") +
  geom_errorbar(aes(xmin=d13C-SD13C, xmax=d13C+SD13C), colour="grey85") +
  geom_point(data = d, aes(x=d13C, y=D47ICDES, color=carbsat), size = 2) +
  scale_color_viridis(name = "Carbonate Ion \nSaturation", option = "D",limits = c(-35, 80))+
  geom_point(data = subset(d, carbsat>=80), aes(x=d13C, y=D47ICDES), color = 'yellow', size = 2) +
  theme_bw()+
  labs(x=expression(delta^13~'C'), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= expression(Delta[47]~'vs'~delta^13~'C')) +
  theme(plot.title = element_text(hjust=0.5, face='bold'))

d13Cplot <- ggplot(d, aes(x=d13C, y=calcTiso)) +
  geom_errorbar(aes(ymin=calcTiso-Terror, ymax=calcTiso+Terror), width = 0, colour="grey") +
  geom_errorbar(aes(xmin=d13C-SD13C, xmax=d13C+SD13C), colour="grey85") +
  geom_point(data = d, aes(x=d13C, y=calcTiso, color=Habitat), size = 2) +
  theme_bw()+
  labs(x=expression(delta^13~'C'), y=expression(Delta[47] ~I-CDES[90] ~"(‰)"), title= expression(Delta[47]~'vs'~delta^13~'C')) +
  theme(plot.title = element_text(hjust=0.5, face='bold'))

d13Cplot

#Test for association between paired samples, using one of Pearson's product moment correlation coefficient, Kendall's \(\tau\) or Spearman's \(\rho\)
cor.test(d$calcTiso, d$D47ICDES)

            