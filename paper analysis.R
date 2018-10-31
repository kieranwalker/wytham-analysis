#######   re-do of scatter plots #######
rm(list=ls())
setwd("D:/Users/Kieran Walker/Documents/summerproject/code")
getwd()


# First is the analysis of the base trees, Ash, followed by Oak
# Then comes the analysis of the intensive trees with analysis of all trees after ash and oak

#################################################################################
# predicting respiration - sapwood vs growth for BASE trees

rm(list=ls()) # clear R environment 
setwd("D:/Users/Kieran Walker/Documents/summerproject/code") #set working directory
allbase<- read.csv("../data/all_base.csv", header = TRUE)
ashdata<- read.csv("../data/ashbase.csv", header = TRUE) 
oakdata<- read.csv("../data/oakbase.csv", header = TRUE)
require (nlme)

baseash<-lm(Co2.flux~recip.diam, data=ashdata) #model flux as function of the recipricol of diameter as per meir 2002
summary(baseash) # no effect p=0.858
plot(Co2.flux~recip.diam, data=ashdata, xlim=c(0,0.08), ylim=c(0, 2))
abline(lm(Co2.flux ~ recip.diam, data=ashdata)) 

#ash base - plots : growth, sapwood, diameter effects on respiration 
plot(Co2.flux~(diameter..cm.), data=ashdata, xlim=c(0,140), ylim=c(0, 2)) # no effect of diameter on resp
abline(lm(Co2.flux~(diameter..cm.), data=ashdata)) # slightly negative slope 

lm1<-lm(Co2.flux~diameter..cm., data=ashdata)
summary(lm1)

plot(Co2.flux~vol.diff, data=ashdata, xlim=c(0, 25), ylim=c(0, 2)) # volume increase positive effect on resp - maybe that small trees have larger vol increase? may explain the above neg trend  
abline(lm(Co2.flux ~ vol.diff, data=ashdata)) 

lm2<-lm(Co2.flux~vol.diff, data=ashdata)
summary(lm2)

plot(vol.diff~sapwood, data=ashdata)
abline(lm(vol.diff~diameter..cm., data=ashdata))

growth.size<-lm(vol.diff~diameter..cm., data=ashdata)
summary(growth.size)# diameter does not predict grwth rate 

RtVollDiff<-lm(Co2.flux~vol.diff, data=ashdata)
summary(RtVollDiff) #sig positve effect 

#models
ashmodel<-glm(Co2.flux ~ sapwood + vol.diff, data=ashdata)
summary(ashmodel) #model including both sapwood and growth - only growth predicts resp

## ash testing sapwood as fnction of diameter
fitash2<- glm(sapwood ~ diameter..cm., data=ashdata,family=gaussian())
summary(fitash2)
plot(fit)

diamash<-ggplot(ashbase, aes(x=diameter, y=sapwood)) + geom_point(size=2.5) + theme_bw()
print(diamash)

require(ggplot)
# OAK 

plot(Co2.flux~vol.diff, data=oakdata, xlim=c(0, 20), ylim=c(0, 0.8))
abline(lm(Co2.flux ~ vol.diff, data=oakdata)) # positive trend of vol diff on flux

RtVollDiffoak<-lm(Co2.flux~vol.diff, data=oakdata)
summary(RtVollDiffoak) #no sig effect p=0.16

oak<-glm(Co2.flux~sapwood + vol.diff, data=oakdata)
summary(oak) # neither sapwood or growth predict flux when both included 

oak<-glm(Co2.flux~diameter..cm., data=oakdata)
summary(oak)

temprespoak<-ggplot(oakbase, aes(x=sapwood, y=Co2.flux)) + geom_point(size=2.5) + theme_bw()
print(temprespoak)
temprespoak + geom_smooth(method=lm, se=TRUE) 

#plotting sapwood width as function of diameter 
diamsapoak<-ggplot(oakbase, aes(x=diameter, y=sapwood)) + geom_point(size=2.5) + theme_bw()
print(diamsapoak)
diamsapoak + geom_smooth(method=lm, se=TRUE)

# using sapwood and growth to predict flux when species are aggregated 

growthmodel<-glm(Co2.flux..gCO2.m2.hr.~wvol.difference, data=allbase)
summary(growthmodel) # when aggregated, growth sig predicter of flux 

sapwoodmodel<-glm(Co2.flux..gCO2.m2.hr.~sapwood, data=allbase)
summary(sapwoodmodel) #when aggregated sapwood is sig predicter of flux 

allmodel<-glm(Co2.flux..gCO2.m2.hr.~wvol.difference+sapwood, data=allbase)
summary(allmodel) #model accross both species - both sapwood and wood vol differnce predict flux



#predicing respiration - INTENIVE TREES
#####################################################################################
rm(list=ls())
setwd("D:/Users/Kieran Walker/Documents/summerproject/code")
allintensive<- read.csv("../data/all_intensive.csv", header = TRUE)
ashintensive<- read.csv("../data/ash_intensive.csv", header = TRUE)
oakintensive<-read.csv("../data/oak_intensive.csv", header = TRUE)
diamintensive<-read.csv("../data/diamintensive.csv", header = TRUE) # contains sycamore values 
allbase<-read.csv("../data/all_base.csv", header = TRUE)
oakbase<-read.csv("../data/oak_base.csv", header = TRUE)
ashbase<-read.csv("../data/ash_base.csv", header = TRUE)
modelvalidation<-read.csv("../data/model_validation.csv", header = TRUE)
syc<-read.csv("../data/syc.csv", header = TRUE) #line 93 starts at


#ASH
#predicting flux with height (branch order)

plot(Co2.flux ~ height, data=ashintensive)
abline(lm(Co2.flux ~ height, data=ashintensive)) #positive trend of height on flux 
m1<-(lm(Co2.flux ~ height, data=ashintensive)) 

ashheight <- lme (Co2.flux ~ height,
                  data=ashintensive, random = ~1 |Individual, na.action=na.exclude, method = "REML") #mixed model, works (produces output)
summary(ashheight) # sig effect of height- p =0.0218

#sapwood
#Basic scatter plot, colour assigned to each individual tree
# Basic scatter plot, colour assigned to each individual tree
require(ggplot2)

gash1<-ggplot(ashintensive, aes(x=Sapwood, y=Co2.flux, color= factor (individual))) + geom_point(size=2.5) + theme_bw()
gash1 + geom_smooth(method=lm, se=FALSE)

#modeling flux~sapwood
amodel.sap <- lme (CO2.flux ~ Sapwood,
                   data=ashintensive, random = ~1 |Individual, na.action=na.exclude, method = "REML") #mixed model, works (produces output)

summary(amodel.sap) #no effect of temperature or sapwood on flux

#predicting sapwood by diam
adsm <- lme(sw.corrected ~ diameter, data = ashintensive, random = ~ 1|individual, na.action=na.exclude)
summary(adsm) #diam has significant effect on sapwood

#diameter
plot(Co2.flux~diameter,data=ashintensive)
abline(lm(Co2.flux~diameter,data=ashintensive))

diamdepth<- lme(Co2.flux ~ diameter, data = ashintensive, random = ~ 1|Individual, na.action=na.exclude)
summary(diamdepth) #no significant effect


#predicting sapwood area fraction by diameter 

gash3 <-ggplot(ashintensive, aes(x=diameter, y=sapwood.area.fraction, color=factor(Individual))) + geom_point(size=2.5) + theme_bw()
gash3 + geom_smooth(method=lm, se=FALSE)
gash3 + scale_x_continuous(name="branch diameter (cm)")
gash3 + scale_x_continuous(name="sapwood area fraction")



#oak
#predicting flux with height
plot(CO2.flux ~ height, data=oakintensive)
abline(lm(CO2.flux ~ height, data=oakintensive))
       
omodel.height<- lme (CO2.flux ~ height,
                   data=oakintensive, random = ~1 |Individual, na.action=na.exclude, method = "REML") #mixed model, works (produces output)

summary(omodel.height) #no effect of height on flux 


#predicting flux with sapwood and stem temperature 
plot(CO2.flux~Sapwood, data=oakintensive)
abline(lm(CO2.flux ~ Sapwood, data=oakintensive))

omodel.sap <- lme (CO2.flux ~ Sapwood + stem.temperature,
                   data=oakintensive, random = ~1 |Individual, na.action=na.exclude, method = "REML") #mixed model, works (produces output)

summary(omodel.sap) #AIC -7.33, significance for sapwood but not temperature

goak<-ggplot(oakintensive, aes(x=Sapwood, y=CO2.flux, color= factor (Individual))) + geom_point(size=2.5) + theme_bw()
goak + geom_smooth(method=lm, se=FALSE)+
  ylab("CO2 efflux" ) +
  xlab("Sapwood depth cm ")

#predicting flux with diameter
odiamresp <- lme(CO2.flux ~ diameter+Sapwood, data = oakintensive, random = ~ 1|Individual, na.action=na.exclude)
summary(odiamresp) #no sig effect of diam


#predicting sapwood with diam
olm<-lm(Sapwood~diameter, oakintensive)
summary(olm) #no predictive effect


#predicting sapwood area fraction with diam
goak3<-ggplot(oakintensive, aes(x=diameter, y=sapwood.area.fraction, color=factor(individual))) + geom_point(size=2.5) + theme_bw()
goak3 + scale_x_continuous(name="branch diameter (cm)"), 
scale_y_continuous(name="sapwood area fraction")
goak3 + geom_smooth(method=lm, se=FALSE)


##looking at both species together

agg.model <- lme ((Co2.flux) ~ sapwood.corrected + species,
                  data=allintensive, random = ~1 |individual, na.action=na.exclude) #mixed model, works (produces output)

summary(agg.model) #significant positive effect of sapwood overall and sig negative effect of oak

plot(agg.model) #model diagnostics 
res_omodel2=residuals(agg.model)
par(mfrow=c(1,2)) #multiple figures per row specifically, 2 figures to 1 row
plot(agg.model)
qqnorm(agg.model)
qqline(agg.model)
hist(agg.model)
AIC(agg.model)


#plot for co2~sapwood for both ash and oak
MMplot<-ggplot(allintensive, aes(x=allintensive$sw.corrected, y=log(allintensive$Co2.flux), color= factor (species))) + geom_point(size=2.5) + theme_bw()
print(MMplot)
MMplot + geom_smooth(method=lm, se=F)

plot(Sapwood~diameter, oakintensive)
abline(lm(Sapwood~diameter, oakintensive))
m11<-lm(Sapwood~diameter, oakintensive)
summary(m11)

#plotting sapwood as function of diameter for both ash and oak
p <- ggplot(allintensive, aes(x=diameter, y=sw.corrected, colour=Species)) +
  geom_point(size=3) +
  geom_line(aes(y=predict(dsm), group=factor(individual), size="individual")) +
  geom_line(data=newdat, aes(y=predict(dsm, level=0, newdata=newdat), size="Population")) +
  scale_size_manual(name="Predictions", values=c("individual"=0.5, "Population"=2)) +
  theme_bw(base_size=22) 
print(p)
                             