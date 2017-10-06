`### Whinchat analysis: Risely, Blackburn and Cresswell 2014 ###




##1) Data manipulation - departure
##2a) Modelling departure with biometrics
##2b) Modelling departure without biometrics
##2c) Final departure model (only sex)
##2d) Using PC1 and PC2 (no difference)
##3a) Predicting departure mass
##3b) Sensitivity analysis graph
##4a) Departure boxplot
##4b) Departure histogram

##5) data manipulation - mass
##6) Modelling mass, all data
##7)PCA analysis on mass data
##8a) modelling period A
##8b) modelling period B
##8c) optimum model period 2
##9) Make tables

##10) Fig.5

##11) ces data manipulation
##12) CES plot & mass prediction



########DEPARTURE ANALYSIS############


##read dep data##

setwd("C:\\Users\\arisely\\Dropbox\\Whin Alice\\Whinchat analysis\\final datasets & script")

###data to load:
read.csv(dep.cvs)
##mass.csv
##ces.csv

library("MuMin")
library("ggplot2")
library("gridExtra")
library("reshape")



#################calculate Julian date
########Overwinter migrant in Africa from 1st Sept
dep$day1[dep$month == 4] <- 30 + 31 + 30 + 31 + 31 +28 +31
dep$day1[dep$month == 3] <- 30 + 31 + 30 + 31 + 31 +28
dep$day1[dep$month == 2] <- 30 + 31 + 30 + 31 + 31
dep$day1[dep$month == 1] <- 30 + 31 + 30 + 31 
dep$day1[dep$month == 12] <- 30 + 31 + 30
dep$day1[dep$month == 11] <- 30 + 31  
dep$day1[dep$month == 10] <- 30 
dep$day1[dep$month == 9] <- 0
dep$jdate <- dep$day1 + dep$day

######Recode month to covariate
dep$month1[dep$month==1] <- 13
dep$month1[dep$month==2] <- 14
dep$month1[dep$month==3] <- 15
dep$month1[dep$month==4] <- 16
dep$month1[dep$month==9] <- 9
dep$month1[dep$month==10] <- 10
dep$month1[dep$month==11] <- 11
dep$month1[dep$month==12] <- 12

###set factors
str(dep)
dep$site <- factor(dep$site)
dep$sex <- factor(dep$sex)
dep$age <- factor(dep$age)
dep$geo <- factor(dep$geo)
dep$pc1<-as.numeric(dep$pc1)
dep$pc2<-as.numeric(dep$pc2)

options(na.action = "na.omit")

######restrict to birds caught 2013/14 winter
dep.winter<-subset(dep, season==1)

######number of days after Sept1 departed
dep.winter$dept1 <- dep.winter$dept+30 + 31 + 30 + 31 + 31 +28+31

###########days between capture and departure
dep.winter$depdiff <- dep.winter$dept1 - dep.winter$jdate 

########restrict to birds caught in 2014 only
dep2014<-subset(dep.winter, year==2014)

#################################################################
#############################################################





###2a) Modelling departure with biometrics
##########################using biometrics################################


#######model departure using only 2014 birds#######


model2<-lm(dept~age+sex+site+wing+tars+geo+mass+time, data=dep2014)

options(na.action = "na.fail")

dd.model2<-dredge(model2, subset=dc(mass, time), extra = list("R^2", "*" = function(x) {
  s<- summary(x)
  c(Rsq = s$r.squared, adjRsq = s$adj.r.squared, F = s$fstatistic[[1]])}))

head(dd.model2,10)
dd.model2.mav<-model.avg(dd.model2, delta<4)

summary(dd.model2.mav)



##2b) Modelling departure without biometrics
##### excludin biometrics n=75 ####### FINAL

finalmodel4<-lm(dept~age+sex+site+geo, data = dep)
summary(finalmodel4)
dd.finalmodel4<-dredge(finalmodel4, extra = list("R^2", "*" = function(x) {
  s<- summary(x)
  c(Rsq = s$r.squared, adjRsq = s$adj.r.squared, F = s$fstatistic[[1]])}))


head(dd.finalmodel4,10)
finalmodel4.mav<-model.avg(dd.finalmodel4, delta<4)


summary(finalmodel4.mav)

##2c) Final departure model (only sex)

##########final model with just sex as significant factor #########

model.final<-lm(dept~sex, data=dep)

summary(model.final)

################using PC 2014 - NO DIFFERENCE ###################
##2d) Using PC1 and PC2 (no difference)


model2<-lm(dept~age+sex+site+pc1+pc2+geo+mass+time, data=dep2014)
summary(model2)

options(na.action = "na.fail")

dd.model2<-dredge(model2, subset=dc(mass, time), extra = list("R^2", "*" = function(x) {
  s<- summary(x)
  c(Rsq = s$r.squared, adjRsq = s$adj.r.squared, F = s$fstatistic[[1]])}))

head(dd.model2,10)
dd.model2.mav<-model.avg(dd.model2, delta<4)

summary(dd.model2.mav)

######################################



#################################################



##3a) Predicting departure mass


############relationship between mass and departure##############

massmodelfinal<-lm(depdiff~mass, data = dep2014)

summary(massmodelfinal)

####################################################

######How does mass relate to departure date
##i.e. birds that were weighed closer to departure should hav higher masses

####Restricted to birds caught Feb-Apr 2014.
dep$dept1 <- dep$dept+30 + 31 + 30 + 31 + 31 +28+31 #########julian date of departure
dep$depdiff <- dep$dept1 - dep$jdate #########difference in days between capture and departure

dep6 <- subset(dep, year ==2014)

model6 <- lm(depdiff ~mass +  tars + age + sex, data=dep6)
summary(model6)
model7 <- lm(depdiff ~mass, data=dep6)
summary(model7)
AIC(model6, model7)

plot(mass~depdiff, data = dep6)

######### check if relationship still exists for depdiff<50

depdiff50<-subset(dep6, depdiff<50)
model.depdiff<-lm(depdiff~mass+tars+age+sex, data=depdiff50)
summary(model.depdiff)

###not significant when restricted to birds caught within 50 days of departure

#########################Reanalysis to look at effect of time of start of study
##on predicted mass at departure

###reverse axes so we can easily see intercept = departure mass
#######All data
model71 <- lm(mass ~depdiff, data=dep6)
summary(model71)
plot(mass ~ depdiff, data=dep6, 
     ylab="Mass (g)", xlab = "Days until departure")
abline(lm(mass ~depdiff, data=dep6))
dim(dep6)

##### Days to departure < 70
dep70 <- subset(dep6, depdiff < 70)
dim(dep70)
model70 <- lm(mass ~depdiff, data=dep70)
summary(model70)
plot(mass ~ depdiff, data=dep70, 
     ylab="Mass (g)", xlab = "Days until departure")
abline(lm(mass ~depdiff, data=dep70))

##### Days to departure < 60
dep60 <- subset(dep6, depdiff < 60)
dim(dep60)
model60 <- lm(mass ~depdiff, data=dep60)
summary(model60)
plot(mass ~ depdiff, data=dep60, 
     ylab="Mass (g)", xlab = "Days until departure")
abline(lm(mass ~depdiff, data=dep60))

##### <50
dep50 <- subset(dep6, depdiff < 50)
dim(dep50)
model50 <- lm(mass ~depdiff, data=dep50)
summary(model50)

#### <40
dep40 <- subset(dep6, depdiff < 40)
dim(dep40)
model40 <- lm(mass ~depdiff, data=dep40)
summary(model40)

###<30
dep30 <- subset(dep6, depdiff < 30)
dim(dep30)
model30 <- lm(mass ~depdiff, data=dep30)
summary(model30)

##3b) Sensitivity analysis graph


############################################################NEW LAST FIGURE
####View sensitivity analysis on one graph
### i.e. same story more or less
par(bty="l")
plot(mass ~ depdiff, data=dep6, xlim=c(0,80),
     ylab="Mass (g)", xlab = "Days until departure")
abline(lm(mass ~depdiff, data=dep6), lty=1, lwd=2)
abline(lm(mass ~depdiff, data=dep60), lty=2)
abline(lm(mass ~depdiff, data=dep70), lty=3)
abline(v=0, lty=3)

x <- seq(0, max(dep6$depdiff), length=1000)
summary(model71)
###Average estimates plus 2 standard errors
dd <- -0.038534
ddse <- 0.009458 
int <- 17.450970
intse <- 0.487431 
upper <- dd + 2*(ddse)
lower <- dd - 2*(ddse)
predu <- (int+ (2*intse))+(upper*x)
predl <- (int- (2*intse)) + (lower*x)
points(x,predu,type="l",col="red", lty=3)
points(x,predl,type="l",col="red", lty=3)



######So intercept is affected a bit by change of period here but is around 17 gm 
## - fatter for sure but not the 24gm we might hope for

#####If you analyse it the way round I did originally then the regression is much more
## sensitive to restriction of the period of study
## because we extrapolate for a longer distance to the intercept
## so butterfly effect magnifying small difference in gradients



###########BOX plots############

##4a) Departure boxplot
######################can't work out how to get the second y-axis to disappear without messing up the whole graph... (see grid.arrange)




levels(dep$sex)[levels(dep$sex)=="1"] <- "Male"
levels(dep$sex)[levels(dep$sex)=="2"] <- "Female"

fig1a<-qplot(sex,dept, data=dep, geom = "boxplot", ylab="Relative departure (days)", xlab="")+ theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                                                                                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

levels(dep$age)[levels(dep$age)=="1"] <- "Adult"
levels(dep$age)[levels(dep$age)=="2"] <- "First winter"

age.female<-subset(dep, sex=="Female")
age.male<-subset(dep, sex=="Male")

fig1bb<- qplot(age,dept, data=age.male, geom = "boxplot", 
               ylab="Relative departure (days)", xlab="Male",
               ylim=c(0,25))+ theme_bw() + theme(panel.border = element_blank(), 
                                                 panel.grid.major = element_blank(), 
                                                panel.grid.minor = element_blank(), 
                                                axis.line = element_line(colour = "black"),
                                                axis.text.x=element_text(size=12), 
                                                axis.text.y=element_text(size=12), 
                                                axis.title.x=element_text(size=12), 
                                                axis.title.y = element_text(size=12),
                                                plot.margin=unit(c(0,-0.3,0,0), "cm"))



fig1b<-qplot(age,dept, data=age.female, geom = "boxplot", ylab="", 
             xlab="Female", ylim=c(0,25))+ theme_bw()+theme(panel.border = element_blank(), 
                                                            panel.grid.major = element_blank(), 
                                                            panel.grid.minor = element_blank(), 
                                                            axis.line = element_line(colour = "black"),
                                                            axis.ticks.y=element_blank(),
                                                            axis.text.y=element_blank(),
                                                            axis.text.x=element_text(size=12), 
                                                            axis.title.x=element_text(size=12),
                                                            plot.margin=unit(c(0,0,0,0.5), "cm"))


grid.arrange(fig1bb, fig1b, ncol=2)



######################histogram#################
##4b) Departure histogram


ggplot(dep, aes(x=dept)) + geom_histogram(binwidth=2, colour="black", fill="grey")+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                                                                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+labs(x="Departure(days)", y = "Frequency")







##88888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888
######888888888888888888888888888888888888888888888888888888888888888888888888888888888888888




###################################MASS ANALYSIS######################################################################

mass <- read.csv("C:/Users/Alice/Dropbox/Whin Alice/Whinchat analysis/final datasets & script/mass.csv")

############Any indication mass changes prior to departure? 

options(na.action = "na.omit")

##5) data manipulation - mass


###One case per bird - all recaptures removed
mass1 <- subset(mass, recap<2)

#################calculate Julian date
########Overwinter migrant in Africa from 1st Sept
mass1$day1[mass1$month == 4] <- 30 + 31 + 30 + 31 + 31 +28 +31
mass1$day1[mass1$month == 3] <- 30 + 31 + 30 + 31 + 31 +28
mass1$day1[mass1$month == 2] <- 30 + 31 + 30 + 31 + 31
mass1$day1[mass1$month == 1] <- 30 + 31 + 30 + 31 
mass1$day1[mass1$month == 12] <- 30 + 31 + 30
mass1$day1[mass1$month == 11] <- 30 + 31  
mass1$day1[mass1$month == 10] <- 30 
mass1$day1[mass1$month == 9] <- 0
mass1$jdate <- mass1$day1 + mass1$day


######Recode month to covariate
mass1$month1[mass1$month==1] <- 13
mass1$month1[mass1$month==2] <- 14
mass1$month1[mass1$month==3] <- 15
mass1$month1[mass1$month==4] <- 16
mass1$month1[mass1$month==9] <- 9
mass1$month1[mass1$month==10] <- 10
mass1$month1[mass1$month==11] <- 11
mass1$month1[mass1$month==12] <- 12

mass1$time <- mass1$hr + (mass1$min/60)
mass1$jdate2 <- mass1$jdate^2

mass1$season <- factor(mass1$season)
mass1$sex <- factor(mass1$sex)
mass1$age <- factor(mass1$age)
mass1$month <- factor(mass1$month)
mass1$location<-factor(mass1$location)




###########subset only relvant columns ##############

mass2<-mass1[,c(2,5,6,8,9,10,11,12,18,19,20,21,22,24,38,42,43,44,45)]



############# delete missing values, n = 377 ###############

mass.na<-na.omit(mass2)

################modelling###########


options(na.action = "na.fail")



#####Splitting up data before and after new year shows 
#######difference in mass gain over time between 2 periods

mass.na$period[mass.na$month1<13] <- 1 
mass.na$period[mass.na$month1>12] <- 2 
table(mass.na$period)

###########modelling all together#######

##6) Modelling mass, all data

model4<-lm(mass1~time+mintar+wing+sex+age+location+season+period*jdate, data = mass.na)
summary(model4)

head(mass.na)

###########################################################################
###########################################################################
###########################################################################


##7)PCA analysis on mass data

#####PCA analysis
masspca <- mass.na [c("mintar", "wing")]
dim(mass.na)
dim(masspca)
# run PCA
PCAmass <- prcomp(masspca, scale=T)  #scale is true

# look at variance explained by each PC in textual form
summary(PCAmass)

######Put Principle components into main data file for analysis
# get PCs into data frame
massPC <- data.frame(PCAmass$x)
head(massPC)
write.csv(massPC,"mass1.csv")
massPC1 <- read.csv("mass1.csv")
head(massPC1)
write.csv(mass.na,"mass2.csv")
mass2 <- read.csv("mass2.csv")
head(mass2)
totalmass <- merge(mass2,massPC1, by="X")
str(totalmass)
dim(totalmass)

##You can now use PC1 instead of wing or tarsus in the model
###########################################################################
###########################################################################



#####modelling periods seperately#######


mass.a<-mass.na[which(mass.na$period==1),]
mass.b<-mass.na[which(mass.na$period==2),]

###1st period, not much going on, tarsus main predictor, but weak interactions with sex and location
######julian date and time not significant


##8a) modelling period A

model1<-lm(mass1~jdate+time+mintar+wing+sex+age+location+season, data=mass.a)
summary(model1)
dd.model1<-dredge(model1, extra=list("R^2", "*" = function(x) {
  s<- summary(x)
  c(Rsq = s$r.squared, adjRsq = s$adj.r.squared, F = s$fstatistic[[1]])}))

head(dd.model1,10)
model1.mav<-model.avg(dd.model1, delta<4)

summary(model1.mav)

##8b) modelling period B

####in 2nd period much more happening, mintar and sex strongest predictors, 
#####however date of capture and time of day now also significant

model2<-lm(mass1~jdate+mintar+sex+location+time+age+wing+season, data=mass.b)
summary(model2)
dd.model2<-dredge(model2, extra=list("R^2", "*" = function(x) {
  s<- summary(x)
  c(Rsq = s$r.squared, adjRsq = s$adj.r.squared, F = s$fstatistic[[1]])}))


head(dd.model2,10)
model2.mav<-model.avg(dd.model2, delta<4)

summary(model2.mav)

###########optimum model period 2 ###########
##8c) optimum model period 2

model.final<-lm(mass1~mintar+jdate+sex+location+time+age, data=mass.b)
summary(model.final)
confint(model.final)

##########make tables################
##9) Make tables

write.table(dd.model1, "clipboard", sep="\t")

confint1<-confint(model1.mav)
write.table(confint1, "clipboard", sep="\t")

anova<-anova(model.final)
write.table(anova, "clipboard", sep="\t")

##10) Fig.5

#####################################################Illustrate result Fig 5
par(mfrow=c(1,3))
####Using parameter estimate from model including all significant variables
## identified with model averaging
###First half of winter
model1<-lm(mass1~jdate+season+time+wing +mintar+sex+age+location, data=mass.a)
summary(model1)

model75 <- model1
###predicted values
p1 <- summary(model75)[[4]][[1,1]]
p1e <- summary(model75)[[4]][[1,2]]
p2<-summary(model75)[[4]][[2,1]]
p2e<-summary(model75)[[4]][[2,2]]
p3<-summary(model75)[[4]][[3,1]]
p3e<-summary(model75)[[4]][[3,2]]
p4<-summary(model75)[[4]][[4,1]]
p4e<-summary(model75)[[4]][[4,2]]
p5<-summary(model75)[[4]][[5,1]]
p5e<-summary(model75)[[4]][[5,2]]
p6<-summary(model75)[[4]][[6,1]]
p6e<-summary(model75)[[4]][[6,2]]
p7<-summary(model75)[[4]][[7,1]]
p7e<-summary(model75)[[4]][[7,2]]
p8<-summary(model75)[[4]][[8,1]]
p8e<-summary(model75)[[4]][[8,2]]


plot(mass1 ~ jdate, data=mass.a, ylab="Mass (g) (predicted lines +/- 1SE)", 
     main="Sep-Dec", xlab = "", ylim=c(13,28), pch=16)
x <- seq(26, 100, length=1000)

###season 1 adult female, 07:00, median tarsus, median wing
yf2 <- p1 +(x * p2) + (7 * p4) + (median(mass.a$wing, na.rm=TRUE) * p5)+ 
  (median(mass.a$mintar, na.rm=TRUE) * p6) +p7
yf2u <- p1 +(x * (p2+p2e)) + (7 * p4) + (median(dep41$wing, na.rm=TRUE) * p5)+ 
  (median(dep41$mintar, na.rm=TRUE) * p6) +p7
yf2l <- p1 +(x * (p2-p2e)) + (7 * p4) + (median(dep41$wing, na.rm=TRUE) * p5)+ 
  (median(dep41$mintar, na.rm=TRUE) * p6) +p7
points(x,yf2,type="l",col="red", lty=2)
points(x,yf2u,type="l",col="red", lty=3)
points(x,yf2l,type="l",col="red", lty=3)

###season 1 adult male, 07:00, median tarsus, median wing
yf2m <- p1 +(x * p2) + (7 * p4) + (median(mass.a$wing, na.rm=TRUE) * p5)+ 
  (median(mass.a$mintar, na.rm=TRUE) * p6)
yf2um <- p1 +(x * (p2+p2e)) + (7 * p4) + (median(mass.a$wing, na.rm=TRUE) * p5)+ 
  (median(mass.a$mintar, na.rm=TRUE) * p6)
yf2lm <- p1 +(x * (p2-p2e)) + (7 * p4) + (median(mass.a$wing, na.rm=TRUE) * p5)+ 
  (median(mass.a$mintar, na.rm=TRUE) * p6)

points(x,yf2m,type="l",col="blue", lty=2)
points(x,yf2um,type="l",col="blue", lty=3)
points(x,yf2lm,type="l",col="blue", lty=3)

abline(v=30, lty=3, col="grey")
abline(v=61, lty=3, col="grey")
abline(v=92, lty=3, col="grey")
mtext(side=1, adj=.15, line=2,"OCTOBER", cex=0.6)
mtext(side=1, adj=.75, line=2,"NOVEMBER", cex=0.6)
legend(45,27, legend=c("Male", "Female"), cex=1.4, col=c("blue", "red"), 
       lty=1, lwd=2, bty="n")

###second half of winter
model21 <- lm(mass1 ~ jdate +  season + time + wing + 
                maxtar + age + sex, data=dep42)
summary(model21)
model22 <- lm(mass1 ~ jdate +  season + time + wing + 
                maxtar + age + sex + sex*jdate +age*jdate, data=dep42)
summary(model22)
AIC(model21, model22)

####Illustrate second half of the winter
model2<-lm(mass1~jdate+season+time+wing+mintar+sex+age+location, data=mass.b)
summary(model2)
model75 <- model2
###predicted values
p1 <- summary(model75)[[4]][[1,1]]
p1e <- summary(model75)[[4]][[1,2]]
p2<-summary(model75)[[4]][[2,1]]
p2e<-summary(model75)[[4]][[2,2]]
p3<-summary(model75)[[4]][[3,1]]
p3e<-summary(model75)[[4]][[3,2]]
p4<-summary(model75)[[4]][[4,1]]
p4e<-summary(model75)[[4]][[4,2]]
p5<-summary(model75)[[4]][[5,1]]
p5e<-summary(model75)[[4]][[5,2]]
p6<-summary(model75)[[4]][[6,1]]
p6e<-summary(model75)[[4]][[6,2]]
p7<-summary(model75)[[4]][[7,1]]
p7e<-summary(model75)[[4]][[7,2]]
p8<-summary(model75)[[4]][[8,1]]
p8e<-summary(model75)[[4]][[8,2]]
p9<-summary(model75)[[4]][[9,1]]
p9e<-summary(model75)[[4]][[9,2]]
summary(dep42)
x <- seq(min(mass.b$jdate), max(mass.b$jdate), length=1000)

plot(mass1 ~ jdate, data=mass.b, ylab="", ylim=c(13,28),
     main="Jan-Mar", xlab = "Days since 1st Sept", pch=16)

###season 1 adult female, 07:00, median tarsus, median wing
yf2 <- p1 +(x * p2) + p3+(7 * p5) + (median(mass.b$wing, na.rm=TRUE) * p6)+ 
  (median(mass.b$mintar, na.rm=TRUE) * p7) + p8
yf2u <- p1 +(x * (p2+p2e)) +p3 + (7 * p5) + (median(mass.b$wing, na.rm=TRUE) * p6)+ 
  (median(mass.b$mintar, na.rm=TRUE) * p7) +p8
yf2l <- p1 +(x * (p2-p2e)) +p3+ (7 * p5) + (median(mass.b$wing, na.rm=TRUE) * p6)+ 
  (median(mass.b$mintar, na.rm=TRUE) * p7)+p8

points(x,yf2,type="l",col="red", lty=1)
points(x,yf2u,type="l",col="red", lty=3)
points(x,yf2l,type="l",col="red", lty=3)

###season 1 adult male, 07:00, median tarsus, median wing
yf2m <- p1 +(x * p2) + p3+(7 * p5) + (median(mass.b$wing, na.rm=TRUE) * p6)+ 
  (median(mass.b$mintar, na.rm=TRUE) * p7) 
yf2um <- p1 +(x * (p2+p2e)) +p3 + (7 * p5) + (median(mass.b$wing, na.rm=TRUE) * p6)+ 
  (median(mass.b$mintar, na.rm=TRUE) * p7) 
yf2lm <- p1 +(x * (p2-p2e)) +p3+ (7 * p5) + (median(mass.b$wing, na.rm=TRUE) * p6)+ 
  (median(mass.b$mintar, na.rm=TRUE) * p7)

points(x,yf2m,type="l",col="blue", lty=1)
points(x,yf2um,type="l",col="blue", lty=3)
points(x,yf2lm,type="l",col="blue", lty=3)

abline(v=155, lty=3, col="grey")
abline(v=186, lty=3, col="grey")
abline(v=214, lty=3, col="grey")
abline(v=245, lty=3, col="grey")
mtext(side=1, adj=.30, line=2,"FEBRUARY", cex=0.6)
mtext(side=1, adj=.83, line=2,"MARCH", cex=0.6)

##11) ces data manipulation

################################################CES data for APRIL
############CES mass
ces <- read.table("C:/temp/whinc ces.csv", header=T, sep="," )
head(ces)
dim(ces)

ces1 <- subset(ces, month==4)
ces2 <- subset(ces, month==5)
table(ces1$month)
ces2

table(ces1$mass)

ces1$time <- ces1$hr + (ces1$min/60)
ces1$year <- factor(ces1$year)
ces1$sex1[ces1$sex=="F"] <- 2
ces1$sex1[ces1$sex=="G"] <- 2
ces1$sex1[ces1$sex=="M"] <- 1
ces1$sex1[ces1$sex=="N"] <- 1
table(ces1$sex1)
ces1$sex1 <- factor(ces1$sex1)
table(ces1$location)
ces1$loc <- as.numeric(ces1$location)
table(ces1$loc)
ces2 <- subset(ces1, loc<5)
table(ces2$loc)
table(ces2$year)
dim(ces2)

ces2$jdate <- ces2$day+30 + 31 + 30 + 31 + 31 +28 +31
model1 <- lm(mass ~day + year + time + location + sex1 + wing , data=ces2)
summary(model1)

##12) CES plot & mass prediction

plot(mass ~ jdate, data=ces2, ylim=c(13,28), xlab="", ylab="", main="April", pch=16)
####Male average size at 07:00 in 2013 location 1
x <- seq(min(ces2$day), max(ces2$day), length=1000)
x1 <- seq(min(ces2$jdate), max(ces2$jdate), length=1000)
yc <-    -6.09591  +(x * (  0.22754)) +(7 *  0.20209) +
  (median(ces2$wing)*0.29929) + (9.29486) + (-11.25518)
ycl <-    -6.09591   +(x * (  0.22754-0.06384)) +(7 *  0.20209) +
  (median(ces2$wing)*0.29929) + (9.29486) + (-11.25518)
ycu <-   -6.09591  +(x * (  0.22754+0.06384)) +(7 *  0.20209) +
  (median(ces2$wing)*0.29929) + (9.29486) + (-11.25518)
points(x1,yc,type="l",col="blue", lty=1)
points(x1,ycl,type="l",col="blue", lty=3)
points(x1,ycu,type="l",col="blue", lty=3)
ycl
####Female average size at 07:00 location 1
ycf <-    -6.09591  +(x * (  0.22754)) +(7 *  0.20209) +
  (median(ces2$wing)*0.29929) + (9.29486) + (-11.25518) -1.35124 
yclf <-    -6.09591   +(x * (  0.22754-0.06384)) +(7 *  0.20209) +
  (median(ces2$wing)*0.29929) + (9.29486) + (-11.25518) -1.35124 
ycuf <-   -6.09591  +(x * (  0.22754+0.06384)) +(7 *  0.20209) +
  (median(ces2$wing)*0.29929) + (9.29486) + (-11.25518) -1.35124 
points(x1,ycf,type="l",col="red", lty=1)
points(x1,yclf,type="l",col="red", lty=3)
points(x1,ycuf,type="l",col="red", lty=3)
mtext(side=1, adj=.5, line=2,"APRIL", cex=0.6)

##############################Range calculations
ces1 <- ces2
dim(ces1)
ces1$mass1[ces1$mass<=14.0] <- 14.0
ces1$mass1[ces1$mass>14 & ces1$mass<=14.5] <- 14.5
ces1$mass1[ces1$mass>14.5 & ces1$mass<=15.0] <- 15.0
ces1$mass1[ces1$mass>15 & ces1$mass<=15.5] <- 15.5
ces1$mass1[ces1$mass>15.5 & ces1$mass<=16] <- 16
ces1$mass1[ces1$mass>16 & ces1$mass<=16.5] <- 16.5
ces1$mass1[ces1$mass>16.5 & ces1$mass<=17] <- 17
ces1$mass1[ces1$mass>17 & ces1$mass<=17.5] <- 17.5
ces1$mass1[ces1$mass>17.5 & ces1$mass<=18] <- 18
ces1$mass1[ces1$mass>18 & ces1$mass<=18.5] <- 18.5
ces1$mass1[ces1$mass>18.5 & ces1$mass<=19] <- 19
ces1$mass1[ces1$mass>19 & ces1$mass<=19.5] <- 19.5
ces1$mass1[ces1$mass>19.5 & ces1$mass<=20] <- 20
ces1$mass1[ces1$mass>20 & ces1$mass<=20.5] <- 20.5
ces1$mass1[ces1$mass>20.5 & ces1$mass<=21] <- 21
ces1$mass1[ces1$mass>21 & ces1$mass<=21.5] <- 21.5
ces1$mass1[ces1$mass>21.5 & ces1$mass<=22] <- 22
ces1$mass1[ces1$mass>22 & ces1$mass<=22.5] <- 22.5
ces1$mass1[ces1$mass>22.5 & ces1$mass<=23] <- 23
ces1$mass1[ces1$mass>23 & ces1$mass<=23.5] <- 23.5
ces1$mass1[ces1$mass>23.5 & ces1$mass<=24] <- 24
ces1$mass1[ces1$mass>24 & ces1$mass<=24.5] <- 24.5
ces1$mass1[ces1$mass>24.5 & ces1$mass<=25] <- 25
ces1$mass1[ces1$mass>25 & ces1$mass<=25.5] <- 25.5
ces1$mass1[ces1$mass>25.5 & ces1$mass<=26] <- 26
ces1$mass1[ces1$mass>26 & ces1$mass<=26.5] <- 26.5
ces1$mass1[ces1$mass>26.5 & ces1$mass<=27] <- 27
ces1$mass1[ces1$mass>27 & ces1$mass<=27.5] <- 27.5
ces1$mass1[ces1$mass>27.5 & ces1$mass<=28.5] <- 28
table(ces1$mass1)

aggces <- aggregate(ces1$mass, by=list(ces1$mass1), FUN=length)
aggces

aggces1 <- rename(aggces, c(Group.1="mass", x="n"))
aggces1
dim(ces1)
aggces1$perbirds <- (aggces1$n/103) * 100
aggces1

##############Data put into excel sheet and the plotted in origin

