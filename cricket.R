cricket = read.csv("Cricket.Data.Experimental.csv")

#####CRICKET INTERRELIABILITY TESTS#########

reliability=read.csv("Cricket.Data.Reliability.csv")
str(reliability)
A = subset(reliability, People=="A", na.rm = T)
B = subset(reliability, People=="B", na.rm = T)
C = subset(reliability, People=="C", na.rm = T)
#the treatment is the same, so can directly check skewness
shapiro.test (reliability$Duration1stEnc)
shapiro.test (reliability$LatTo1stEnc)
shapiro.test (reliability$Level1stEnc)
shapiro.test (reliability$NoEncs)#not skewed
shapiro.test (reliability$HighestLevel)

hist(reliability$Duration1stEnc, breaks=10)
hist(reliability$LatTo1stEnc, breaks=10)
hist(reliability$Level1stEnc, breaks=10)
hist(reliability$NoEncs, breaks=10)
hist(reliability$HighestLevel, breaks=10)

#ANOVA test with no assumption of equal variances
oneway.test(LatTo1stEnc ~ People, data = reliability)
oneway.test(Marked ~ People, data = reliability)
oneway.test(Unmarked ~ People, data = reliability)
oneway.test(Duration1stEnc ~ People, data = reliability)
oneway.test(Level1stEnc ~ People, data = reliability)
oneway.test(NoEncs ~ People, data = reliability)
oneway.test(HighestLevel ~ People, data = reliability)
oneway.test(Winner ~ People, data = reliability)

#regular ANOVA
summary(aov(LatTo1stEnc ~ People, data = reliability))
summary(aov(Marked ~ People, data = reliability))
summary(aov(Unmarked ~ People, data = reliability))
summary(aov(Duration1stEnc ~ People, data = reliability))
summary(aov(Level1stEnc ~ People, data = reliability))
summary(aov(NoEncs ~ People, data = reliability))
summary(aov(HighestLevel ~ People, data = reliability))
summary(aov(Winner ~ People, data = reliability))

#regular ANOVA, assumes similar variance 
kruskal.test(LatTo1stEnc ~ People, data = reliability)
kruskal.test(Marked ~ People, data = reliability)
kruskal.test(Unmarked ~ People, data = reliability)
kruskal.test(Duration1stEnc ~ People, data = reliability)
#could use index of concordance from here #calculated directly with excel 
kruskal.test(Level1stEnc ~ People, data = reliability)
kruskal.test(NoEncs ~ People, data = reliability)
kruskal.test(HighestLevel ~ People, data = reliability)
kruskal.test(Winner ~ People, data = reliability)

#plot boxplots
plot(LatTo1stEnc ~ People, data = reliability)
plot(Marked ~ People, data = reliability)
plot(Unmarked ~ People, data = reliability)
plot(Duration1stEnc ~ People, data = reliability)
plot(Level1stEnc ~ People, data = reliability)
plot(NoEncs ~ People, data = reliability)
plot(HighestLevel ~ People, data = reliability)
plot(Winner ~ People, data = reliability)

###### VARIABLES & SUBSETS #########
str(cricket)
summary(cricket)
attach(cricket)
female = subset(cricket, Treatment=="WithFemale", na.rm = TRUE)
female.control = subset(cricket, Treatment=="WithoutFemale")
pheromone = subset(cricket, Treatment=="WithPheromone")
pheromone.control = subset(cricket, Treatment=="WithouPheromone")
paper = subset(cricket, Treatment == "WithPheromone, WithouPheromone")
nopaper = subset(cricket, Treatment == "WithFemale, WithoutFemale")
female.all = subset(cricket, Treatment == "WithFemale, WithPheromone")
control.all = subset(cricket, Treatment == "WithFemale, WithoutFemale")
#if the winner is consistent
winner.fct = function(first,overall){if (isTRUE(first == overall)) {return("1")} else {return("0")}}
cricket$concistency = cricket$consistency
cricket$consistency = mapply(FUN = winner.fct, cricket$Winner1stEnc, cricket$Winner)
cricket$consistency = as.numeric(cricket$consistency)
cricket$X = NULL
percent.consistent = mean(cricket$consistency)
percent.heavywin = mean(as.numeric(cricket$Winner), na.rm = TRUE)


######### NORMALITY #######
#	Normality	tests- does	variable	1	have	a	normal	distribution

library (pastecs)
by (cricket$LatTo1stEnc, Treatment,stat.desc,basic=FALSE, norm=TRUE)
#if p-value of shaipiro test, reject null hypothesis (p<0.05 --> not normally distributed)
#shows that everything is skewed 
#show this graphically with density plots
by (cricket$Duration1stEnc, Treatment,stat.desc,basic=FALSE, norm=TRUE)
by (cricket$Level1stEnc, Treatment,stat.desc,basic=FALSE, norm=TRUE)
by (cricket$NoEncs, Treatment,stat.desc,basic=FALSE, norm=TRUE)
by (cricket$HighestLevel, Treatment,stat.desc,basic=FALSE, norm=TRUE)

#check normalization of logged data 
#log doesn't help
by (log(cricket$LatTo1stEnc), Treatment,stat.desc,basic=FALSE, norm=TRUE)
by (log(cricket$Duration1stEnc), Treatment,stat.desc,basic=FALSE, norm=TRUE)
by (log(cricket$Level1stEnc), Treatment,stat.desc,basic=FALSE, norm=TRUE)
by (log(cricket$NoEncs), Treatment,stat.desc,basic=FALSE, norm=TRUE)
by (log(cricket$HighestLevel), Treatment,stat.desc,basic=FALSE, norm=TRUE)


#box plots 
boxplot(female)
par(mfrow=c(3,2),pty='m')
plot(cricket$LatTo1stEnc ~ cricket$Treatment, na.rm = TRUE)
plot(cricket$Duration1stEnc ~cricket$Treatment, na.rm = TRUE)
plot(cricket$Level1stEnc ~cricket$Treatment, na.rm = TRUE)
plot(cricket$NoEncs ~cricket$Treatment, na.rm = TRUE)
plot(cricket$HighestLevel ~cricket$Treatment, na.rm = TRUE)

#histograms to show distribution 
hist(cricket$LatTo1stEnc, na.rm = TRUE)
hist(cricket$Duration1stEnc, na.rm = TRUE)
hist(cricket$Level1stEnc, na.rm = TRUE)
hist(cricket$NoEncs, na.rm = TRUE)
hist(cricket$HighestLevel, na.rm = TRUE)

#variables = c(levels(cricket$LatTo1stEnc|cricket$Duration1stEnc|cricket$Level1stEnc|cricket$NoEncs|cricket$HighestLevel))

#variables.com = c(levels(cricket$LatTo1stEnc,cricket$Duration1stEnc,cricket$Level1stEnc,cricket$NoEncs,cricket$HighestLevel))


library(ggplot2)
library(grid)
p1 = ggplot(cricket, aes(x=Treatment))+
  geom_boxplot(aes(y=LatTo1stEnc)) + geom_point(aes(y=LatTo1stEnc), color="blue", alpha = 0.3)+
  theme(text = element_text(size=20))
p2 = ggplot(cricket, aes(x=Treatment))+
  geom_boxplot(aes(y=Duration1stEnc)) + geom_point(aes(y=Duration1stEnc), color="blue", alpha = 0.3)+
  theme(text = element_text(size=20))
p3 = ggplot(cricket, aes(x=Treatment))+
  geom_boxplot(aes(y=Level1stEnc)) + geom_point(aes(y=Level1stEnc), color="blue", alpha = 0.3)+
  theme(text = element_text(size=20))
p4 = ggplot(cricket, aes(x=Treatment))+
  geom_boxplot(aes(y=NoEncs)) + geom_point(aes(y=NoEncs), color="blue", alpha = 0.3)+
  theme(text = element_text(size=20))
p5 = ggplot(cricket, aes(x=Treatment))+
  geom_boxplot(aes(y=HighestLevel)) + geom_point(aes(y=HighestLevel), color="blue", alpha = 0.3)+
  theme(text = element_text(size=20))

p1
p2
p3
p4
p5
# doesnt work
p = ggplot(cricket, aes(x=Treatment))+facet_grid(Treatment ~.)+
  geom_boxplot(aes(y=LatTo1stEnc)) + geom_point(aes(y=LatTo1stEnc), color="blue", alpha = 0.3)+
  geom_boxplot(aes(y=Duration1stEnc)) + geom_point(aes(y=Duration1stEnc), color="blue", alpha = 0.3)+
  geom_boxplot(aes(y=Level1stEnc)) + geom_point(aes(y=Level1stEnc), color="blue", alpha = 0.3)+
  geom_boxplot(aes(y=NoEncs)) + geom_point(aes(y=NoEncs), color="blue", alpha = 0.3)+
  geom_boxplot(aes(y=HighestLevel)) + geom_point(aes(y=HighestLevel), color="blue", alpha = 0.3)+

#+facet_grid(variables.com ~.)+
#Another try 
ggplot(cricket, aes(x=Treatment))+ 
  geom_boxplot(aes(y=LatTo1stEnc, color="LatTo1stEnc")) + geom_point(aes(y=LatTo1stEnc, color="LatTo1stEnc"))+
  geom_boxplot(aes(y=Duration1stEnc, color="Duration1stEnc")) + geom_point(aes(y=Duration1stEnc, color="Duration1stEnc"))+
  geom_boxplot(aes(y=Level1stEnc, color="Level1stEnc")) + geom_point(aes(y=Level1stEnc, color="Level1stEnc"))+
  geom_boxplot(aes(y=NoEncs, color="NoEncs")) + geom_point(aes(y=NoEncs, color="NoEncs"))+
  geom_boxplot(aes(y=HighestLevel, color="HighestLevel")) + geom_point(aes(y=HighestLevel, color="HighestLevel"))


#plot all density plots in one graph
#library(sm)
# plot densities 
#sm.density.compare(female$LatTo1stEnc, model="equal", xlab="Latency to first encounter of Female group")
#title(main="latency female")

# add legend via mouse click
#colfill<-c(2:(2+length(levels(cyl.f)))) 
#legend(locator(1), levels(cyl.f), fill=colfill)
#cdplot(cricket$Treatment~cricket$LatTo1stEnc)

####### GLM MODELS #####
#observed 4 barplot distributions with quasipoisson distribution 
modellatency=glm(LatTo1stEnc~Treatment, family="poisson", data=cricket)
summary(modellatency)
modellatency.quasi=glm(LatTo1stEnc~Treatment, family="quasipoisson", data=cricket)
model.duration=glm(Duration1stEnc~Treatment, family="quasipoisson", data=cricket)
model.1level=glm(Level1stEnc~Treatment, family="quasipoisson", data=cricket)
model.enc=glm(NoEncs~Treatment, family="quasipoisson", data=cricket)
model.highestLevel=glm(HighestLevel~Treatment, family="quasipoisson", data=cricket)
model.overallWin=glm(Winner~Treatment, family="quasipoisson", data=cricket)
summary(modellatency.quasi)
summary(model.duration)
summary(model.1level)
summary(model.enc)
summary(model.highestLevel) ## significant! 
summary(model.overallWin) ## small significance 

# taking in account of weight difference 
#shows that size different is controlled ie size diff does not have an effect wrt to the different treatments 
modellatency.quasi.size=glm(LatTo1stEnc~Treatment*SizeDiff, family="quasipoisson", data=cricket)
model.duration.size=glm(Duration1stEnc~Treatment*SizeDiff, family="quasipoisson", data=cricket)
model.1level.size=glm(Level1stEnc~Treatment*SizeDiff, family="quasipoisson", data=cricket)
model.enc.size=glm(NoEncs~Treatment*SizeDiff, family="quasipoisson", data=cricket)
model.highestLevel.size=glm(HighestLevel~Treatment*SizeDiff, family="quasipoisson", data=cricket)
model.overallWin.size=glm(Winner~Treatment*SizeDiff, family="quasipoisson", data=cricket)
summary(modellatency.quasi.size)
summary(model.duration.size)
summary(model.1level.size)
summary(model.enc.size)
summary(model.highestLevel.size)
summary(model.overallWin.size)

######## NEGATIVE BINOMIAL ##########
#negative binomial distribution = distribution of # of independent trials needed to get the rth success
# --> success = fixed # 
#binomial distribution = # of sucesses in fixed number of trials
library(MASS)
modellatency.quasi.size.nb=glm.nb(LatTo1stEnc~Treatment*SizeDiff, data=cricket)
model.duration.size.nb=glm.nb(Duration1stEnc~Treatment*SizeDiff, data=cricket)
model.1level.size.nb=glm.nb(Level1stEnc~Treatment*SizeDiff, data=cricket)
model.enc.size.nb=glm.nb(NoEncs~Treatment*SizeDiff, data=cricket)
model.highestLevel.size.nb=glm.nb(HighestLevel~Treatment*SizeDiff, data=cricket)
model.overallWin.size.nb=glm.nb(Winner~Treatment*SizeDiff, data=cricket)
summary(modellatency.quasi.size.nb)
summary(model.duration.size.nb)
summary(model.1level.size.nb)
summary(model.enc.size.nb)
summary(model.highestLevel.size.nb)
summary(model.overallWin.size.nb)
# show no significance 

# without size 
modellatency.quasi.nb=glm.nb(LatTo1stEnc~Treatment, data=cricket)
model.duration.nb=glm.nb(Duration1stEnc~Treatment, data=cricket)
model.1level.nb=glm.nb(Level1stEnc~Treatment, data=cricket)
model.enc.nb=glm.nb(NoEncs~Treatment, data=cricket)
model.highestLevel.nb=glm.nb(HighestLevel~Treatment, data=cricket)
model.overallWin.nb=glm.nb(Winner~Treatment, data=cricket)
summary(modellatency.quasi.nb)
summary(model.duration.nb)
summary(model.1level.nb)
summary(model.enc.nb)## significance! 
summary(model.highestLevel.nb)
summary(model.overallWin.nb)

####### NON PARAMETRIC MODELS #########
#wilcox to see if first win = overall win 
wilcox.test(cricket$Winner1stEnc,cricket$Winner)#p> 0.05 --> not different, therefore first = overall 

#does not make any assumptions 
# kruskal wallis is a non-parametric test 
kruskal.test(LatTo1stEnc~Treatment, data=cricket)
kruskal.test(Level1stEnc~Treatment, data=cricket)

#post hoc analysis 
#non-parametric test between all categories of treatments of the latency 
#kruskal wallis is a low power test: does not make assumptions, ability to identify patterns is low, and data does not
#have enough repetitions 
library(PMCMR)
posthoc.kruskal.nemenyi.test(LatTo1stEnc~Treatment, data=cricket)
posthoc.kruskal.nemenyi.test(Level1stEnc~Treatment, data=cricket)
posthoc.kruskal.nemenyi.test(Duration1stEnc~Treatment, data=cricket)
posthoc.kruskal.nemenyi.test(NoEncs~Treatment, data=cricket)
posthoc.kruskal.nemenyi.test(HighestLevel~Treatment, data=cricket)

####### SIMULATIONS ########
# using random numbers to generate date with known characteristics and which follow hypothesized processes 
#install.packages("RCurl")
#library(RCurl) - RCurl is used to download data from the internet 

sample(letters,1)   #Draw a random letter
#letters = all the letters
EQ<-c('heads','tails')  #Define the ecologist's quarter
sample(EQ,20,replace=TRUE,p=c(0.4,0.6))

### Simulate a virtual room of participants *like* this one.
sim_heights<-rnorm(30,mean=5,sd=1.5)
sim_heights #Print the simulated heights
plot(sim_heights) #Plot the values
## Look at the histogram for comparison
hist(sim_heights,xlab='Heights (cm)',ylab='frequency',main='Virtual Cool Kids\' heights')

## Generating Random values from a variety of distributions
n=1000  ## Number of samples to draw (you can change this if you want)
par(mfrow=c(1,2),pty='s') ## Graphical parameters

# Normal
x<-rnorm(n,mean=0,sd=1)
plot(x,main='random draws from \n a standard normal') ## Here are all the randomly sampled points 
hist(x,probability=TRUE)                              ## A histogram of the samples
curve(dnorm(x,mean=0,sd=1),add=TRUE)                  ## Overlay the pdf to convince ourselves that the points were 
## infact drawn from that distribution.
# Beta
x<-rbeta(n,7,2)
plot(x,main='random draws \n from a beta distribution')
hist(x,probability=TRUE)
curve(dbeta(x,7,2),add=TRUE)

# Log-Normal
x<-rlnorm(n,0,1)
plot(x,main='random draws \n from a log-Normal distribution')
hist(x,probability=TRUE)
curve(dlnorm(x,0,1),add=TRUE)

# Exponential
x<-rexp(n,0.1)
plot(x,main='random draws \n from an exponential distribution')
hist(x,probability=TRUE)
curve(dexp(x,0.1),add=TRUE)

# Poisson
x<-rpois(n,3)
plot(x,main='random draws \n from a Poisson distribution')
hist(x,probability=TRUE,breaks=seq(-0.5,max(x)+0.5,1))
lines(0:10,dpois(0:10,3))

# Chi-squared
x<-rchisq(n,3)
plot(x,main='random draws \n from a chi-squared distribution')
hist(x,probability=TRUE)
curve(dchisq(x,3),add=TRUE)

# Binomial
x<-rbinom(n,size=10,p=0.7)
plot(x,main='random draws \n from a binomial distribution')
hist(x,probability=TRUE,breaks=seq(-0.5,10.5))
lines(1:10,dbinom(1:10,size=10,p=0.7))

## R has samplers for many more distributions: So, in general, you just use r<dist>(n,pars).

#@ 2.1.1 @#
##Simulate from a simple linear model.

#Model parameters
intercept<-10    #B_0
slope<-1        #B_1
error_sd<-2     #sigma

n<-30   #number of sample points

x<-runif(n,10,20) #Simulate x values

#Simulate from the model
y<- intercept + slope*x #Deterministic
y<-y + rnorm(n,0,error_sd) #Stochastic

#Plot the simulated data
plot(x,y,xlab='Length (cm)',ylab='Swimming speed (cm/s)')
##--##

abline(intercept,slope) #Plot the true (generating) relationship
fit<-lm(y~x)        #Run a linear regression on x and y
summary(fit)        #Results of the model
abline(fit,lty=2)   #Plot the best fit line


##Simulate tadpole predation according to a Holling type II function response model.
##

#Model parameters
a<-0.8
h<-0.04 
n<-300
N<-sample(1:100,n,replace=TRUE) #Simulate initial tadpole densities


#Simulate from the model
predprob<- a/(1+a*h*N) #Deterministic part
killed<- rbinom(n,prob=predprob,size=N) #Stocastic part

plot(N,killed,xlab='Initial population',ylab='Number killed')

curve(a/(1+a*h*x)*x,add=TRUE) #Add the response curve (the deterministic part)

# poisson simulation 
#https://stats.stackexchange.com/questions/17853/r-code-for-estimating-a-poisson-parameter-and-its-ci
#https://speakerdeck.com/cjbayesian/introduction-to-simulation-using-r
library(MASS)
parms <- fitdistr(sarahs.data, "poisson")
parms 
lambda  
3.72254335 
(0.04638706)

lambda <- parms$estimate
sd_x     <- parms$sd   