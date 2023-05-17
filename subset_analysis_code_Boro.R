#-----------------------------------------SEASON: Boro---------------------------------------------

library(readxl)
library(dplyr)
library(Matrix)
library(lme4)
library(lmerTest)

# DataBoro1 <- readxl::read_xlsx("DataBoro1.xlsx", sheet = 1)
# str(DataBoro1)
# ## excluding year: 2000 and 2022
# DataBoro1 <- DataBoro1 %>% filter(!Year %in% c("2021", "2022"))
# DataBoro1$L     <- as.factor(DataBoro1$Location)
# DataBoro1$Y     <- as.factor(DataBoro1$Year)
# str(DataBoro1)
# 
# DataBoro2 <- readxl::read_xlsx("DataBoro2.xlsx", sheet = 1)
# DataBoro2 <- DataBoro2 %>% filter(!Year %in% "2021")
# names(DataBoro2)
# DataBoro2$L <- as.factor(DataBoro2$Location)
# DataBoro2$Y <- as.factor(DataBoro2$Year)
# DataBoro2$G <- as.factor(DataBoro2$Genotype)
# str(DataBoro2)
# 
# dat <- merge(DataBoro1,DataBoro2,by=c('L','Y'))
# str(dat)
# 
# 
# # correct format (num or Factor) and give them shorter names
# dat$r.i   <- as.numeric(dat$YOR)
# dat$t.k   <- as.numeric(dat$Year.x)
# dat$G     <- as.factor(dat$Genotype)
# dat$L     <- as.factor(dat$Location.x)
# dat$Rep   <- as.factor(dat$Rep)
# dat$Group <- as.factor(dat$Group)
# 
# dat$Env   <- as.factor(paste(dat$Y, dat$L, sep="-"))
# 
# data_final <- dat[ ,c("Y", "t.k", "r.i", "L", "Env", "MAXT", 
#                       "MINT", "RAIN", "HUMIDITY","SUNSH", "G", "Rep", "Group", "Yield", "Wind")]
# 
# # Reduce the dataset to only those 13 columns we name above.
# 
# str(data_final)
# 
# write.csv(data_final, "data_final.csv", row.names = F)

setwd("E:\\Niaz Bhai\\Boro")

borodata <- read.csv("data_final_boro.csv")

borodata$Y <- as.numeric(as.character(borodata$Y))

mod1 <- lmer(Yield ~ r.i + t.k + MAXT + MINT + RAIN + HUMIDITY + SUNSH +Wind+
               (1|G) + (1|L) + (1|Y) + (1|L:Y) + (1|L:G) +(1|G:Y)+(1|G:L:Y),borodata,
             control=lmerControl(check.nobs.vs.nlev = "ignore",
                                 check.nobs.vs.rankZ = "ignore",
                                 check.nobs.vs.nRE="ignore"))

summary(mod1)
coef(summary(mod1))

library(cAIC4)
library(MASS)

(step_res <- step(mod1))
final <- get_model(step_res)
anova(final)

confint(final)

coef(summary(mod1)) # fixed effects solutions

anova(mod1) # anova for fixed effects



##################Aman#################

Amandata <- read.csv("data_final_Aman.csv")

Amandata$Y <- as.numeric(as.character(Amandata$Y))

mod1 <- lmer(Yield ~ r.i + t.k + MAXT + MINT + RAIN + HUMIDITY + SUNSH +WIND+
               (1|G) + (1|L) + (1|Y) + (1|L:Y) + (1|L:G) +(1|G:Y)+(1|G:L:Y),Amandata,
             control=lmerControl(check.nobs.vs.nlev = "ignore",
                                 check.nobs.vs.rankZ = "ignore",
                                 check.nobs.vs.nRE="ignore"))

summary(mod1)
coef(summary(mod1))

library(cAIC4)
library(MASS)

(step_res <- step(mod1))
final <- get_model(step_res)
anova(final)

confint(final)

coef(summary(mod1)) # fixed effects solutions

anova(mod1) # anova for fixed effects


###################################################
### Step: Mean yield per genotype and environment
###################################################
library(data.table) 

# data_final <- as.data.table(data_final) # reformat our data to "data.table" format
# 
# data_final[ , mean.yield:=mean(Yield), by=c("Env", "G")] # creates column "mean.yield"
#     # This way of getting the mean is part of the data.table package/format.
#     # It creates gets the mean of the numbers in the "Yield" columns for each "Env"-"G"
#     # combination and saves it into a new column called "mean.yield".
# 
# 
# data_final <- data_final[ , -c("Rep", "Yield")] # Drop "Rep" & "Yield" columns. 
# # They are now unnessary, since we focus on the mean yields
# meandata <- unique(data_final)            # Drop duplicate rows
# 
# # write.csv(meandata, "meandata.csv", row.names = F)
# 
# str(meandata)



################################################
################ subset 1######################
################################################

subset1 <- as.data.table(subset1) # reformat our data to "data.table" format

subset1[ , mean.yield:=mean(Yield), by=c("Env", "G")] # creates column "mean.yield"
# This way of getting the mean is part of the data.table package/format.
# It creates gets the mean of the numbers in the "Yield" columns for each "Env"-"G"
# combination and saves it into a new column called "mean.yield".


subset1 <- subset1[ , -c("Rep", "Yield")] # Drop "Rep" & "Yield" columns. 
# They are now unnessary, since we focus on the mean yields
meandata <- unique(subset1)            # Drop duplicate rows


##################################################
### Step: Estimate genetic trends in mixed model
##################################################
# Two packages required: lme4, lmerTest
library(Matrix)
library(lme4)
library(lmerTest)
# library(min)

# Fitting the model
# This model is based on the basic model for long-term MET data 
# (slide 7 in "Piepho - trend.pdf") and extended via incorporating regression 
# terms for a genetic and a non-genetic trend, respectively
# (slide 14 in "Piepho - trend.pdf")

mod1 <- lmer(mean.yield ~ r.i + t.k + MAXT + MINT + RAIN + HUMIDITY + SUNSH +Wind+
               (1|G) + (1|L) + (1|Y) + (1|L:Y) + (1|L:G) +(1|G:Y)+(1|G:L:Y),meandata,
             control=lmerControl(check.nobs.vs.nlev = "ignore",
                                 check.nobs.vs.rankZ = "ignore",
                                 check.nobs.vs.nRE="ignore"))

sub1 <- lmer(mean.yield ~ r.i + t.k + (1|G) + (1|L) + (1|Y) + (1|L:Y) + (1|L:G) +(1|G:Y)+(1|G:L:Y),meandata,
             control=lmerControl(check.nobs.vs.nlev = "ignore",
                                 check.nobs.vs.rankZ = "ignore",
                                 check.nobs.vs.nRE="ignore"))
summary(sub1)
coef(summary(sub1))

# Fixed effects are written plain, while random effects are in the (1|...) notation
# as seen above. 
# Note also that r.i and t.k are numeric variables in our dataset and thus a single 
# slope will be estimated for them, respectively,  while G, Y, L (and thus all 
# their interactions, too,) are factors and thus receive an effect estimate 
# for each level. 
# When comparing our model to Prof. Piepho's slide 7 in "Piepho - trend.pdf", 
# it seems that we are missing two effects: "?" and "(1|Y:L:G)"
# This is not true, however, since lmer (and most statistical softwares) will
# by default put a general intercept (i.e. ?) and an error effect in any model.
# In this case, the three-way-interaction Y:L:G is in fact the error effect.

###################################
### Step 5: Viewing results for mod
###################################
summary(mod1) # summary of model results

confint(mod1)

coef(summary(mod1)) # fixed effects solutions
# the estimate (i.e. slope) for r.i is the genetic trend
# the estimate (i.e. slope) for t.k is the non-genetic trend
# the estimate (i.e. slope) for max, min, rain and RH are the environment trend
# you can also find their respective standard errors and p-values from t-tests
# Detail: the latter are obtained via t-tests which use Satterthwaite's DF



anova(mod1) # anova for fixed effects
# here you can find p-values from F-tests for r.i and t.k
# Detail: this is a Type III Analysis of Variance Table with Satterthwaite's DF

###############################
### Obtain BLUPs for genotypes
###############################
G.BLUPs.lme <- ranef(mod1)$G
G.BLUPs.lme


################################################
################ subset 2 ######################
################################################

subset2 <- as.data.table(subset2) # reformat our data to "data.table" format

subset2[ , mean.yield:=mean(Yield), by=c("Env", "G")] # creates column "mean.yield"


subset2 <- subset2[ , -c("Rep", "Yield")] # Drop "Rep" & "Yield" columns. 
meandata <- unique(subset2)            # Drop duplicate rows


mod2 <- lmer(mean.yield ~ r.i + t.k + MAXT + MINT + RAIN + HUMIDITY + SUNSH +Wind+
               (1|G) + (1|L) + (1|Y) + (1|L:Y) + (1|L:G) +(1|G:Y)+(1|G:L:Y),meandata,
             control=lmerControl(check.nobs.vs.nlev = "ignore",
                                 check.nobs.vs.rankZ = "ignore",
                                 check.nobs.vs.nRE="ignore"))

sub2 <- lmer(mean.yield ~ r.i + t.k + (1|G) + (1|L) + (1|Y) + (1|L:Y) + (1|L:G) +(1|G:Y)+(1|G:L:Y),meandata,
             control=lmerControl(check.nobs.vs.nlev = "ignore",
                                 check.nobs.vs.rankZ = "ignore",
                                 check.nobs.vs.nRE="ignore"))
summary(sub2)
coef(summary(sub2))


summary(mod2) # summary of model results

confint(mod2)

coef(summary(mod2)) # fixed effects solutions



anova(mod2) # anova for fixed effects

### Obtain BLUPs for genotypes
G.BLUPs.lme <- ranef(mod2)$G
G.BLUPs.lme


################################################
################ subset 3 ######################
################################################

subset3 <- as.data.table(subset3) # reformat our data to "data.table" format

subset3[ , mean.yield:=mean(Yield), by=c("Env", "G")] # creates column "mean.yield"


subset3 <- subset3[ , -c("Rep", "Yield")] # Drop "Rep" & "Yield" columns. 
meandata <- unique(subset3)            # Drop duplicate rows


mod3 <- lmer(mean.yield ~ r.i + t.k + MAXT + MINT + RAIN + HUMIDITY + SUNSH +Wind+
               (1|G) + (1|L) + (1|Y) + (1|L:Y) + (1|L:G) +(1|G:Y)+(1|G:L:Y),meandata,
             control=lmerControl(check.nobs.vs.nlev = "ignore",
                                 check.nobs.vs.rankZ = "ignore",
                                 check.nobs.vs.nRE="ignore"))

sub3 <- lmer(mean.yield ~ r.i + t.k + (1|G) + (1|L) + (1|Y) + (1|L:Y) + (1|L:G) +(1|G:Y)+(1|G:L:Y),meandata,
             control=lmerControl(check.nobs.vs.nlev = "ignore",
                                 check.nobs.vs.rankZ = "ignore",
                                 check.nobs.vs.nRE="ignore"))
summary(sub3)
coef(summary(sub3))

summary(mod3) # summary of model results

confint(mod3)

coef(summary(mod3)) # fixed effects solutions



anova(mod3) # anova for fixed effects

### Obtain BLUPs for genotypes
G.BLUPs.lme <- ranef(mod3)$G
G.BLUPs.lme


################################################
################ subset 4 ######################
################################################

subset4 <- as.data.table(subset4) # reformat our data to "data.table" format

subset4[ , mean.yield:=mean(Yield), by=c("Env", "G")] # creates column "mean.yield"


subset4 <- subset4[ , -c("Rep", "Yield")] # Drop "Rep" & "Yield" columns. 
meandata <- unique(subset4)            # Drop duplicate rows


mod4 <- lmer(mean.yield ~ r.i + t.k + MAXT + MINT + RAIN + HUMIDITY + SUNSH +Wind+
               (1|G) + (1|L) + (1|Y) + (1|L:Y) + (1|L:G) +(1|G:Y)+(1|G:L:Y),meandata,
             control=lmerControl(check.nobs.vs.nlev = "ignore",
                                 check.nobs.vs.rankZ = "ignore",
                                 check.nobs.vs.nRE="ignore"))

sub4 <- lmer(mean.yield ~ r.i + t.k + (1|G) + (1|L) + (1|Y) + (1|L:Y) + (1|L:G) +(1|G:Y)+(1|G:L:Y),meandata,
             control=lmerControl(check.nobs.vs.nlev = "ignore",
                                 check.nobs.vs.rankZ = "ignore",
                                 check.nobs.vs.nRE="ignore"))
summary(sub4)
coef(summary(sub4))

summary(mod4) # summary of model results

confint(mod4)

coef(summary(mod4)) # fixed effects solutions



anova(mod4) # anova for fixed effects

### Obtain BLUPs for genotypes
G.BLUPs.lme <- ranef(mod4)$G
G.BLUPs.lme

################################################
################ subset 11 ######################
################################################

subset11 <- as.data.table(subset11) # reformat our data to "data.table" format

subset11[ , mean.yield:=mean(Yield), by=c("Env", "G")] # creates column "mean.yield"


subset11 <- subset11[ , -c("Rep", "Yield")] # Drop "Rep" & "Yield" columns. 
meandata <- unique(subset11)            # Drop duplicate rows


mod4 <- lmer(mean.yield ~ r.i + t.k + MAXT + MINT + RAIN + HUMIDITY + SUNSH +Wind+
               (1|G) + (1|L) + (1|Y) + (1|L:Y) + (1|L:G) +(1|G:Y)+(1|G:L:Y),meandata,
             control=lmerControl(check.nobs.vs.nlev = "ignore",
                                 check.nobs.vs.rankZ = "ignore",
                                 check.nobs.vs.nRE="ignore"))

sub11 <- lmer(mean.yield ~ r.i + t.k + (1|G) + (1|L) + (1|Y) + (1|L:Y) + (1|L:G) +(1|G:Y)+(1|G:L:Y),meandata,
              control=lmerControl(check.nobs.vs.nlev = "ignore",
                                  check.nobs.vs.rankZ = "ignore",
                                  check.nobs.vs.nRE="ignore"))
summary(sub11)
coef(summary(sub11))

summary(mod4) # summary of model results

confint(mod4)

coef(summary(mod4)) # fixed effects solutions



anova(mod4) # anova for fixed effects

### Obtain BLUPs for genotypes
G.BLUPs.lme <- ranef(mod4)$G
G.BLUPs.lme

################################################
################ subset 22 ######################
################################################

subset22 <- as.data.table(subset22) # reformat our data to "data.table" format

subset22[ , mean.yield:=mean(Yield), by=c("Env", "G")] # creates column "mean.yield"


subset22 <- subset22[ , -c("Rep", "Yield")] # Drop "Rep" & "Yield" columns. 
meandata <- unique(subset22)            # Drop duplicate rows


mod4 <- lmer(mean.yield ~ r.i + t.k + MAXT + MINT + RAIN + HUMIDITY + SUNSH +Wind+
               (1|G) + (1|L) + (1|Y) + (1|L:Y) + (1|L:G) +(1|G:Y)+(1|G:L:Y),meandata,
             control=lmerControl(check.nobs.vs.nlev = "ignore",
                                 check.nobs.vs.rankZ = "ignore",
                                 check.nobs.vs.nRE="ignore"))

sub22 <- lmer(mean.yield ~ r.i + t.k + (1|G) + (1|L) + (1|Y) + (1|L:Y) + (1|L:G) +(1|G:Y)+(1|G:L:Y),meandata,
              control=lmerControl(check.nobs.vs.nlev = "ignore",
                                  check.nobs.vs.rankZ = "ignore",
                                  check.nobs.vs.nRE="ignore"))
summary(sub22)
coef(summary(sub22))

summary(mod4) # summary of model results

confint(mod4)

coef(summary(mod4)) # fixed effects solutions



anova(mod4) # anova for fixed effects

### Obtain BLUPs for genotypes
G.BLUPs.lme <- ranef(mod4)$G
G.BLUPs.lme
