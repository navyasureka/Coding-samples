
list.of.packages <- c("tidyverse", "knitr","dbplyr","dplyr","ivpack","haven","broom","desc","lmtest","stargazer","tibble","ggplot2","psych","base","gmodels","spatstat", "xtable", "readxl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
invisible(lapply(list.of.packages, library, character.only = TRUE))


dat <- read_dta("data_ps3.dta")
#Q-1
cor(dat$dist_to_pri, dat$Private)

reg1 <- lm(dat$dist_to_pri ~ dat$low_ses)
summary(reg1)

reg <- lm(dat$Literacy ~ dat$low_ses)
summary(reg)

#Q-2
dat$diff <- dat$dist_to_pub - dat$dist_to_pri
reg1 <- lm(dat$Private~dat$dist_to_pub + dat$diff)


prob1 <- glm(Private ~ dist_to_pub + diff, family = binomial(link = "probit"), data=dat) 

stargazer(reg1, prob1)



pred1 <- predict(reg1)
pred2 <- predict(prob1, type="response")

# (1) Linear

lpred1 <- as.data.frame(pred1[dat$Private==1])
lpred0 <- as.data.frame(pred1[dat$Private==0])


colnames(lpred1)[1] <- "PropensityScore"
colnames(lpred0)[1] <- "PropensityScore"

ggplot(data = lpred1,
       aes(lpred1$PropensityScore)) +
  geom_histogram(binwidth=0.05,
                 col="black", 
                 fill="red", 
                 alpha = .5) +
  labs(title="Private School Students",
       subtitle="Linear Model",
       x="Predicted Propensity Score",
       y="Number of Pupils")
ggplot(data = lpred0,
       aes(lpred0$PropensityScore)) +
  geom_histogram(binwidth=0.05,
                 col="black", 
                 fill="red", 
                 alpha = .5) +
  labs(title="Public School Students",
       subtitle="Linear Model",
       x="Predicted Propensity Score",
       y="Number of Pupils")


# (2) Probit
ppred1 <- as.data.frame(pred2[dat$Private==1])
ppred0 <- as.data.frame(pred2[dat$Private==0])

colnames(ppred1)[1] <- "PropensityScore"
colnames(ppred0)[1] <- "PropensityScore"

ggplot(data = ppred1,
       aes(ppred1$PropensityScore)) +
  geom_histogram(binwidth=0.03,
                 col="black", 
                 fill="red", 
                 alpha = .5) +
  labs(title="Private School Students",
       subtitle="Probit Model",
       x="Predicted Propensity Score",
       y="Number of Pupils")
ggplot(data = ppred0,
       aes(ppred0$PropensityScore)) +
  geom_histogram(binwidth=0.03,
                 col="black", 
                 fill="red", 
                 alpha = .5) +
  labs(title="Public School Students",
       subtitle="Probit Model",
       x="Predicted Propensity Score",
       y="Number of Pupils")

#Q-3

# Selection Equation 
dat$p_hat <- pred2
dat$TT <- dat$Private

# Heckit Model
lambda_0 = function(p)  dnorm(qnorm(p))/(1 - pnorm(qnorm(p)))
lambda_1 = function(p) -dnorm(qnorm(p))/pnorm(qnorm(p))

# Mills
m_0 = lambda_0(dat$p_hat[dat$TT == 0])
m_1 = lambda_1(dat$p_hat[dat$TT == 1])

# Literacy
eq_0_l = lm(dat$Literacy[dat$TT == 0] ~ m_0)
eq_0_l
eq_1_l = lm(dat$Literacy[dat$TT == 1] ~ m_1)
eq_1_l


# beta_0- 5.429187
# beta_1- 6.850766
beta0_lit <- coef(eq_0_l)[1]
beta1_lit <- coef(eq_1_l)[1]

# Numeracy
eq_0_n = lm(dat$Numeracy[dat$TT == 0] ~ m_0)
print(eq_0_n)
eq_1_n = lm(dat$Numeracy[dat$TT == 1] ~ m_1)
eq_1_n

# beta_0- 6.010748
# beta_1- 6.994086

beta0_num <- coef(eq_0_n)[1]
beta1_num <- coef(eq_1_n)[1]

#epsilons of the outcome equations

# Literacy
dat$eps0_lit <- dat$Literacy - beta0_lit
epsilon0_lit<- mean(dat$eps0_lit)
dat$eps1_lit <- dat$Literacy - beta1_lit
epsilon1_lit<-mean(dat$eps1_lit)

# Numeracy
dat$eps0_num <- dat$Numeracy - beta0_num
epsilon0_num<-mean(dat$eps0_num)
dat$eps1_num <- dat$Numeracy - beta1_num
epsilon1_num<-mean(dat$eps1_num)

#Table

a <- matrix(c(beta0_lit,beta0_num,beta1_lit,beta1_num,epsilon0_lit,epsilon0_num,epsilon1_lit,epsilon1_num),ncol=2,byrow=TRUE)
colnames(a) <- c("Literacy","Numeracy")
rownames(a) <- c("Beta_0","Beta_1","Epsilon(0)", "Epsilon(1)")
a <- as.table(a)
a
xtable(a)

#Q-4

#Suppose that you observe a population that has a baseline average probability to
#enter the treatment of p0 = 0.1. An experiment manages to increase this probability to p1 = 0.2. From the
#estimates obtained in the previous question compute the LATE associated to this experiment. What if p
#increases from p0 = 0.8 to p1 = 0.9? Finally, compute the ATE, the ATT, and the ATU and comment the
#results.

p0<-0.1
p1<-0.2

Gamma = function(p0, p1, l1){
  (p1*l1(p1) - p0*l1(p0))/(p1 - p0)
}

LATE_CF_l = coef(eq_1_l)[1] - coef(eq_0_l)[1] + 
  (coef(eq_1_l)[2] - coef(eq_0_l)[2])*Gamma(p0, p1, lambda_1)
names(LATE_CF_l) = NULL

LATE_CF_n = coef(eq_1_n)[1] - coef(eq_0_n)[1] + 
  (coef(eq_1_n)[2] - coef(eq_0_n)[2])*Gamma(p0, p1, lambda_1)

names(LATE_CF_n) = NULL

p0_n<-0.8
p1_n<-0.9

Gamma_n = function(p0_n, p1_n, l1){
  (p1_n*l1(p1_n) - p0_n*l1(p0_n))/(p1_n - p0_n)
}

LATE_CF_l1 = coef(eq_1_l)[1] - coef(eq_0_l)[1] + 
  (coef(eq_1_l)[2] - coef(eq_0_l)[2])*Gamma_n(p0_n, p1_n, lambda_1)
names(LATE_CF_l1) = NULL

LATE_CF_n1 = coef(eq_1_n)[1] - coef(eq_0_n)[1] + 
  (coef(eq_1_n)[2] - coef(eq_0_n)[2])*Gamma_n(p0_n, p1_n, lambda_1)
names(LATE_CF_n1)= NULL

result<-matrix(c(LATE_CF_l, LATE_CF_n, LATE_CF_l1, LATE_CF_n1), ncol=2)

colnames(result)<- c ("Old Values", "New Values")
rownames(result)<- c ("Literacy", "Numeracy")

result<-as.table(result)
xtable(result)


ATE = a[2,] - a[1,]
outcomes<- as.matrix(c(ATE))
colnames(outcomes)<-c("ATE")

# Under normality Assumption

ATT_l = ATE[1] + (coef(eq_1_l)[2] - coef(eq_0_l)[2])*mean(lambda_1(dat$p_hat[dat$TT == 1]))
ATT_n = ATE[2] + (coef(eq_1_n)[2] - coef(eq_0_n)[2])*mean(lambda_1(dat$p_hat[dat$TT==1]))
ATU_l = ATE[1] + (coef(eq_1_l)[2] - coef(eq_0_l)[2])*mean(lambda_0(dat$p_hat[dat$TT==0]))
ATU_n = ATE[2] + (coef(eq_1_n)[2] - coef(eq_0_n)[2])*mean(lambda_0(dat$p_hat[dat$TT==0]))

pp = mean(dat$TT)

ATE_l1 = ATT_l*pp + (1-pp)*ATU_l
ATE_n1 = ATT_n*pp + (1-pp)*ATU_n

outcomes<-matrix(c(ATE_l1, ATE_n1, ATT_l, ATT_n, ATU_l, ATU_n), ncol=3)
colnames(outcomes)<-c ("ATE", "ATT", "ATU")
rownames(outcomes)<-c("Literacy", "Numeracy")

outcomes<-as.table(outcomes)

xtable(outcomes)

#Question 5: Parts needed for Q9


######################################################################################### #
######################################################################################### #
######################################################################################### #

#propensity score is p_hat
dat$p_hat <- pred2
dat$p_hat2 <- (dat$p_hat)^2
dat$p_hat3 <- (dat$p_hat)^3
dat$p_hat4 <- (dat$p_hat)^4

# (1) Literacy

reg_lit <- lm(dat$Literacy ~ dat$p_hat + dat$p_hat2 + dat$p_hat3 + dat$p_hat4)
summary(reg_lit)

## Alphas for Lit 
alpha0_lit <- reg_lit$coefficients[1]
alpha1_lit <- reg_lit$coefficients[2]
alpha2_lit <- reg_lit$coefficients[3]
alpha3_lit <- reg_lit$coefficients[4]
alpha4_lit <- reg_lit$coefficients[5]

# (2) Numeracy

reg_num <- lm(dat$Numeracy ~ dat$p_hat + dat$p_hat2 + dat$p_hat3 + dat$p_hat4)
summary(reg_num)

## Alphas for Num
alpha0_num <- reg_num$coefficients[1]
alpha1_num <- reg_num$coefficients[2]
alpha2_num <- reg_num$coefficients[3]
alpha3_num <- reg_num$coefficients[4]
alpha4_num <- reg_num$coefficients[5]


###Compute the MTE for Lit and Num separately 

#### Literacy
dat$MTE_lit <- alpha1_lit + (2 * alpha2_lit * dat$p_hat) + (3 * alpha3_lit * dat$p_hat2) + (4 * alpha4_lit * dat$p_hat3) 

#### Numeracy
dat$MTE_num <- alpha1_num + (2 * alpha2_num * dat$p_hat) + (3 * alpha3_num * dat$p_hat2) + (4 * alpha4_num * dat$p_hat3) 
View(dat)


# Q9:


summary(dat$dist_to_pri_new)
dat2 <- dat

dat2$diff <- dat2$dist_to_pub - dat2$dist_to_pri_new
mean(dat$diff) #605.5562
mean(dat2$diff) #3120.247
# -> The distance diff between pub and priv is higher after the new private schools
# were built because the new private schools are even closer (even before public
# schools were further away, but less so than after the policy)

#Predict and compare the propensity scores
dat2$p_hat_new <- predict(prob1, newdata = dat2, type="response")

p_hat_diff <- dat2$p_hat_new - dat2$p_hat
summary(p_hat_diff)
hist(p_hat_diff)

ggplot(data=dat2, aes(p_hat_diff)) + 
  geom_histogram()




# Total Difference
diff <- as.data.frame(p_hat_diff)
colnames(diff)[1] <- "Diff"
View(diff)

ggplot(data = diff,
       aes(diff$Diff)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)), binwidth=0.05,
                 col="black", 
                 fill="yellow", 
                 alpha = .6) +
  labs(title="Propensity Score Difference (New - Old)",
       subtitle="Total",
       x="Difference",
       y="Share of Propensity Scores")+
  scale_y_continuous(labels = scales::percent)

# Difference for public
diff <- as.data.frame(p_hat_diff[dat2$Private==0])
colnames(diff)[1] <- "Diff"
View(diff)

ggplot(data = diff,
       aes(diff$Diff)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)), binwidth=0.05,
                 col="black", 
                 fill="yellow", 
                 alpha = .6) +
  labs(title="Propensity Score Difference (New - Old)",
       subtitle="Public School Pupils",
       x="Difference",
       y="Share of Propensity Scores")+
  scale_y_continuous(labels = scales::percent)

# Difference for private
diff <- as.data.frame(p_hat_diff[dat2$Private==1])
colnames(diff)[1] <- "Diff"
View(diff)

ggplot(data = diff,
       aes(diff$Diff)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)), binwidth=0.05,
                 col="black", 
                 fill="yellow", 
                 alpha = .6) +
  labs(title="Propensity Score Difference (New - Old)",
       subtitle="Private School Pupils",
       x="Difference",
       y="Share of Propensity Scores")+
  scale_y_continuous(labels = scales::percent)



# NeW
hist(dat2$p_hat_new[dat2$Private==1])
hist(dat2$p_hat_new[dat2$Private==0])

# Old
hist(dat$p_hat[dat$Private==1])
hist(dat$p_hat[dat$Private==0])


### Private School Students
dpred <- as.data.frame(dat2$p_hat_new[dat2$Private==1])
colnames(dpred)[1] <- "PropensityScore"
View(dpred)

# In percent
ggplot(data = dpred,
       aes(dpred$PropensityScore)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)), binwidth=0.05,
                 col="black", 
                 fill="red", 
                 alpha = .6) +
  labs(title="Private School Pupils - New Propensity Score",
       subtitle="",
       x="Predicted Propensity Score",
       y="Share of Pupils")+
  scale_y_continuous(labels = scales::percent)

# Public School Students
dpred <- as.data.frame(dat2$p_hat_new[dat2$Private==0])
colnames(dpred)[1] <- "PropensityScore"
View(dpred)

# In percent
ggplot(data = dpred,
       aes(dpred$PropensityScore)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)), binwidth=0.05,
                 col="black", 
                 fill="red", 
                 alpha = .6) +
  labs(title="Public School Pupils - New Propensity Score",
       subtitle="",
       x="Predicted Propensity Score",
       y="Share of Pupils")+
  scale_y_continuous(labels = scales::percent)



# PRTE 
## Literacy
PRTE_lit <-  mean((dat2$p_hat_new - dat2$p_hat)/(mean(dat2$p_hat_new) - mean(dat2$p_hat) ) * (dat2$MTE_lit)) 
# 1.243836
## Numeracy
PRTE_num <-  mean((dat2$p_hat_new - dat2$p_hat)/(mean(dat2$p_hat_new) - mean(dat2$p_hat) ) * (dat2$MTE_num)) 
# 0.6021439

PRTE<-matrix(c(PRTE_lit, PRTE_num))
PRTE<-as.table(PRTE)
xtable(PRTE)


#Q-5 to Q-7( Shreya's code)

df<-dat

####Question 5 

#propensity score is p_hat
df$p_hat <- pred_probit
df$p_hat2 <- (df$p_hat)^2
df$p_hat3 <- (df$p_hat)^3
df$p_hat4 <- (df$p_hat)^4

# (1) Literacy

reg_lit <- lm(df$Literacy ~ df$p_hat + df$p_hat2 + df$p_hat3 + df$p_hat4)
summary(reg)

## Alphas for Lit 
alpha0_lit <- reg_lit$coefficients[1]
alpha1_lit <- reg_lit$coefficients[2]
alpha2_lit <- reg_lit$coefficients[3]
alpha3_lit <- reg_lit$coefficients[4]
alpha4_lit <- reg_lit$coefficients[5]

# (2) Numeracy

reg_num <- lm(df$Numeracy ~ df$p_hat + df$p_hat2 + df$p_hat3 + df$p_hat4)
summary(reg)

## Alphas for Num
alpha0_num <- reg_num$coefficients[1]
alpha1_num <- reg_num$coefficients[2]
alpha2_num <- reg_num$coefficients[3]
alpha3_num <- reg_num$coefficients[4]
alpha4_num <- reg_num$coefficients[5]


###Compute the MTE for Lit and Num separately 

#### Literacy
df$MTE_lit <- alpha1_lit + (2 * alpha2_lit * df$p_hat) + (3 * alpha3_lit * df$p_hat2) + (4 * alpha4_lit * df$p_hat3) 

#### Numeracy
df$MTE_num <- alpha1_num + (2 * alpha2_num * df$p_hat) + (3 * alpha3_num * df$p_hat2) + (4 * alpha4_num * df$p_hat3) 
View(df)

### Plotting MTE for Lit and Num 

ggplot(data=df, aes(x=p_hat)) +
  geom_line(aes(y = MTE_num, colour= "Numeracy")) +
  geom_line(aes(y = MTE_lit, colour= "Literacy")) +
  geom_ribbon(aes(ymin = MTE_num*0.95, ymax = MTE_num*1.05), alpha = 0.2)  +
  geom_ribbon(aes(ymin = MTE_lit*0.95, ymax = MTE_lit*1.05), alpha = 0.2)  +
  scale_color_discrete(name = "Outcome") +
  labs(x="Propensity Score", y="MTE")

### Calculating MTE for specific prop scores (p_hat)

# New df for MTE of Lit and Num and Prop Scores 
MTE_df <- df[c("p_hat", "MTE_num", "MTE_lit")]
MTE_df$p_hat_round <- round(MTE_df$p_hat, digits = 2) #rounding p_hat to 2 decimal places

#MTE of Numeracy for specific prop scores 
MTE_num_p0 <- median(MTE_df$MTE_num[MTE_df$p_hat_round==0.00]) #-0.443
MTE_num_p025 <- median(MTE_df$MTE_num[MTE_df$p_hat_round==0.25]) #0.462
MTE_num_p050 <- median(MTE_df$MTE_num[MTE_df$p_hat_round==0.50]) #0.988
MTE_num_p075 <- median(MTE_df$MTE_num[MTE_df$p_hat_round==0.75]) #1.494
MTE_num_p1 <- median(MTE_df$MTE_num[MTE_df$p_hat_round==1.00]) #2.347

# MTE of Literacy for specific prop scores 
MTE_lit_p0 <- median(MTE_df$MTE_lit[MTE_df$p_hat_round==0.00]) #0.503
MTE_lit_p025 <- median(MTE_df$MTE_lit[MTE_df$p_hat_round==0.25]) #1.236
MTE_lit_p050 <- median(MTE_df$MTE_lit[MTE_df$p_hat_round==0.50]) #1.470
MTE_lit_p075 <- median(MTE_df$MTE_lit[MTE_df$p_hat_round==0.75]) #1.550
MTE_lit_p1 <- median(MTE_df$MTE_lit[MTE_df$p_hat_round==1.00]) #1.830


#### Question 7 
#For the functions of diff treatment effects as a weight of MTE see the overleaf

### Computing Weights 

## Weight for ATE is 1 
##Weight for ATT is P(T_i=1|U_i=u)/P(T_i=1)
df$weight_t = (df$p_hat)/mean(df$p_hat)
## Weight for ATU is P(T_i=0|U_i=u)/P(T_i=0)
df$weight_u = ((1-df$p_hat)/(1-mean(df$p_hat)))
df$weight_ate = 1 

### Plotting the functions 

library(ggplot2)

ggplot(data = df,
       aes(df$weight_t)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)), binwidth=0.05,
                 col="black", 
                 fill="green", 
                 alpha = .6) +
  labs(title="Treated Group",
       subtitle="P(T_i = 1 | U_i = u)",
       x="Weights of MTE",
       y="Density")+
  scale_y_continuous(labels = scales::percent)


ggplot(data = df,
       aes(df$weight_u)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)), binwidth=0.05,
                 col="black", 
                 fill="blue", 
                 alpha = .6) +
  labs(title="Untreated Group",
       subtitle="P(T_i=0|U_i=u)",
       x="Weights of MTE",
       y="Density")+
  scale_y_continuous(labels = scales::percent)


### Alternate graph that I think is more relevant 

ggplot(data = df) + 
  geom_line(aes(x = p_hat, y = weight_t, colour = "ATT"))+
  geom_line(aes(x = p_hat, y = weight_u, colour = "ATU"))+
  geom_line(aes(x = p_hat, y = weight_ate, colour = "ATE"))+
  scale_color_discrete(name = "Treatment Effect") +
  labs(x="Propensity Score", y="MTE Weights") 

