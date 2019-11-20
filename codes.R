# Part 1
set.seed(12345)
lambda<-0.2
n<-40
sim<-1000
df<-matrix(rexp(n*sim,lambda),sim,n)
dfmean<-apply(df,1,mean)

# 1.Show the sample mean and compare it to the theoretical mean of the distribution.
mean(dfmean) # sample mean
1/lambda     # theo mean
1/lambda-mean(dfmean)  # Diff btw theoretical and sample means

# 2.Show how variable the sample is (via variance) 
# and compare it to the theoretical variance of the distribution.
var(dfmean) # sample var
(1/lambda)^2/n # theo var
(1/lambda)^2/n-var(dfmean) # Diff btw theo and sample variances

# 3.Show that the distribution is approximately normal.
z<-(dfmean-mean(dfmean))/(sd(dfmean)) # Standard normal
mean(z)
var(z)

hist(df) # histogram of large collection of random exponentials
hist(dfmean) # histogram of large collection of means of 40 exponentials

#####

# Part 2

library(datasets)

#1.Load the ToothGrowth data and perform some basic exploratory data analyses
head(ToothGrowth)
summary(ToothGrowth)
str(ToothGrowth)

#2. Seperate data
oj<-ToothGrowth[ToothGrowth$supp=="OJ",]
vc<-ToothGrowth[ToothGrowth$supp=="VC",]
t.test(oj$len, alternative = "two.sided")$conf
t.test(vc$len, alternative = "two.sided")$conf

half<-ToothGrowth[ToothGrowth$dose==0.5,]
one<-ToothGrowth[ToothGrowth$dose==1,]
two<-ToothGrowth[ToothGrowth$dose==2,]
t.test(half$len, alternative = "two.sided")$conf
t.test(one$len, alternative = "two.sided")$conf
t.test(two$len, alternative = "two.sided")$conf
