## obs1, obs0: observed number of events in intervention and control group
## exp1, exp0: expected number of events in intervention and control group
## obs1Add, obs0Add: number of events to be added to observed number of events in sensitivity analysis
## w: weight describing how the edditional events obs1Add + obs0Add are distributed to the expected number of events

#The expected number of events is calculated under H_0 (assume no difference between the two groups)
#HR=(obs1/exp1)/(obs0/exp0)= ratio of relative hazard (hazard rate compared to the hazard rate under the null hypothesis) of the two groups
#Therefore HR=(obs1/obs0)/(exp1/exp0)
#Following this we have se(log(HR))=sqrt((1/exp1)+(1/exp0), which depends only on the number of events

sensitivity <- function(obs1, obs0, exp1, exp0, obs1Add, obs0Add, w=0.5){
## standard error and hazard ratio of primary analysis
    seP <- sqrt(1/exp1+1/exp0)
    HRP <- (obs1/obs0)/(exp1/exp0)

## add new observed counts to observed number of events
    obs1new <- obs1 + obs1Add
    obs0new <- obs0 + obs0Add

## add new observed counts to expected number of events 
    obsAdd <- obs1Add + obs0Add 
    exp0new <- exp0 + w*obsAdd 
    exp1new <- exp1 + (1-w)*obsAdd 
    
## standard error and hazard ratio of sensitivity analysis
    seS <- sqrt(1/exp1new+1/exp0new)
    HRS <- (obs1new/obs0new)/(exp1new/exp0new)
## ratio of standard errors and hazard ratios    
    ratioSE <- seS/seP
    ratioHR <- HRS/HRP
    return(c(ratioSE, ratioHR))
}

#

myi <- seq(from=-10, to=10, by=1)
result <- matrix(NA, ncol=2, nrow=length(myi))
f1 <- 1
f0 <- 1
myw <- 0.5

par(las=1, mfrow=c(2,2))
## Figure 1
myobs1 <- 20
myobs0 <- 30
myexp1 <- myexp0 <- 25
HR <- (myobs1/myobs0)/(myexp1/myexp0)
for(i in 1:length(myi))
    result[i,]  <- sensitivity(obs1=myobs1, obs0=myobs0, exp1=myexp1, exp0=myexp0, obs1Add=f1*myi[i], obs0Add=f0*myi[i], w=myw)
par(las=1)
plot(result, type="b", xlab="ratio of standard errors", ylab="ratio of hazard ratios", log="xy", ylim=c(0.7, 10/7))
title(paste("Primary hazard ratio =", as.character(round(HR, 2))))
abline(h=1, lty=2)
abline(v=1, lty=2)
## Figure 2
myobs1 <- 23
myobs0 <- 27
myexp1 <- myexp0 <- 25
HR <- (myobs1/myobs0)/(myexp1/myexp0)
for(i in 1:length(myi))
    result[i,]  <- sensitivity(obs1=myobs1, obs0=myobs0, exp1=myexp1, exp0=myexp0, obs1Add=f1*myi[i], obs0Add=f0*myi[i], w=myw)
par(las=1)
plot(result, type="b", xlab="ratio of standard errors", ylab="ratio of hazard ratios", log="xy", ylim=c(0.7, 10/7))
title(paste("Primary hazard ratio =", as.character(round(HR, 2))))
abline(h=1, lty=2)
abline(v=1, lty=2)
## Figure 3
myobs1 <- 25
myobs0 <- 25
myexp1 <- myexp0 <- 25
HR <- (myobs1/myobs0)/(myexp1/myexp0)
for(i in 1:length(myi))
    result[i,]  <- sensitivity(obs1=myobs1, obs0=myobs0, exp1=myexp1, exp0=myexp0, obs1Add=f1*myi[i], obs0Add=f0*myi[i], w=myw)
par(las=1)
plot(result, type="b", xlab="ratio of standard errors", ylab="ratio of hazard ratios", log="xy", ylim=c(0.7, 10/7))
title(paste("Primary hazard ratio =", as.character(round(HR, 2))))
abline(h=1, lty=2)
abline(v=1, lty=2)
## Figure 4
myobs1 <- 30
myobs0 <- 20
myexp1 <- myexp0 <- 25
HR <- (myobs1/myobs0)/(myexp1/myexp0)
for(i in 1:length(myi))
    result[i,]  <- sensitivity(obs1=myobs1, obs0=myobs0, exp1=myexp1, exp0=myexp0, obs1Add=f1*myi[i], obs0Add=f0*myi[i], w=myw)
par(las=1)
plot(result, type="b", xlab="ratio of standard errors", ylab="ratio of hazard ratios", log="xy", ylim=c(0.7, 10/7))
title(paste("Primary hazard ratio =", as.character(round(HR, 2))))
abline(h=1, lty=2)
abline(v=1, lty=2)
