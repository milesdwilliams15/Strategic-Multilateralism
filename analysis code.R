library(dplyr)
library(ggplot2)
library(dotwhisker)
library(sandwich)
library(lmtest)
library(dotwhisker)


# Get data
dat = na.omit(read.csv('data.csv'))
dat[,-c(1,2)] = apply(dat[,-c(1,2)],2,function(x) (x>1)*x)
dat[dat == 0] = NA
berm = haven::read_dta('Bermeo_IOTargetedDevelopment_repdata1.dta')
dat = merge(dat,berm %>% 
        mutate(Recipient = recipient,
               Year = year) %>%
        group_by(Recipient,Year) %>%
        summarize(gdp_cap = mean(gdp_cap,na.rm=T)),
      group.by = c(Recipient,Year),
      all.x = T)
dat = na.omit(dat)


stratAM.fun = function(X,y_j,y_i,R_i,b){
  beta = exp(X%*%b[1:ncol(X)])
  gamma = (b[ncol(X)+1])
  delta1 = beta/(beta + 1)
  delta2 = gamma/(beta + 1)
  y_hat = delta1*R_i - delta2*y_j
  rss   = sum((y_hat-y_i)^2)
  return(rss)
}
stratAMpred = function(X,y_j,R_i,b,CI=FALSE,alpha=0.95){
  beta = exp(X%*%b[1:ncol(X)])
  gamma = (b[ncol(X)+1])
  delta1 = beta/(beta + 1)
  delta2 = gamma/(beta + 1)
  y_hat = delta1*R_i - delta2*y_j
  if(CI==FALSE){
    return(y_hat)
  } else {
    se.fit = sqrt(rowSums((cbind(X,y_j)%*%mod.strat$vcov) * 
                            cbind(X,y_j)))
    Qt = c(-1,1)*qt((1 - alpha)/2,
                    (nrow(X)+1)-(ncol(X)+1),
                    lower.tail=F)
    out = data.frame(prediction=y_hat,
                     lo = y_hat + Qt[1]*se.fit,
                     hi = y_hat + Qt[2]*se.fit)
    return(out)
  }
}
stratAM = function(X,y_j,y_i,R_i){
  out = optim(stratAM.fun,
              par=c(rep(0,len=1+ncol(X))),
              hessian = TRUE, method = "BFGS", 
              control = list(REPORT = 10, 
                             trace = 1, 
                             maxit = 50000),
              X=X,y_j=y_j,y_i=y_i,R_i=R_i)
  vcov = try(as.matrix(solve(out$hessian, 
                             tol=1e-24)), T)
  sum = data.frame(term = c(colnames(X),"gamma"),
                   estimate=out$par,
                   std.error = sqrt(diag(vcov)),
                   statistic = out$par/
                     sqrt(diag(vcov)),
                   p.value = round(2*pnorm(abs(out$par/
                                                 sqrt(diag(vcov))),
                                           lower.tail=FALSE),4))
  fit = stratAMpred(X = X, y_j = y_j,R_i=R_i,
                    b=out$par)
  return(list(pars=out$par,
              vcov=vcov,
              rss=out$value,
              fit=fit,
              sum=sum))
}
stratME = function(model,y_j,R_i,X){
  pars    = model$pars
  mod_se  = model$sum$std.error
  y_j     = median(y_i)
  R_i     = median(R_i)
  mar_effs = matrix(NA,ncol=3,nrow=ncol(X))
  rownames(mar_effs) = colnames(X)
  for(j in colnames(X)){
    X_meds = apply(X,2,function(x) rep(median(x),len=2))
    colnames(X_meds) = colnames(X)
    X_meds[,j] = seq(min(X[,j]),max(X[,j]),len=2)
    delta1 = exp(X_meds%*%pars[1:ncol(X)])/
      (exp(X_meds%*%pars[1:ncol(X)])+1)
    delta2 = mod.strat$pars[ncol(X)+1]/
      (exp(X_meds%*%pars[1:ncol(X)])+1)
    y_fit_rni = delta1*R_i - delta2*y_j
    y_nul_rni = delta1*R_i
    y_dif_rni = mean(diff(y_nul_rni - y_fit_rni))
    y_dif_rni.b = c()
    # 95% CIs
      err = sqrt(rowSums((cbind(X_meds,y_j)%*%mod.strat$vcov) * 
                                        cbind(X_meds,y_j)))
      Qt = c(-1,1)*qt((1 - 0.95)/2,
                      (nrow(X)+1)-(ncol(X)+1),
                      lower.tail=F)
      y_fit_rni.lo = delta1*R_i - delta2*y_j + Qt[1]*err
      y_nul_rni.lo = delta1*R_i + Qt[1]*err
      y_fit_rni.hi = delta1*R_i - delta2*y_j + Qt[2]*err
      y_nul_rni.hi = delta1*R_i + Qt[2]*err
    mar_effs[j,] = c(y_dif_rni,y_dif_rni.lo,y_dif_rni.hi)
  }
  term = colnames(X)
  estimate = mar_effs[,1]
  lo.ci = mar_effs[,2]
  hi.ci = mar_effs[,3]
  if(pars[ncol(X)+1]>0){
    out = list(Outcome="Effect on Free-Riding",
               Output=data.frame(term,estimate,lo.ci,hi.ci))
  } else {
    out = list(Outcome="Effect on Rivalry",
               Output=data.frame(term,estimate=-estimate,
                                 lo.ci=-lo.ci,hi.ci=-hi.ci))
  }
  rownames(out$Output) = NULL
  return(out)
}

me_on_gapFilling = function(model,X){
  pars    = model$pars
  mod_se  = model$sum$std.error
  mar_effs = list()
  for(j in colnames(X)){
    X_means = matrix(rep(rbind(apply(X,2,mean)),
                         len=100*ncol(X)),
                     ncol=ncol(X),byrow = T)
    colnames(X_means) = colnames(X)
    X_means[,j] = seq(min(X[,j]),max(X[,j]),len=100)
    delta = mod.strat$pars[ncol(X)+1]/
      (exp(X_means%*%pars[1:ncol(X)])+1)
    delta.b = matrix(0,ncol=10000,nrow=100)
    for(i in 1:10000){
      err <- c()
      for(k in 1:(ncol(X)+1)){
        err[k] = rnorm(n=1,sd=mod_se[k])
      }
      delta.b[,i] = (pars[ncol(X)+1]+err[ncol(X)+1])/
        (exp(X_means%*%(pars[1:ncol(X)]+err[1:ncol(X)]))+1)
    }
    mar_effs[[j]] = data.frame(delta = delta,
                               se = apply(delta.b,1,sd),
                               var = X_means[,j],
                               term = j)
  }
  out = do.call(rbind,mar_effs)
  rownames(out) = NULL
  return(out)
}


X = with(dat,cbind(log(G7),log(DAC - G7 + 3.6),
                       log(nonDAC),
                   log(gdp_cap)))
X = cbind(apply(X,2,function(x)(x - min(x))/(max(x - min(x)))))
colnames(X) = c("G7","DAC - G7","non-DAC","Development")

y_j = log(dat$DAC + dat$nonDAC)
y_i = log(dat$WB)
R_i = log(dat$WB_budget)
mod.strat = stratAM(X=X,y_j=y_j,y_i=y_i,R_i=R_i)
mod.strat

library(tidyr)
# Visualize Data Summaries
dat %>% group_by(Year) %>%
  summarize(G7 = sum(G7),
            non_G7 = sum(DAC) - sum(G7),
            non_DAC = sum(nonDAC),
            WB = sum(WB)) %>%
  ggplot(aes(Year, WB)) +
  geom_line() +
  geom_line(aes(Year, G7),
            linetype=2) +
  geom_line(aes(Year, non_G7),
            linetype=3) +
  geom_line(aes(Year, non_DAC),
            linetype=4) +
  scale_y_log10() +
  geom_text(aes(2007.75,WB[1]-200,label="World\nBank"),
            family="serif",size=2) +
  geom_text(aes(2007.75,G7[1],label="G7"),
            family="serif",size=2) +
  geom_text(aes(2007.75,non_G7[1]+900,label="non-G7\nDAC"),
            family="serif",size=2) +
  geom_text(aes(2007.85,non_DAC[1]+100,label="non-DAC"),
            family="serif",size=2) +
  labs(x="",
       y="Aid Disbursments (in millions)\non a log-10 scale") +
  theme_bw() +
  theme(text=element_text(family="serif"),
        panel.grid = element_blank(),
        axis.text = element_text(color="black")) + 
  ggsave('aidyear.pdf',
         units="in",
         height=3,
         width=5)

stargazer::stargazer(dat,summary = T)

# Visualize Estimates
mod.strat$sum %>%
  relabel_predictors(c(gamma="Rate of Spillin")) %>%
  dwplot(dot_args = list(size=.3,color='black')) +
  geom_vline(xintercept=0) +
  labs(x="Estimated Coefficient\n(with 95% CIs)",
       title="Model Estimates for World Bank Aid Disbursements",
       subtitle="N: 180; Coverage: 2008-2012") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(face='italic',size=11,hjust=.5),
        axis.text = element_text(color="black"),
        text = element_text(family='serif'),
        legend.position = "none") +
  ggsave('coefplot.pdf',
         units="in",
         height=3,
         width = 5)

mes = me_on_gapFilling(mod.strat,X)
mes %>% 
  ggplot(aes(var,delta)) + geom_line(size=1) + 
  geom_ribbon(aes(ymin=delta - 1.96*se,
                  ymax=delta + 1.96*se),
              alpha=.5) +
  geom_hline(yintercept = 0,linetype=2) +
  facet_wrap(~term) +
  labs(x="Covariate Values",
       y="Marginal Effect on Gap-Filling") +
  theme_bw() +
  theme(text=element_text(family="serif")) +
  ggsave('meplot.pdf',
         units="in",
         height=3,
         width = 5)

###############################################




