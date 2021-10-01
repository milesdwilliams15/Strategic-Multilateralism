#################
# Main Analysis
#################


# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(SARM)
library(kableExtra)


# data --------------------------------------------------------------------

dt <- 
  read_csv(
    "01_data/final_data.csv"
  )



# some recodes ------------------------------------------------------------

new_dt <- dt %>%
  mutate(
    income = gdp / pop,
    donor_aid = non_dac + dac,
    dac_non_g7 = dac - g7
  ) %>%
  mutate_at(
    c("wb", "non_dac", "dac", "g7",
      "wb_budget", "gdp", "pop", "income",
      "unemp", "disaster", "donor_aid",
      "dac_non_g7"),
    asinh
  ) %>%
  mutate_at(
    c("g7", "dac_non_g7", "non_dac",
      "income", "pop", "unemp", "disaster",
      "civilwar", "gdp"),
    ~ (.x - mean(.x)) / sd(.x)
  )



# analysis ----------------------------------------------------------------

fit <- rob_sarm(
  wb ~ wb_budget + donor_aid +
    g7 + dac_non_g7 + non_dac +
    income + recipient_iso3,
  data = new_dt
)
summary_sarm(fit)

new_summary <- fit$summary %>%
  mutate(
    term = c(
      "Control", "G7 Aid", "DAC (non-G7) Aid",
      "Non-DAC Aid", "Development",
      "$\\gamma$"
    )
  ) %>%
  rename(
    c(" " = term,
      "Coefficient" = estimate,
      "S.E." = std.error,
      "z-stat" = statistic,
      "p-value" = p.value)
  )


reg_tab <- kable(new_summary, "latex", booktabs = T,
      linesep = "", escape = F,
      digits = 3,
      caption = "DARM Estiamtes") %>%
  add_footnote(
    "Inference done with cluster-robust errors.",
    threeparttable = T
  )
save(reg_tab, file = "04_paper/reg_tab.tex")

me_on_gapFilling <- function(fit){
  pars     <- fit$summary$estimate
  mod_se   <- fit$summary$std.error
  mar_effs <- list()
  X <- cbind(control = 1, fit$model_frame[, -c(1:3)])
  for(j in colnames(X)[-1]){
    X_means = matrix(rep(rbind(apply(X,2,mean)),
                         len=100*ncol(X)),
                     ncol=ncol(X),byrow = T)
    colnames(X_means) = colnames(X)
    X_means[,j] = seq(-2,2,len=100)
    delta = - pars[ncol(X)+1]/
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

mes <- me_on_gapFilling(fit)
mes %>% 
  mutate(
    term = rep(
      c("(1) G7", "(2) DAC - G7",
        "(3) Non-DAC", "(4) Development"),
      each = 100
    )
  ) %>%
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
  ggsave('04_paper/meplot.png',
         units="in",
         height=4,
         width = 5)
