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

fit <- sarm(
  wb ~ wb_budget + donor_aid +
    g7 + dac_non_g7 + non_dac +
    gdp + pop + 
    recipient_iso3,
  data = new_dt,
  robust = T,
  cluster = T
)
summary_sarm(fit)

new_summary <- fit$summary %>%
  mutate(
    term = c(
      "Control", "G7 Aid", "DAC (non-G7) Aid",
      "Non-DAC Aid", "GDP", "Population", 
      "$\\gamma$", "\\ln(\\sigma)"
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

g7_dt <- fit$model_frame %>%
  mutate(
    wb_budget = mean(wb_budget),
    g7 = seq(-2, 2, len = n()),
    donor_aid = g7 + mean(dac_non_g7) + mean(non_dac),
    dac_non_g7 = mean(dac_non_g7),
    non_dac = mean(non_dac),
    gdp = mean(gdp),
    pop = mean(pop)
  )

preds1 <- predict_sarm(
  model = fit,
  newdata = g7_dt
)

p1 <- preds1 %>%
  mutate(
    g7 = g7_dt$g7
  ) %>%
  ggplot() +
  aes(
    x = g7,
    y = fit,
    ymin = lwr,
    ymax = upr
  ) +
  geom_line(size = 0.75) +
  geom_ribbon(
    alpha = 0.4
  ) +
  labs(
    x = "G7 Aid",
    y = "Predicted WB Allocation\n(IHS with 95% CIs)"
  ) +
  theme_bw()

dac_non_g7_dt <- fit$model_frame %>%
  mutate(
    wb_budget = mean(wb_budget),
    dac_non_g7 = seq(-2, 2, len = n()),
    donor_aid = mean(g7) + dac_non_g7 + mean(non_dac),
    g7 = mean(g7),
    non_dac = mean(non_dac),
    gdp = mean(gdp),
    pop = mean(pop)
  )

preds2 <- predict_sarm(
  model = fit,
  newdata = dac_non_g7_dt
)

p2 <- preds2 %>%
  mutate(
    dac_non_g7 = dac_non_g7_dt$dac_non_g7
  ) %>%
  ggplot() +
  aes(
    x = dac_non_g7,
    y = fit,
    ymin = lwr,
    ymax = upr
  ) +
  geom_line(size = 0.75) +
  geom_ribbon(
    alpha = 0.4
  ) +
  labs(
    x = "DAC Aid (excluding G7)",
    y = "Predicted WB Allocation\n(IHS with 95% CIs)"
  ) +
  theme_bw()

non_dac_dt <- fit$model_frame %>%
  mutate(
    wb_budget = mean(wb_budget),
    non_dac = seq(-2, 2, len = n()),
    donor_aid = mean(g7) + mean(dac_non_g7) + non_dac,
    dac_non_g7 = mean(dac_non_g7),
    g7 = mean(non_dac),
    gdp = mean(gdp),
    pop = mean(pop)
  )

preds3 <- predict_sarm(
  model = fit,
  newdata = non_dac_dt
)

p3 <- preds3 %>%
  mutate(
    non_dac = non_dac_dt$non_dac
  ) %>%
  ggplot() +
  aes(
    x = non_dac,
    y = fit,
    ymin = lwr,
    ymax = upr
  ) +
  geom_line(size = 0.75) +
  geom_ribbon(
    alpha = 0.4
  ) +
  labs(
    x = "Non-DAC Aid",
    y = "Predicted WB Allocation\n(IHS with 95% CIs)"
  ) +
  theme_bw()

gridExtra::grid.arrange(
  p1, p2, p3, ncol = 1
) %>%
  ggsave(
    plot = .,
    filename = "04_paper/pred_plot.png",
    height = 8,
    width = 4
  )

