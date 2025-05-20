### Updated Analysis/Data 

source("Data Clean.R")

### IHOT-12

## Plot 
dat_ihot_p$TP <- factor(dat_ihot_p$TP, labels = c("Baseline", levels(factor(dat_ihot_p$TP))[2:7]))

p <- ggplot(
  data = dat_ihot_p, 
  aes(
    x = TP,
    y = ihot_score,
    fill = fai_cohort_or_control
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  )  + 
  geom_vline(
    xintercept = 1.5, 
    linetype = "dashed",
    color ="lightgrey"
  ) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = 'bottom'
  ) + 
  labs(
    x = "Months Post-Op",
    y = "iHOT-12 Score",
    fill = ""
  )

p 

ggsave(filename = "ihot_plot.tiff", path = "HiRes Figures", width = 4, height = 3, device='tiff', dpi=600, scaling = .5, units = "in")



# Regression Model iHOT-12

library(nlme)
library(emmeans)
library(kableExtra)

source(".../covariance_structure_test.R")

## ACL 
# cov_var_fn(
#   y = dat_ihot_an$ihot_score,
#   time = dat_ihot_an$TP,
#   pred = dat_ihot_an$fai_cohort_or_control,
#   ID = dat_ihot_an$pid,
#   sort_by = "AIC",
#   VC_only = FALSE
# )


ihot_mod <- gls(ihot_score ~ as.factor(TP) + fai_cohort_or_control, 
                    correlation = corCompSymm(form = ~1|pid), weights = varIdent(form = ~1|TP), 
                    data = dat_ihot_an, na.action = na.omit, method = "ML")

summary(ihot_mod)
plot(ihot_mod)

ihot_mod2 <- gls(ihot_score ~ as.factor(TP)*fai_cohort_or_control, 
                 correlation = corCompSymm(form = ~1|pid), weights = varIdent(form = ~1|TP), 
                 data = dat_ihot_an, na.action = na.omit, method = "ML")

summary(ihot_mod2)

AIC(ihot_mod, ihot_mod2)

mod_ihot_int <- gls(
  ihot_score ~ fai_cohort_or_control*as.factor(TP), 
  correlation = corCompSymm(form = ~1|pid), weights = varIdent(form = ~1|TP), 
  data = dat_ihot_an, na.action = na.omit, method = "REML"
  )    

summary(mod_ihot_int)
plot(mod_ihot_int)

ihot_sum <- data.frame(anova(mod_ihot_int))

names(ihot_sum) <- c("df", "F-value", "p-value")

rownames(ihot_sum) <- c(
  "(Intercept)",
  "Bilateral FAI (1) vs Control (0)",
  "Time Post-Op",
  "Interaction"
)

ihot_sum <- ihot_sum %>% 
  mutate_if(
    is.numeric, round, digits = 3
  ) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) %>% 
  kable(
    
  ) %>% 
  kable_classic(html_font = "cambria", full_width = F)

emm_ihot1 <- emmeans(mod_ihot_int, ~fai_cohort_or_control|TP, mode = "satterthwaite")

emm_ihot1_sum <- dat_ihot_p %>% group_by(fai_cohort_or_control, TP) %>% 
  summarize(mean_score = mean(ihot_score),
            sd = sd(ihot_score)) %>% 
  arrange(TP) %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  `colnames<-`(c("Group", "Months Post-Op", "Mean iHOT-12 Score", "SD")) %>% 
  kable() %>% kable_classic(html_font = "cambria", full_width = F)
  
ihot_cont_table1 <- as.data.frame(summary(contrast(emm_ihot1, method = "pairwise", adjust = "tukey")))

ihot_conf_table1 <- round((confint(contrast(emm_ihot1, method = "pairwise", adjust = "tukey")))[, 6:7], 2)
ihot_conf_table1 <- paste0("(", ihot_conf_table1[, 1], ", ", ihot_conf_table1[, 2], ")")

ihot_cont_table1 <- cbind(ihot_cont_table1, ihot_conf_table1)

names(ihot_cont_table1) <- c("Contrast", "Months Post-Op", "Diff. Mean iHOT-12 Score", "Std. Err.", "df", "t-ratio", "p-value", "95% CI") 

ihot_cont_table1 <- ihot_cont_table1 %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) %>% 
  select(
    -c("df")
  ) %>% 
  kable() %>% 
  kable_classic(html_font = 'cambria', full_width = F)


