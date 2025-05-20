### Updated Analysis/Data 

source("Data Clean.R")

### Hip Scores

## Plot 
dat_hip_p$TP <- factor(dat_hip_p$TP, labels = c("Baseline", levels(factor(dat_hip_p$TP))[2:7]))

p <- ggplot(
  data = dat_hip_p, 
  aes(
    x = TP,
    y = hip_score,
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
    y = "NAHS Score",
    fill = ""
  )

p 

ggsave(filename = "nahs_plot.tiff", path = "HiRes Figures", width = 4, height = 3, device='tiff', dpi=600, scaling = .5, units = "in")


# Regression Model hip-12

library(nlme)
library(emmeans)
library(kableExtra)

source(".../covariance_structure_test.R")

## ACL 
# cov_var_fn(
#   y = dat_hip_an$hip_score,
#   time = dat_hip_an$TP,
#   pred = dat_hip_an$fai_cohort_or_control,
#   ID = dat_hip_an$pid,
#   sort_by = "AIC",
#   VC_only = FALSE
# )


hip_mod <- gls(hip_score ~ as.factor(TP) + fai_cohort_or_control, 
                correlation = corCompSymm(form = ~1|pid), weights = varIdent(form = ~1|TP), 
                data = dat_hip_an, na.action = na.omit, method = "ML")

summary(hip_mod)
plot(hip_mod)

hip_mod2 <- gls(hip_score ~ as.factor(TP)*fai_cohort_or_control, 
                correlation = corCompSymm(form = ~1|pid), weights = varIdent(form = ~1|TP), 
                data = dat_hip_an, na.action = na.omit, method = "ML")

summary(hip_mod2)

AIC(hip_mod, hip_mod2)

mod_hip_int <- gls(
  hip_score ~ fai_cohort_or_control*as.factor(TP), 
  correlation = corCompSymm(form = ~1|pid), weights = varIdent(form = ~1|TP), 
  data = dat_hip_an, na.action = na.omit, method = "REML"
)    

summary(mod_hip_int)
plot(mod_hip_int)

hip_sum <- data.frame(anova(mod_hip_int))

names(hip_sum) <- c("df", "F-value", "p-value")

rownames(hip_sum) <- c(
  "(Intercept)",
  "Bilateral FAI (1) vs Control (0)",
  "Time Post-Op",
  "Interaction"
)

hip_sum <- hip_sum %>% 
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

emm_hip1 <- emmeans(mod_hip_int, ~fai_cohort_or_control|TP, mode = "satterthwaite")

emm_hip_sum <- dat_hip_p %>% group_by(fai_cohort_or_control, TP) %>% 
  summarize(mean_score = mean(hip_score),
            sd = sd(hip_score)) %>% 
  arrange(TP) %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  `colnames<-`(c("Group", "Months Post-Op", "Mean NAHS Score", "SD")) %>% 
  kable() %>% kable_classic(html_font = "cambria", full_width = F)




hip_cont_table1 <- as.data.frame(summary(contrast(emm_hip1, method = "pairwise", adjust = "tukey")))

hip_conf_table1 <- round((confint(contrast(emm_hip1, method = "pairwise", adjust = "tukey")))[, 6:7], 2)
hip_conf_table1 <- paste0("(", hip_conf_table1[, 1], ", ", hip_conf_table1[, 2], ")")

hip_cont_table1 <- cbind(hip_cont_table1, hip_conf_table1)

names(hip_cont_table1) <- c("Contrast", "Months Post-Op", "Diff. Mean hip-12 Score", "Std. Err.", "df", "t-ratio", "p-value", "95% CI") 

hip_cont_table1 <- hip_cont_table1 %>% 
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

