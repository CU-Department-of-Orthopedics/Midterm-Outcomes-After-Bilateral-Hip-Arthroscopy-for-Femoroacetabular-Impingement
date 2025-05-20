### Summary Stats 

library(table1)

source("Data Clean.R")

dat_sum <- dat_full 

## Summary Tables 

# label(dat_sum$iHOT12_TP1) <-"iHOT-12 Preop"
# label(dat_sum$iHOT12_TP2) <-"iHOT-12 6WPO"
# label(dat_sum$iHOT12_TP3) <-"iHOT-12 3MPO"
# label(dat_sum$iHOT12_TP4) <-"iHOT-12 6MPO"
# label(dat_sum$iHOT12_TP5) <-"iHOT-12 12MPO"
# # label(dat_sum$iHOT12_TP6) <-"iHOT-12 18MPO"
# label(dat_sum$iHOT12_TP7) <-"iHOT-12 24MPO"
# label(dat_sum$iHOT12_TP8) <-"iHOT-12 5YPO"
# 
# label(dat_sum$Hip_Score_TP1) <- "NAHS Preop"
# label(dat_sum$Hip_Score_TP2) <- "NAHS 6WPO "
# label(dat_sum$Hip_Score_TP3) <- "NAHS 3MPO "
# label(dat_sum$Hip_Score_TP4) <- "NAHS 6MPO "
# label(dat_sum$Hip_Score_TP5) <- "NAHS 12MPO"
# # label(dat_sum$Hip_Score_TP6) <- "NAHS 18MPO"
# label(dat_sum$Hip_Score_TP7) <- "NAHS 24MPO"
# label(dat_sum$Hip_Score_TP8) <- "NAHS 5YPO"

render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits = 3, round.integers = F, digits.pct = 2),
       c("", "Mean (SD)" = sprintf("%s (&plusmn;%s)", MEAN, SD)))
}

render.cat <- function(x) {
  c("",
    sapply(stats.default(x),
           function(y) with(y, sprintf("%d (%0.0f%%)", FREQ, PCT))))
}


pvalue <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times = sapply(x, length)))
  if (is.numeric(y)) {
    p <- t.test(y ~ g,na.action = na.omit)$p.value
  } else {
    p <- chisq.test(table(y, g), simulate.p.value = T)$p.value
  }
  c("", sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)))
}


# iHOT_tab <- table1(
#   ~ iHOT12_TP1 + iHOT12_TP2
#   + iHOT12_TP3 + iHOT12_TP4
#   + iHOT12_TP5 
#   + iHOT12_TP7 + iHOT12_TP8| Group,
#   data = dat_sum,
#   overall = F,
#   extra.col=list(`P-value`= pvalue),
#   render.continuous = render.cont,
#   render.categorical = render.cat
# )
# 
# NAHS_tab <- table1(
#   ~ Hip_Score_TP1 + Hip_Score_TP2
#   + Hip_Score_TP3 + Hip_Score_TP4
#   + Hip_Score_TP5 +
#   + Hip_Score_TP7 + Hip_Score_TP8| Group,
#   data = dat_sum,
#   overall = F,
#   extra.col=list(`P-value`= pvalue),
#   render.continuous = render.cont,
#   render.categorical = render.cat
# )
# 


dat_sum_num <- dat_sum %>% 
  select(pid, age, fai_cohort_or_control) %>% 
  group_by(pid, fai_cohort_or_control) %>% 
  summarize(mean_age = mean(age))




dat_sum_cat <- dat_sum %>% 
  select(pid, sex_0_female_1_male, ethnicity, fai_cohort_or_control) %>% 
  distinct(pid, .keep_all = T)

dat_sum <- merge(dat_sum_num, dat_sum_cat, by = "pid")

label(dat_sum$mean_age) <- "Age (yrs)"
label(dat_sum$sex_0_female_1_male) <- "Sex"
label(dat_sum$ethnicity) <- "Ethnicity"

demo_tab <- table1(
  ~ mean_age + sex_0_female_1_male + ethnicity | fai_cohort_or_control.x,
  data = dat_sum,
  overall = F, 
  extra.col=list(`P-value`= pvalue),
  render.continuous = render.cont,
  render.categorical = render.cat,
  footnote = "Note: Age is averaged over all duplicate patients"
)
