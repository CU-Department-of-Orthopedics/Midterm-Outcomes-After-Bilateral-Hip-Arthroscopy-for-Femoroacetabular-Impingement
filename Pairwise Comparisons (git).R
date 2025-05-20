#### T-Tests / Wilcoxon Signed Rank Test

source("...")

library(broom)

## iHOT12 T-test 

# 6-weeks post-op
ihot_test_ihot2 <- t.test(dat_full$iHOT12_TP2 ~ dat_full$Group)

ihot_test_ihot2 <- tidy(ihot_test_ihot2)[c(1, 4, 5, 7, 8)]
names(ihot_test_ihot2) <- c("Mean Difference", "t-value", "p-value", "95% CI: LB", "95% CI: UB")

ihot_test_ihot2 <- ihot_test_ihot2 %>% 
  mutate_if(
    is.numeric, round, digits = 3
  ) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  )


# 3-months post-op 
ihot_test_ihot3 <- t.test(dat_full$iHOT12_TP3 ~ dat_full$Group)

ihot_test_ihot3 <- tidy(ihot_test_ihot3)[c(1, 4, 5, 7, 8)]
names(ihot_test_ihot3) <- c("Mean Difference", "t-value", "p-value", "95% CI: LB", "95% CI: UB")

ihot_test_ihot3 <- ihot_test_ihot3 %>% 
  mutate_if(
    is.numeric, round, digits = 3
  ) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  )


## NAHS T-test 

# 6-weeks post-op
test_hip2 <- t.test(dat_full$Hip_Score_TP2 ~ dat_full$Group)

test_hip2 <- tidy(test_hip2)[c(1, 4, 5, 7, 8)]
names(test_hip2) <- c("Mean Difference", "t-value", "p-value", "95% CI: LB", "95% CI: UB")

test_hip2 <- test_hip2 %>% 
  mutate_if(
    is.numeric, round, digits = 3
  ) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  )


# 3-months post-op 
test_hip3 <- t.test(dat_full$Hip_Score_TP3 ~ dat_full$Group)

test_hip3 <- tidy(test_hip3)[c(1, 4, 5, 7, 8)]
names(test_hip3) <- c("Mean Difference", "t-value", "p-value", "95% CI: LB", "95% CI: UB")

test_hip3 <- test_hip3 %>% 
  mutate_if(
    is.numeric, round, digits = 4
  ) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  )
