library(tidyverse)
library(haven)


read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/meganellertson/Replication-2-/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

cps_mixtape <- read_dta("Data/cps_mixtape.dta")
nsw_mixtape <- read_dta("Data/nsw_mixtape.dta")
dataset <- read_dta("Data/combineddata.dta")

nsw_dw_cpscontrol <- dataset %>% 
  mutate(agesq = age^2,
         agecube = age^3,
         educsq = educ*educ,
         u74 = case_when(re74 == 0 ~ 1, TRUE ~ 0),
         u75 = case_when(re75 == 0 ~ 1, TRUE ~ 0),
         interaction1 = educ*re74,
         re74sq = re74^2,
         re75sq = re75^2,
         interaction2 = u74*hisp)
#Creating logit data 
nsw_dw_cpscontrol_logit <- nsw_dw_cpscontrol %>%
  mutate(educcube = educ^3,
         re74cube=re74^3, 
         re75cube=re75^3)
#Creating ols data
nsw_dw_cpscontrol_ols <- nsw_dw_cpscontrol %>%
  mutate(re74quad = re74^4,
         re75quad = re74^4)

# basic logit model
logit_nsw_basic <- glm(treat ~ age + agesq + agecube + educ + educsq + 
                   marr + nodegree + black + hisp + re74 + re75 + u74 +
                   u75 + interaction1, family = binomial(link = "logit"), 
                 data = nsw_dw_cpscontrol_logit)
# advanced logit model 
logit <- glm(treat ~ age + agesq + agecube + educ + educsq
                     + educcube + marr + nodegree
                     + black + hisp + re74 + re74sq + re74cube 
                     + re75 + re75sq + re75cube + u74 + u75 +
                       interaction1, family = binomial(link = "logit"),
                     data = nsw_dw_cpscontrol_logit)
#basic ols model 
ols_nsw_basic <- lm(treat ~ age + agesq + agecube + educ + educsq + 
                         marr + nodegree + black + hisp + re74 + re75 + u74 +
                         u75 + interaction1, 
                       data = nsw_dw_cpscontrol_ols)
# advanced ols model
ols <- lm(treat ~ age + agesq + agecube + educ + marr + 
                    nodegree+ black + hisp + re74 + re74sq
                     + re75 + re75sq + u74 + u75 + interaction1,
                  data = nsw_dw_cpscontrol_ols)

#Creating fitted values and pscores 
nsw_dw_cpscontrol_logit <- nsw_dw_cpscontrol_logit %>% 
  mutate(pscore = logit$fitted.values)


nsw_dw_cpscontrol_ols <- nsw_dw_cpscontrol_ols %>% 
  mutate(pscore = ols$fitted.values)

# mean pscores 

pscore_control_logit <- nsw_dw_cpscontrol_logit %>%
  filter(treat==0) %>%
  pull(pscore) %>%
  mean()
pscore_control_logit <- nsw_dw_cpscontrol_logit %>%
  filter(treat==1) %>%
  pull(pscore) %>%
  mean()

pscore_control_ols <- nsw_dw_cpscontrol_ols %>%
  filter(treat==0) %>%
  pull(pscore) %>%
  mean()
pscore_control_ols <- nsw_dw_cpscontrol_ols %>%
  filter(treat==1) %>%
  pull(pscore) %>%
  mean()

#Histograms 
nsw_dw_cpscontrol_logit %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

nsw_dw_cpscontrol_logit %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

nsw_dw_cpscontrol_ols %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

nsw_dw_cpscontrol_ols %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

## Dropping the ends 
nsw_dw_cpscontrol_logit <- nsw_dw_cpscontrol_logit %>% 
  filter(!(pscore >= 0.9)) %>% 
  filter(!(pscore <= 0.1))

Nl <- nrow(nsw_dw_cpscontrol_logit)


nsw_dw_cpscontrol_ols <- nsw_dw_cpscontrol_ols %>% 
  filter(!(pscore >= 0.9)) %>% 
  filter(!(pscore <= 0.1))

No <- nrow(nsw_dw_cpscontrol_ols)

## Redo means
pscore_control_logit <- nsw_dw_cpscontrol_logit %>%
  filter(treat==0) %>%
  pull(pscore) %>%
  mean()
pscore_control_logit <- nsw_dw_cpscontrol_logit %>%
  filter(treat==1) %>%
  pull(pscore) %>%
  mean()

pscore_control_ols <- nsw_dw_cpscontrol_ols %>%
  filter(treat==0) %>%
  pull(pscore) %>%
  mean()
pscore_control_ols <- nsw_dw_cpscontrol_ols %>%
  filter(treat==1) %>%
  pull(pscore) %>%
  mean()
## Redo Histograms

nsw_dw_cpscontrol_logit %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

nsw_dw_cpscontrol_logit %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

nsw_dw_cpscontrol_ols %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

nsw_dw_cpscontrol_ols %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

##ATE Logit 
nsw_dw_cpscontrol_logit %>% 
  filter(treat == 1) %>% 
  summary(re78)

mean1log <- nsw_dw_cpscontrol_logit %>% 
  filter(treat == 1) %>% 
  pull(re78) %>% 
  mean()

nsw_dw_cpscontrol_logit$y1 <- mean1log

nsw_dw_cpscontrol_logit %>% 
  filter(treat == 0) %>% 
  summary(re78)

mean0log <- nsw_dw_cpscontrol_logit %>% 
  filter(treat == 0) %>% 
  pull(re78) %>% 
  mean()

nsw_dw_cpscontrol_logit$y0 <- mean0log

atelog <- unique(nsw_dw_cpscontrol_logit$y1 - nsw_dw_cpscontrol_logit$y0)

## ATE OLS 
nsw_dw_cpscontrol_ols %>% 
  filter(treat == 1) %>% 
  summary(re78)

mean1ols <- nsw_dw_cpscontrol_ols %>% 
  filter(treat == 1) %>% 
  pull(re78) %>% 
  mean()

nsw_dw_cpscontrol_ols$y1 <- mean1ols

nsw_dw_cpscontrol_ols %>% 
  filter(treat == 0) %>% 
  summary(re78)

mean0ols <- nsw_dw_cpscontrol_ols %>% 
  filter(treat == 0) %>% 
  pull(re78) %>% 
  mean()

nsw_dw_cpscontrol_ols$y0 <- mean00ls

ateols <- unique(nsw_dw_cpscontrol_ols$y1 - nsw_dw_cpscontrol_ols$y0)
##Differencing and weighting LOGIT
#- Manual with non-normalized weights using trimmed data
nsw_dw_cpscontrol_logit <- nsw_dw_cpscontrol_logit %>% 
  mutate(d1 = treat/pscore,
         d0 = (1-treat)/(1-pscore))

s1 <- sum(nsw_dw_cpscontrol_logit$d1)
s0 <- sum(nsw_dw_cpscontrol_logit$d0)

nsw_dw_cpscontrol_logit <- nsw_dw_cpscontrol_logit %>% 
  mutate(y1 = treat * re78/pscore,
         y0 = (1-treat) * re78/(1-pscore),
         ht = y1 - y0)

#- Manual with normalized weights with trimmed data
nsw_dw_cpscontrol_logit <- nsw_dw_cpscontrol_logit %>% 
  mutate(y1 = (treat*re78/pscore)/(s1/Nl),
         y0 = ((1-treat)*re78/(1-pscore))/(s0/Nl),
         norm = y1 - y0)

nsw_dw_cpscontrol_logit %>% 
  pull(ht) %>% 
  mean()

nsw_dw_cpscontrol_logit %>% 
  pull(norm) %>% 
  mean()

##Differencing and weighting OLS 

#- Manual with non-normalized weights using trimmed data
nsw_dw_cpscontrol_ols <- nsw_dw_cpscontrol_ols %>% 
  mutate(d1 = treat/pscore,
         d0 = (1-treat)/(1-pscore))

s1 <- sum(nsw_dw_cpscontrol_ols$d1)
s0 <- sum(nsw_dw_cpscontrol_ols$d0)

nsw_dw_cpscontrol_ols <- nsw_dw_cpscontrol_ols %>% 
  mutate(y1 = treat * re78/pscore,
         y0 = (1-treat) * re78/(1-pscore),
         ht = y1 - y0)

#- Manual with normalized weights with trimmed data
nsw_dw_cpscontrol_ols <- nsw_dw_cpscontrol_ols %>% 
  mutate(y1 = (treat*re78/pscore)/(s1/No),
         y0 = ((1-treat)*re78/(1-pscore))/(s0/No),
         norm = y1 - y0)

nsw_dw_cpscontrol_ols %>% 
  pull(ht) %>% 
  mean()

nsw_dw_cpscontrol_ols %>% 
  pull(norm) %>% 
  mean()


```{stata}
use https://github.com/scunning1975/mixtape/raw/master/nsw_mixtape.dta, clear
drop if treat==0
append using https://github.com/scunning1975/mixtape/raw/master/cps_mixtape.dta
```
