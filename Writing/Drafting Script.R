library(tidyverse)
library(haven)


read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

cps_mixtape <- read_dta("Data/cps_mixtape.dta")
nsw_mixtape <- read_dta("Data/nsw_mixtape.dta")

nsw_dw_cpscontrol <- read_data("cps_mixtape.dta") %>% 
  bind_rows(nsw_dw) %>% 
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
  mutate(educcube = educ^3,
         educquad = educ^4,
         agequad = age^4,
         re74cube = re74^3,
         re74quad = re74^4, 
         re75cube = re75^3, 
         re75quad = re75^4)
# estimating
logit_nsw <- glm(treat ~ age + agesq + agecube + educ + educsq + 
                   marr + nodegree + black + hisp + re74 + re75 + u74 +
                   u75 + interaction1, family = binomial(link = "logit"), 
                 data = nsw_dw_cpscontrol)


