# Evaluate the Granger test for all sites in the la_thuille_small

library(tidyverse)
library(FLUXNETGranger)

flux_data <- la_thuille_small %>%
  relocate(time) %>%
  group_by(site) %>%
  nest()

max_lag <- 14  # 14 day lag
alpha <- .05  # significance level

# Filter through and make sure we have at least 14 entries

flux_data_filter <- flux_data %>%
  mutate(enough = map(.x=data,.f=~(value <- .x %>% na.omit %>% NROW() ) )) %>%
  filter(enough > max_lag) %>%
  select(-enough)


# Now compute the granger across all possible sites
flux_granger <- flux_data_filter %>%
  mutate(granger = map(.x=data,
                       .f=~granger_site(.x,alpha,max_lag)
                       )
         )

save(flux_granger,file='process-results/flux-granger-results.Rda')
