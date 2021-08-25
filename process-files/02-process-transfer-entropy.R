# Evaluate the Transfer Entropy test for all sites in the la_thuille_small

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

n_sites <- length(flux_data_filter$site)
flux_te <- vector(mode = "list",length = n_sites)
flux_te_no_lag <- vector(mode = "list",length = n_sites)

for (i in seq_along(flux_te) ) {
  print(flux_data_filter$site[[i]])
  flux_te[[i]] <- transfer_entropy_site(flux_data_filter$data[[i]],alpha,max_lag,bootstrap_samples = 5)
}

# Bind these all together
flux_te <- tibble(site=flux_data_filter$site,te = flux_te)


save(flux_te,file='process-results/flux-transfer-entropy-results.Rda')

# Do this again, but with no lags and more bootstrap samples
for (i in seq_along(flux_te_no_lag) ) {
  print(flux_data_filter$site[[i]])
  flux_te_no_lag[[i]] <- transfer_entropy_site(flux_data_filter$data[[i]],alpha,max_lag = 1,bootstrap_samples = 100)
}

flux_te_no_lag <- tibble(site=flux_data_filter$site,te = flux_te_no_lag)

save(flux_te_no_lag,file='process-results/flux-transfer-entropy-results-no-lag.Rda')
