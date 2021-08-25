# For each of the sites in the network compute the CCM from the embedding dimension.  Then also compute the autocorrelation and determine if the variables are associated

library(tidyverse)
library(FLUXNETGranger)

load('process-results/flux-embedding-dimension-results.Rda')

# Nest data
nested_flux <- flux_e_optimal %>%
  group_by(site) %>%
  nest()

# Compute the cross correlation mapping, do some cleanup
flux_ccm_pre <- nested_flux %>%
  mutate(ccm = map(.x=data,.f=~ccm_site(.x))) %>%
  select(-data) %>%
  mutate(ccm = map(.x=ccm,.f=~separate(data = .x,col=name,into=c("x","y"),sep=":")))

# Compute the auto correlation
max_lag <- 14  # 14 day lag


# Now compute the granger across all possible sites
flux_auto_corr <- flux_e_optimal %>%
  mutate(auto_corr = map(.x=data,.f=~auto_correlation_site(.x,max_lag))) %>%
  select(site,auto_corr)

# Join these up and nest
flux_ccm <- unnest(flux_ccm_pre,cols=c("ccm")) %>%
  inner_join(unnest(flux_auto_corr,cols=c("auto_corr")),by=c("site","x","y")) %>%
  mutate(ccm_greater_05 = value > 0.5,
         ccm_greater_corr = value > correlation,
         ccm_test = ccm_greater_05 & ccm_greater_corr) %>%
  group_by(site) %>%
  nest()



# Save the results
save(flux_ccm,file='process-results/flux-ccm-results.Rda')
