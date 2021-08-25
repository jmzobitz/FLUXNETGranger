# For each of the sites in the network compute the CCM, the embedding dimension.

library(tidyverse)
library(FLUXNETGranger)

flux_data <- la_thuille_small %>%
  relocate(time) %>%
  na.omit() %>%
  group_by(site) %>%
  nest()

max_lag <- 14

# Filter through and make sure we have at least 14 entries, omitting the NAs

flux_data_filter <- flux_data %>%
  mutate(enough = map(.x=data,.f=~(value <- .x %>% na.omit %>% NROW() ) )) %>%
  filter(enough > max_lag) %>%
  select(-enough) %>%
  mutate(data = map(.x=data,.f=~na.omit(.x)))

out_data <- vector(mode="list",length=dim(flux_data_filter)[1])

for (i in 1:dim(flux_data_filter)[1]) {
  print(i)
  out_data[[i]] <- embedding_dimension_site(flux_data_filter$data[[i]])
}

# Bind the rows and save to a data frame
flux_e_optimal <- flux_data_filter %>%
  cbind(bind_rows(out_data))

save(flux_e_optimal,file='process-results/flux-embedding-dimension-results.Rda')
