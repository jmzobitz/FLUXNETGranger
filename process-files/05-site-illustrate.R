# Make a ggplot of the timeseries for NEE with night temperature for Niwot

library(FLUXNETGranger)
library(tidyverse)

# Plot the Niwot data:
niwot_data <- la_thuille_small %>%
  relocate(time) %>%
  filter(site == "US-NR1")

niwot_data %>%
  select(time, TA_F, NEE_VUT_REF) %>%
  pivot_longer(cols = c(-time)) %>%
  mutate(name = factor(name,
    levels = c("NEE_VUT_REF", "TA_F"),
    labels = c("NEE", "Air T")
  )) %>%
  ggplot(aes(x = time, y = value, color = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  theme_granger() +
  xlab("Day of Year") +
  ylab("Measurement") +
  theme_granger() +
  ggtitle("US-NR1") +
  guides(color = "none")

niwot_data %>%
  select(time, TA_F, NEE_VUT_REF) %>%
  ggplot(aes(x = TA_F, y = NEE_VUT_REF)) +
  geom_point() +
  xlab("Air Temperature") +
  ylab("NEE") +
  theme_granger() +
  ggtitle("US-NR1")

library(FLUXNETGranger)
library(tidyverse)
# Conduct a Granger Test
# Define the data
niwot_data <- la_thuille_small %>%
  relocate(time) %>%
  filter(site == "US-NR1")

niwot_alpha <- .05 # significance level
niwot_lag <- 14 # Max 14 day lag

# Compute the Granger test - does temperature cause NEE?
granger_test(
  x = niwot_data$TA_F,
  y = niwot_data$NEE_VUT_REF,
  alpha = niwot_alpha,
  max_lag = niwot_lag
)

# Conduct a Transfer Entropy test
library(RTransferEntropy)
transfer_entropy(
  x = niwot_data$TA_F,
  y = niwot_data$NEE_VUT_REF,
  lx = niwot_lag,
  ly = niwot_lag,
  nboot = 100
)

# Conduct a CCM test
library(rEDM)

# Make a smaller dataset to work with
niwot_data_small <- niwot_data %>%
  select(time,TA_F,NEE_VUT_REF)

# Determine the embedding dimension
niwot_library <- embedding_dimension_site(niwot_data_small)

# Now do the CCM
CCM(
  dataFrame = niwot_data_small,
  columns = "TA_F",
  target = "NEE_VUT_REF",
  libSizes = niwot_library$library_size,
  Tp = 0,
  E = niwot_library$E_opt,
  sample = 1
) %>%
  select(-LibSize) %>%
  pivot_longer(cols = c(everything()))
