
# Maybe build a visualization function to make things easier?
# Labels correct as well.
# Type of site
# Want to separate by environmental variables + fluxes
# Need to get a uniform scale

# Summarize the granger plot

load('process-results/flux-granger-results.Rda')
n_sites_g <- length(unique(flux_granger$site))
granger_out <- flux_granger %>%
  select(-data) %>%
  unnest(cols=c(granger)) %>%
  select(site,x,y,null_reject) %>%
  group_by(x,y) %>%
  summarize(prop=sum(null_reject)/n_sites_g)

### Now do TE
load('process-results/flux-transfer-entropy-results.Rda')

n_sites_te <- length(unique(flux_te$site))
te_out <- flux_te %>%
  unnest(cols=c(te)) %>%
  select(site,x,y,null_reject) %>%
  #filter(null_reject) %>%
  group_by(x,y) %>%
  summarize(prop=sum(null_reject)/n_sites_te)

### Now do TE
load('process-results/flux-transfer-entropy-results-no-lag.Rda')

n_sites_te <- length(unique(flux_te_no_lag$site))
te_out_no_lag <- flux_te_no_lag %>%
  unnest(cols=c(te)) %>%
  select(site,x,y,null_reject) %>%
  #filter(null_reject) %>%
  group_by(x,y) %>%
  summarize(prop=sum(null_reject)/n_sites_te)



### Now do CCM
load('process-results/flux-ccm-results.Rda')
n_sites_ccm <- length(unique(flux_ccm$site))
ccm_out <- flux_ccm %>%
  unnest(cols=c(data)) %>%
  select(site,x,y,ccm_test) %>%
  #filter(ccm_test) %>%
  group_by(x,y) %>%
  summarize(prop=sum(ccm_test)/n_sites_ccm)


### Define a quick function that plots the correlation matrix for cause variables, and effect variables

prop_matrix_plot <- function(data,cause_vars,effect_vars,title) {

  data %>%
    filter(x %in% cause_vars,
           y %in% effect_vars) %>%
    mutate(x = factor(x,levels=c("GPP_NT_VUT_REF","NEE_VUT_REF","RECO_NT_VUT_REF","P_F","PPFD_IN","TA_F"),
                      labels=c("GPP","NEE","TER","Precip","PPFD","Air T")),
           y = factor(y,levels=c("GPP_NT_VUT_REF","NEE_VUT_REF","RECO_NT_VUT_REF","P_F","PPFD_IN","TA_F"),
                      labels=c("GPP","NEE","TER","Precip","PPFD","Air T"))
    ) %>%
    ggplot(aes(x=x, y=y, fill = prop))+
    geom_tile(color = "white")+
    scale_fill_viridis_b(limits = c(0, 1), oob = scales::squish,
                                name = "Proportion of sites") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = -45, vjust = 1,
                                     size = 12, hjust = 1),
          axis.text.y = element_text(size = 12)) +
    coord_fixed() +
    ggtitle(title) +
    xlab("Cause Variable") +
    ylab("Effect Variable") +
    scale_y_discrete(limits=rev) +
    scale_x_discrete(position = "top") +
    theme(legend.position = "bottom")



}

# Plot these up!
envir_vars <- c("P_F","PPFD_IN","TA_F")
flux_vars <- c("GPP_NT_VUT_REF","NEE_VUT_REF","RECO_NT_VUT_REF")

# First do the causation plot, then do the full matrix
prop_matrix_plot(granger_out,envir_vars,flux_vars,"Granger Causality")
prop_matrix_plot(granger_out,c(flux_vars,envir_vars),c(flux_vars,envir_vars),"Granger Causality")

prop_matrix_plot(te_out,envir_vars,flux_vars,"Transfer Entropy")
prop_matrix_plot(te_out,c(flux_vars,envir_vars),c(flux_vars,envir_vars),"Transfer Entropy")

prop_matrix_plot(te_out_no_lag,envir_vars,flux_vars,"Transfer Entropy")
prop_matrix_plot(te_out_no_lag,c(flux_vars,envir_vars),c(flux_vars,envir_vars),"Transfer Entropy")


prop_matrix_plot(ccm_out,envir_vars,flux_vars,"Convergent Cross Mapping")
prop_matrix_plot(ccm_out,c(flux_vars,envir_vars),c(flux_vars,envir_vars),"Convergent Cross Mapping")

### Also compare how well they do at the same site

granger_small <- flux_granger %>%
  filter(site == "US-NR1") %>%
  select(-data) %>%
  mutate(method = "GC") %>%
  unnest(cols=c(granger)) %>%
  ungroup() %>%
  select(x,y,null_reject,method)

te_small <- flux_te %>%
  filter(site == "US-NR1") %>%
  mutate(method = "TE") %>%
  unnest(cols=c(te)) %>%
  ungroup() %>%
  select(x,y,null_reject,method)

te_small_no_lag <- flux_te_no_lag %>%
  filter(site == "US-NR1") %>%
  mutate(method = "TE") %>%
  unnest(cols=c(te)) %>%
  ungroup() %>%
  select(x,y,null_reject,method)



ccm_small <- flux_ccm %>%
  filter(site == "US-NR1") %>%
  mutate(method = "CCM") %>%
  unnest(cols=c(data)) %>%
  ungroup() %>%
  select(x,y,ccm_test,method) %>%
  rename(null_reject = ccm_test)

### Now roll these all up:

methods_compare <- rbind(granger_small,te_small_no_lag,ccm_small)





site_compare_plot <- function(data,cause_vars,effect_vars,title) {

  data %>%
    filter(x %in% cause_vars,
           y %in% effect_vars) %>%
    mutate(x = factor(x,levels=c("GPP_NT_VUT_REF","NEE_VUT_REF","RECO_NT_VUT_REF","P_F","PPFD_IN","TA_F"),
                      labels=c("GPP","NEE","TER","Precip","PPFD","Air T")),
           y = factor(y,levels=c("GPP_NT_VUT_REF","NEE_VUT_REF","RECO_NT_VUT_REF","P_F","PPFD_IN","TA_F"),
                      labels=c("GPP","NEE","TER","Precip","PPFD","Air T")),
           method = factor(method,levels=c("GC","TE","CCM"),labels=c("Granger \n Causality","Transfer \n Entropy)","Convergent Cross \n Mapping"))
    ) %>%
    filter(null_reject) %>%
    ggplot(aes(x=x,y=y,color=method,size=method)) +
    geom_point(shape=1,stroke=1) +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = -45, vjust = 1,
                                     size = 12, hjust = 1),
          axis.text.y = element_text(size = 12)) +
    coord_fixed() +
    ggtitle(title) +
    xlab("Cause Variable") +
    ylab("Effect Variable") +
    scale_y_discrete(limits=rev) +
    scale_x_discrete(position = "top") +
    theme(legend.position = "bottom") +
    scale_color_discrete(name = "Method") +
    scale_size_discrete(name = "Method")



}

site_compare_plot(methods_compare,envir_vars,flux_vars,"US-NR1")

site_compare_plot(methods_compare,c(flux_vars,envir_vars),c(flux_vars,envir_vars),"US-NR1")

