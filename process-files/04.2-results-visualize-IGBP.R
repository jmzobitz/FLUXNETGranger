# Plot of the sites we are using in our study - Figure 1

library(tidyverse)
library(FLUXNETGranger)


load('process-results/flux-granger-results.Rda')
joined_granger <- flux_granger %>%
  inner_join(la_thuille_site_info,by=c("site"="SITE")) %>%
  select(-data) %>%
  unnest(cols=c(granger)) %>%
  select(site,IGBP,x,y,null_reject) %>%
  group_by(IGBP,x,y) %>%
  summarize(prop=sum(null_reject))  # Just doing the sum, not prop here (I know, hacky!)


load('process-results/flux-transfer-entropy-results-no-lag.Rda')
# Filter out on these sites
joined_te <-  flux_te_no_lag %>%
  inner_join(la_thuille_site_info,by=c("site"="SITE")) %>%
  unnest(cols=c(te)) %>%
  select(site,IGBP,x,y,null_reject) %>%
  group_by(IGBP,x,y) %>%
  summarize(prop=sum(null_reject))


load('process-results/flux-ccm-results.Rda')

joined_ccm <- flux_ccm %>%
  inner_join(la_thuille_site_info,by=c("site"="SITE")) %>%
  unnest(cols=c(data)) %>%
  select(site,IGBP,x,y,ccm_test) %>%
  #filter(ccm_test) %>%
  group_by(IGBP,x,y) %>%
  summarize(prop=sum(ccm_test))







prop_matrix_plot_wrap <- function(data,cause_vars,effect_vars,title) {

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
    scale_fill_viridis_b(limits = c(0, 20), oob = scales::squish,
                         name = "Number of sites") +
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
    facet_grid(.~IGBP)



}

# Plot these up!
envir_vars <- c("P_F","PPFD_IN","TA_F")
flux_vars <- c("GPP_NT_VUT_REF","NEE_VUT_REF","RECO_NT_VUT_REF")

# First do the causation plot, then do the full matrix

prop_matrix_plot_wrap(joined_granger,envir_vars,flux_vars,"Granger Causality")

prop_matrix_plot_wrap(joined_te,envir_vars,flux_vars,"Transfer Entropy")

prop_matrix_plot_wrap(joined_ccm,envir_vars,flux_vars,"Convergent Cross Mapping")
prop_matrix_plot_wrap(joined_ccm,c(flux_vars,envir_vars),c(flux_vars,envir_vars),"Convergent Cross Mapping")
