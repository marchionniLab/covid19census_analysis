us_scripts <- list(
  "libraries.R",
  "functions_analysis.R",
  "functions_graph.R",
  "us_preprocess.R"
)
#
#
# # Ode to the here https://github.com/jennybc/here_here
library(here)
lapply(us_scripts, function(x) source(here("code",  x)))

mess <- "\nAnalyzing Strata diab!\n"
sep_mess <- paste(rep.int("=", nchar(mess)), collapse = "")

message(paste0(sep_mess, mess, sep_mess))


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Set some general parameters and dataframe to use---------------
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

data_to_use <- dat_diab

dates_tostudy <- sort(unique(data_to_use$date))
dates_tostudy <- dates_tostudy[(length(dates_tostudy) - 210):length(dates_tostudy)]
to_filt_cases <- 1:100
XX <- grep("XX", names(data_to_use), value = TRUE)
min_filt <- 1
variables_used <- "diab"

# Dry? no, definitely WET :(

# @@@@@@@@@@@@@@@@@@@@@@@@
# QUINTILE ---------------
# @@@@@@@@@@@@@@@@@@@@@@@@

# formula for propensity score
form_ps <- reformulate(termlabels = XX, response = "logitZZ_diab")
number_strata_quintiles <- 5 # quintiles

# @@@@@@@@@@@@@@@
# state by date
# @@@@@@@@@@@@@@

form_state <- as.formula(YY_deaths ~ ZZ_diab + PP + state)

strata_states_diab_dates_5 <- map_dfr(dates_tostudy, strata_glm_pp,
                                          dat = data_to_use,
                                          filt_c = 1,
                                          form_ps = form_ps,
                                          form_glm = form_state,
                                          number_strata = number_strata_quintiles
) %>%
  mutate(analysis = "state_dates")




# @@@@@@@@@@@@@@@@@
# Calculate MRR
# @@@@@@@@@@@@@@@@@


strata_diab_results_splitted_5 <- strata_states_diab_dates_5 %>%
  unite("date_filt_analysis", c("date", "filt_cases_c", "analysis")) %>%
 dplyr::select(-model, -data) %>%
  split.data.frame(., .$date_filt_analysis)


MRR_strata_diab_5 <- map_dfr(strata_diab_results_splitted_5, strata_weights_MRR, var_int = "ZZ_diab") %>%
  relocate(c("date_filt_analysis", "term"), .before = estimate) %>%
  separate(date_filt_analysis, into = c("date", "filt", "adjustment", "analysis_group"), sep = "_") %>%
  mutate(filt = as.numeric(filt)) %>%
  mutate(nyc_removed = if_else(analysis_group == "nonyc", TRUE, FALSE)) %>%
  relocate(nyc_removed, .before = term) %>%
  mutate(variables_used = !!variables_used) %>%
  relocate(variables_used, .after = date) %>%
  relocate(type_pp, .before = term)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Save objects of the day-----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

plot_MRR_strata_diab_5_date <- ggplot_MRR_strata(MRR_strata_diab_5, "dates")

ggplot_MRR_strata(MRR_strata_diab_5, "dates") +
  scale_y_continuous(limit = c(0.80, 2.5))

ggsave(plot = plot_MRR_strata_diab_5_date, filename = here("figs", "MRR_strata_diab_5_dates.png"))

plot_MRR_strata_diab_5_cases <- ggplot_MRR_strata(MRR_strata_diab_5, "cases")
ggsave(plot = plot_MRR_strata_diab_5_cases, filename = here("figs",  "MRR_strata_diab_5_cases.png"))

# Save the results
saveRDS(MRR_strata_diab_5, file = here("objs", "MRR_strata_diab_5.RDS"))


