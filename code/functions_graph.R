# @@@@@@@@@@@@@@@@@@@@@
# graph functions-----
# @@@@@@@@@@@@@@@@@@@@@


#' ggcovid
#'
#' Plot results of `glm_pp`
#'
#' @param dat a glm_pp object
#' @param x_var date or cases. Date plots the time-serie of the slope of the variable of interest in the model,
#'     cases takes the last day and plot the slope of the variable of interest in the model changing inclusion criterion
#'     for country.
#' @param filt_cases  minimum number of cases to included in the model,
#' @param nation it or us
ggcovid <- function(dat, x_var, filt_cases = 10, nation = "us") {
  if (!x_var %in% c("date", "cases")) stop("x_var can be date or cases!")

  if (x_var == "date") {
    to_add <- paste0("cases >=", filt_cases)
    aes_x <- "date"
    dat <- dat %>%
      filter(filt_cases_c == !!filt_cases)
    x_title <- NULL
  }

  if (x_var == "cases") {
    to_add <- paste0("date ", max(dat$date))
    aes_x <- "filt_cases_c"
    dat <- dat %>%
      filter(date == max(date))
    x_title <- "inclusion criteria (minimum cases)"
  }

  if (nation == "it") {
    var_facet <- c("estimate", "deaths", "cases")
  }


  if (nation == "us") {
    var_facet <- c("estimate", "number of counties")

    dat <- dat %>%
      rename(`number of counties` = n_counties)
  }


  # dependent variable used in title
  dep_var <- stringr::word(first(dat$formula), 1)

  # browser()
  dat %>%
    pivot_longer(
      cols = !!var_facet,
      names_to = "var_name"
    ) %>%
    mutate(var_name = factor(var_name, levels = !!var_facet)) %>%
    mutate(sign = if_else(p.value < 0.05, "p<0.05", "NS")) %>%
    mutate(sign = factor(sign, levels = c("p<0.05", "NS"))) %>%
    mutate(p.value = log10(p.value)) %>%
    # mutate(std.error = if_else(var_name == "estimate", std.error, 0)) %>%

    mutate_at(vars(std.error, conf.low, conf.high), ~ if_else(var_name == "estimate", ., 0)) %>%
    ggplot(aes_string(aes_x, "value", color = "p.value", group = "term")) +
    geom_line() +
    geom_point() +
    geom_linerange(aes(ymax = conf.high, ymin = conf.low), show.legend = FALSE) +
    facet_grid(vars(var_name), scales = "free") +
    scale_color_gradient(low = "blue", high = "red") +
    # scale_color_manual(values = c("NS" = "#00BFC4", "p<0.05" = "#F8766D"), drop = FALSE) +
    theme(
      # legend.position = "top",
      # legend.title = element_blank(),
      legend.box.background = element_rect(colour = "black")
    ) +
    labs(
      title = paste0("Slope of ", first(dat$term), " over time"),
      colour = "log p.value",
      subtitle = paste0(
        dep_var, "; offset = ", first(dat$offset_f),
        "\n", to_add
      ),
      x = x_title
    ) +
    expand_limits(y = 0)
}

#' lord of the ring function
#'
#' Given a COVID-19 dataframe of data, graph a specific variable by state/region in a jitter plot.
#' It will be lapply to the dataframe to get with one function all the graphs needed. One function to graph
#' them all (this is lame I know)
#'
#' @param dat COVID-19 dataframe
#' @param var_n the column to be plotted
#' @param model_dat boolean, is this for the data of the model or not? (model data have less country)
#' @param log_v boolean, do you want a
#' @param path_f where to save data
#'
#' @return generate a ggplot
lord_rings <- function(var_n,
                       dat,
                       model_dat,
                       log_v = FALSE,
                       nation,
                       path_f = getwd()) {
  x <- var_n
  model_dat <- paste0("Model data: ", model_dat)
  tot_NA <- paste0("; Total NA: ", sum(is.na(dat[, var_n])))


  if (log_v == FALSE) {
    log_name_file <- "abs_"
  }

  if (log_v == TRUE) {
    log_name_file <- "log_"
    var_n <- paste0("log10(", var_n, ")")
  }
  if (model_dat == FALSE) {
    model_dat_name_file <- "_model_data.png"
  } else {
    model_dat_name_file <- "_all_data.png"
  }

  name_file <- paste0(path_f, "/", log_name_file, x, model_dat_name_file)
  message("Drawing graph, ", x, " just a moment!")


  # create colors to use and change title dynamically
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  if (nation == "us") {
    n_rep <- ceiling(length(unique(dat$state)) / 8)
    colors_to_use <- rep(cbbPalette, n_rep)[1:length(unique(dat$state))]
    tot_observ <- paste0("; Counties: ", nrow(dat))

    plot_r <- dat %>%
      # mutate(state = factor(state, levels = ordered_levels)) %>%
      ggplot(aes_string(var_n, "state", colour = "state")) +
      geom_jitter(alpha = 0.5, height = 0.3) +
      theme(
        legend.title = element_blank(),
        legend.position = "none"
      ) +
      scale_color_manual(values = colors_to_use) +
      labs(
        title = paste0(toupper(var_n)),
        subtitle = paste0(model_dat, tot_NA, tot_observ),
        y = NULL
      )
  }

  if (nation == "it") {
    tot_observ <- paste0("; Regions: ", nrow(dat))

    plot_r <- dat %>%
      ggplot(aes_string(var_n, "region")) +
      geom_col(fill = "grey30") +
      theme(
        legend.title = element_blank(),
        legend.position = "none"
      ) +
      labs(
        title = paste0(toupper(var_n)),
        subtitle = paste0(model_dat, tot_NA, tot_observ),
        y = NULL
      )
  }


  ggsave(name_file, units = "in", width = 6, height = 7)
}



#' the fifth element
#'
#' Function to plot each variable based on quint of propensity score
#'
#' @param dat COVID-19 dataframe of one date, with var PP with quintiles
#' @param var_n the column to be plotted

#' @param log_v boolean, do you want a
#' @param path_f where to save data
#'
#' @return generate a ggplot
fifth_element <- function(var_n,
                          dat,
                          log_v = FALSE,
                          path_f = getwd()) {
  x <- var_n


  if (log_v == FALSE) {
    log_name_file <- "abs_"
  }

  if (log_v == TRUE) {
    log_name_file <- "log_"
    var_n <- paste0("log10(", var_n, ")")
  }

  name_file <- paste0(path_f, "/", log_name_file, x, ".png")
  message("Drawing graph, ", x, " just a moment!")


  # plot_r <-
  dat %>%
    # mutate(state = factor(state, levels = ordered_levels)) %>%
    ggplot(aes_string(var_n, "PP")) +
    geom_violin() +
    geom_boxplot(width = 0.1) +
    theme(
      legend.title = element_blank(),
      legend.position = "none"
    ) +
    scale_color_manual(values = colors_to_use) +
    labs(
      y = "Quintile"
    )

  ggsave(name_file, units = "in", width = 6, height = 7)
}



glm_by_strata <- function(dat, var_strata) {
  XX <- grep("XX", names(dat), value = TRUE)
  XX <- XX[!XX %in% c("XX_total_beds")]
  XX <- XX[XX != var_strata]

  form_ps <- reformulate(termlabels = XX, response = "logitZZ_perc_imm65")

  PropScores_dat <- lm(form_ps, data = dat)

  glm_formula <- as.formula(paste("YY_deaths ~ ZZ_perc_imm65 + PP_cont +", var_strata))

  dat_split <-
    dat %>%
    mutate(
      PP = as.factor(ntile(fitted.values(PropScores_dat), 5)),
      PP_cont = fitted.values(PropScores_dat)
    )
  var_model <- dat_split %>%
    nest(-PP) %>%
    arrange(PP) %>%
    mutate(
      fit = map(data, ~ glm(
        glm_formula,
        family = quasipoisson(link = "log"),
        offset = log(NP_total_pop), data = .x
      )),
      tidy = map(fit, tidy)
    )

  var_model %>%
    unnest(tidy) %>%
    mutate(formula_model = deparse(glm_formula)) %>%
    dplyr::relocate(formula_model, .before = PP)
}



#' Plot MRT of strata
#'
#' @param dat  dataframe of aggregated data of MRR for strata analysis
#' @param type if to plot dates or cases
#'
#' @return ggplot

ggplot_MRR_strata <- function(dat, type = "dates") {
  if (type == "dates") {
    graph_MRR <- dat %>%
      filter(analysis_group == "dates") %>%
      mutate(date = ymd(date)) %>%
      ggplot(aes(date, MRR)) +
      scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d")

    lab_x <- NULL
  }

  if (type == "cases") {
    graph_MRR <- dat %>%
      filter(analysis_group == "cases") %>%
      # filter(filt <= 10) %>%
      ggplot(aes(filt, MRR))
    # scale_x_continuous(breaks = 1:10)

    lab_x <- "county inclusion criterion (\u2265 cases)"
  }


  graph_MRR +
    geom_linerange(aes(ymin = MRR_conf.low, ymax = MRR_conf.high), show.legend = FALSE) +
    geom_point(size = 3, colour = "black", fill = "black", shape = 21) +
    geom_hline(aes(yintercept = 1), lty = 3) +
    geom_line(lwd = 1) +
    facet_grid(vars(adjustment), labeller = as_labeller(c("state" = "States", "divname" = "Census Regions"))) +
    labs(
      y = "Mortality Risk Ratio (+/- CI)",
      x = lab_x
    )
}


#' love plot
#'
#' Generates love plots for diagnose PS balancing, plotting correlations between exposure and confounders before and
#' after stratification.
#'
#' @param dat list of dataframes returned by `cor_diagnostic`.
#' @param type if `aggregates` than plot absolute mean of cor between strata and `max`, `strata` all single strata
#'
#' @return love plot
love_plot <- function(dat, type) {
  dat_p <- dat$all_cor %>%
    filter(metric != "crude_mean_strata") %>%
    mutate(
      confounder = str_remove(confounder, "XX_"),
      exposure = str_remove(exposure, "ZZ_")
    ) %>%
    mutate(metric = fct_rev(metric))

  n_strata <- max(as.numeric(dat$dat_cor_strata_all$stratum))
  exposure <- unique(dat_p$exposure)

  if (type == "aggregates") {
    p <- dat_p %>%
      ggplot(aes(value, confounder, col = type, group = type)) +
      geom_vline(xintercept = 0.1, lty = 3) +
      geom_line(orientation = "y") +
      geom_point() +
      facet_wrap(vars(metric))
  }

  if (type == "strata") {
    p <-
      dat$dat_cor_strata_all %>%
      mutate(cor = abs(cor)) %>%
      mutate(
        confounder = str_remove(confounder, "XX_"),
        exposure = str_remove(exposure, "ZZ_")
      ) %>%
      ggplot(aes(cor, confounder)) +
      geom_vline(xintercept = 0.1, lty = 3) +
      geom_text(aes(label = stratum))
  }

  p +
    scale_x_continuous(limits = c(0, 1)) +
    labs(
      y = NULL,
      x = paste0("correlation with ", exposure),
      title = paste0("Number of Strata:", n_strata)
    ) +
    theme(plot.title = element_text(hjust = 0.5))
}
