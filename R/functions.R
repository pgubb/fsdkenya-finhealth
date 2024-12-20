
# Functions to retrieve and prepare data

get_data <- function(year) { 
  
  path <- "data"
  
  if (year == 2024) {
    file <- glue("{path}/FA_{year}_Public.dta")
    data <- read_dta(file) %>% filter(A18 >= 18) %>% select(-starts_with("mfhi"))
  }
  if (year == 2021) {
    file <- glue("{path}/FA_{year}_Public.sav")
    data <- read_sav(file) %>% filter(A19 >= 18) %>% select(-starts_with("mfhi"))
  }
  if (year == 2019) {
    file <- glue("{path}/FA_{year}_Public.sav")
    data <- read_sav(file) %>% filter(a13 >= 18)
  }
  if (year == 2016) {
    file <- glue("{path}/FA_{year}_Int.csv")
    data <- read_csv(file) %>% select(-starts_with("year"))
  }
  
  return(data)
  
}

get_and_prep <- function(year) { 
  
  years <- c("2016" = 2015.6, "2019" = 2018.9, "2021" =  2021.6, "2024" = 2024.9)
  
  data <- get_data(year) %>% 
    mutate(
      year = year, 
      year_c = years[as.character(year)],
      year_f = factor(year, levels = c("2016", "2019", "2021", "2024"), ordered = TRUE)
      )
  
  if (year == 2024) { 
    data <- prep_2024(data) 
    }
  else if (year == 2021) { 
    data <- prep_2021(data)
  }
  else if (year == 2019) { 
    data <- prep_2019(data)
  }
  else if (year == 2016) { 
    data <- prep_2016(data)
  }
  
  return(data)
  
}


# Function to compute summary statistics -------------------

compute_summary_mainlevel_1g <- function(inidcators, groups, data, weights, psu = NULL, keep = NULL) {
  
  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]
  
  results <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_1g,
         data = data,
         iref = INDICATORS,
         gref = GROUPS,
         psu = psu,
         strata = NULL,
         w = weights)
  ) %>% 
    separate_wider_delim(indicator_name, delim = ": ", names = c("indicator_domain", "indicator_category", "indicator_stem", "indicator_branch"), too_few = "align_start") %>% 
    mutate(indicator_name = paste(indicator_stem, indicator_branch, sep = ": "), 
           indicator_group = paste(indicator_domain, indicator_category, sep = ": "))
  
  if (!is.null(keep)) {
    return(results %>% select(indicator, indicator_name, indicator_group, indicator_domain, indicator_caregory, indicator_stem, indicator_branch, group, group_name, group_cat_val, nobs, starts_with(keep)))
  } else {
    return(results)
  }
  
}

compute_summary_mainlevel_2g <- function(inidcators, groups_l1, groups_l2, data, weights, psu = NULL, keep = NULL) {
  
  combinations <- expand.grid(indicators, names(groups_l2), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]
  
  results <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_2g,
         data = data,
         iref = INDICATORS,
         g_l1 = groups_l1,
         gref = GROUPS,
         psu = psu,
         strata = NULL,
         w = weights)
  ) %>% 
    separate_wider_delim(indicator_name, delim = ": ", names = c("indicator_domain", "indicator_category", "indicator_stem", "indicator_branch"), too_few = "align_start") %>% 
    mutate(indicator_name = paste(indicator_stem, indicator_branch, sep = ": "), 
           indicator_group = paste(indicator_domain, indicator_category, sep = ": "))
  
  if (!is.null(keep)) {
    return(results %>% select(indicator, indicator_name, indicator_group, indicator_domain, indicator_category, indicator_stem, indicator_branch, group, group_name, group_cat_val, nobs, starts_with(keep)))
  } else {
    return(results)
  }
  
}




# Function to run regressions -------------------

capture_terms <- function(depvar, maineffect, confounds, data) {
  
  # Function captures the effects of simple linear regression model
  
  if (!is.null(confounds)) {
    f <- as.formula(paste(depvar, "~", paste(c(maineffect, confounds), collapse = "+")))
    flag <- 1
  } else {
    f <- as.formula(paste(depvar, "~", maineffect))
    flag <- 0
  }
  
  lm_mod <- linear_reg()
  lm_fit <- lm_mod %>% fit(f, data = data)
  rsq <- glance(lm_fit)$adj.r.squared
  tidy(lm_fit) %>%
    mutate(adj_rsquared = rsq, depvar = depvar, confounds_flag = flag) %>%
    filter(term %in% c("(Intercept)", maineffect))
  
}

capture_terms_clse <- function(depvar, maineffects, confounds, data) {
  
  # Function captures the effects of simple linear regression model with clustered standard errors
  
  if (!is.null(confounds)) {
    f <- as.formula(paste(depvar, "~", paste(c(maineffects, confounds), collapse = "+")))
    flag <- 1
  } else {
    if (length(maineffects) > 1) {
      f <- as.formula(paste(depvar, "~", paste(c(maineffects), collapse = "+")))
    } else {
      f <- as.formula(paste(depvar, "~", maineffects))
    }
    flag <- 0
  }
  
  lm_fit <- feols(f, cluster = ~ sample_psu, data = data)
  rsq <- glance(lm_fit)$adj.r.squared
  tidy(lm_fit) %>%
    mutate(adj_rsquared = rsq, depvar = depvar, confounds_flag = flag) %>%
    filter(term %in% c("(Intercept)", maineffects))
  
}

prep_fig <- function(terms, depvar_labels, effect_labels, depvarlog = FALSE, model_type_override = NULL) {
  
  terms %>% mutate(
    depvar_label = factor(depvar_labels[depvar], levels = depvar_labels, ordered = TRUE),
    effect_label = factor(effect_labels[term], levels = effect_labels, ordered = TRUE),
    fig_data = max(ifelse(term == "(Intercept)", estimate, NA), na.rm = TRUE),
    fig_data = ifelse(term != "(Intercept)", estimate + fig_data, estimate),
    model_type = factor(ifelse(confounds_flag == 0, "Model: Unadjusted", "Model: Adjusted"), levels = c("Model: Unadjusted", "Model: Adjusted"), ordered = TRUE)
  ) -> terms
  
  if(depvarlog) { 
    terms %>% mutate(fig_data = exp(fig_data)) -> terms
  }
  
  if (!is.null(model_type_override)){ 
    terms %>% mutate(
      model_type = model_type_override
    ) -> terms
  }
  
  return(terms)
  
}

model_and_prepfig <-function(depvar, maineffect, confounds, data, depvar_labels, effect_labels, depvarlog = FALSE, model_type_override = NULL) {

  terms <- capture_terms_clse(depvar, maineffect, confounds, data)
  fig <- prep_fig(terms, depvar_labels, effect_labels, depvarlog, model_type_override) 
  return(fig)
  
}




