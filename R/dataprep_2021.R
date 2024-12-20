


prep_2021 <- function(data) { 
  
  # Function-specific parameters
  
  sav_uses <- c(2, 4:6, 8:10, 15:23)
  crd_uses <- c(2, 4:8, 10:12, 17:23, 26)
  oldage <- c(1, 2, 6)
  
  #crd_uses <- 2
  
  # MFHI parameters:
  w = 1/9
  k = 0.6
  
  w_alt <- 1/7
  w_alt2 <- 1/8
  w_alt3 <- 1/3
  
  goals_levels = c(
    "food" = "Putting food on the table",
    "edu" = "Educating myself or my family",
    "live"= "Developing my livelihood/career",
    "other" = "Other"
  )
  
  mfhi_levels = c(
    "High" = "High",
    "Mid" = "Mid-range",
    "Low"= "Low/Struggling"
  )
  
data <- data %>% mutate(
  
  hh_id = paste(ClusterNo, HHNo, sep = "_"),
  mem_id = paste(hh_id, A14vi, sep = "_"),
  resp_all = "All adults",
  
  # - Dimension 1: Day to day money management
  
  # -- No trouble making money last , # Need to double check this
  mfhi_1_1 = ifelse(B1B4 == 2, 1, 0),
  mfhi_1_1 = ifelse(B1B4 %in% c(4, 5), NA, mfhi_1_1),
  
  # -- Plan for how to spend money
  mfhi_1_2 = ifelse(B1B3 == 1, 1, 0),
  mfhi_1_2 = ifelse(B1B3 %in% c(4, 5), NA, mfhi_1_2),
  
  # -- Does not go without food
  mfhi_1_3 = ifelse(B1C1 == 3, 1, 0),
  mfhi_1_3 = ifelse(B1C1 %in% c(98, 99), NA, mfhi_1_3),
  
  # - Dimension 2: Coping with risk
  
  # -- Does not go without medicine
  mfhi_2_1 = ifelse(B1C2 == 3, 1, 0),
  mfhi_2_1 = ifelse(B1C2 %in% c(98, 99), NA, mfhi_2_1),
  
  # -- Can access a lump sum (If you needed Ksh 5,500 for rural, Ksh 9,000 for urban in 3 days, would you be able to get it?)
  mfhi_2_2 = B1H,
  mfhi_2_2 = ifelse(is.na(B1H), B1I, mfhi_2_2),
  mfhi_2_2 = ifelse(mfhi_2_2 == 2, 0, mfhi_2_2),
  mfhi_2_2 = ifelse(mfhi_2_2 %in% c(98,99), NA, mfhi_2_2),
  
  # -- Keeps money aside for an emergency
  mfhi_2_3 = ifelse(B1B2 == 1, 1, 0),
  mfhi_2_3 = ifelse(B1B2 %in% c(4, 5), NA, mfhi_2_3),
  
  # - Dimension 3: Investing in the future/livelihood
  
  # -- Keeps money aside for a specific purpose
  #mfhi_3_1 = ifelse(b1b_4 == 1, 1, 0),
  #mfhi_3_1 = ifelse(b1b_4 %in% c(98, 99), NA, mfhi_3_1)
  
  #-- For 2021, using Section 3 meeting goals data
  
  mfhi_3_1_alt = ifelse(R3A == 1 & (R3C__10 ==1 | R3C__11 == 1 | R3C__12 == 1 | R3C__13 == 1 | R3C__14 == 1 | R3C__15 == 1 ), 1, 0),
  
  mfhi_3_1_alt2 = ifelse((R3C__10 ==1 | R3C__11 == 1 | R3C__12 == 1 | R3C__13 == 1 | R3C__14 == 1 | R3C__15 == 1) | (F1__1 == 1 | F1__2 == 1 | F1__3 == 1), 1, 0),
  
  
) %>%
  
  # -- Uses savings or credit for mainly for productive purposes/ long-term assets
  
  mutate(
    mfhi_3_2_sav = ifelse(F3__2 == 1 | F3__4 == 1 | F3__5 == 1 | F3__6 == 1 | F3__8 == 1 |
                            F3__9 == 1 | F3__10 == 1 | F3__15 == 1 | F3__16 == 1 |
                            F3__17 == 1 | F3__18 == 1 | F3__19 == 1 | F3__20 == 1 | F3__21 == 1 | F3__22 == 1 | F3__23 == 1, 1, 0),
    mfhi_3_2_sav = ifelse(is.na(mfhi_3_2_sav), 0, mfhi_3_2_sav),
    
  ) %>%
  
  mutate_at(vars(matches("E1iii[a-zA-Z0-9]")), ~ ifelse(. %in% crd_uses, 1, 0)) %>%
  mutate(mfhi_3_2_crd = ifelse(rowSums(.[grepl("E1iii[a-zA-Z0-9]", names(.))]) > 0, 1, 0)) %>%
  
  mutate(mfhi_3_2 = ifelse(mfhi_3_2_sav == 1 | mfhi_3_2_crd == 1, 1, 0)) %>%
  
  # -- Is using or planning to use savings, investment or pension during old age
  
  mutate(
    mfhi_3_3 = ifelse(B1EF1 == 1 | B1EF2 == 1 | B1EF6 == 1, 1, 0)
  ) %>%
  
  # Creating multi-dimensional financial health index -----------------------

mutate(
  
  mfhi_1_c = mfhi_1_1*w + mfhi_1_2*w + mfhi_1_3*w,
  mfhi_2_c = mfhi_2_1*w + mfhi_2_2*w + mfhi_2_3*w,
  mfhi_3_c = mfhi_3_1_alt*w + mfhi_3_2*w + mfhi_3_3*w,
  mfhi_o_c = mfhi_1_c + mfhi_2_c + mfhi_3_c,
  
  # Head counts
  mfhi_1_h = ifelse(mfhi_1_c >= k/3, 1, 0),
  mfhi_2_h = ifelse(mfhi_2_c >= k/3, 1, 0),
  mfhi_3_h = ifelse(mfhi_3_c >= k/3, 1, 0),
  
  mfhi_o_h = ifelse(mfhi_o_c >= k, 1, 0),
  mfhi_o_h_v16 = mfhi_o_h,
  
  # Number of domains represented if healthy
  domain1 = ifelse(mfhi_1_1 == 1 | mfhi_1_2 == 1 | mfhi_1_3 == 1, 1, 0),
  domain2 = ifelse(mfhi_2_1 == 1 | mfhi_2_2 == 1 | mfhi_2_3 == 1, 1, 0),
  domain3 = ifelse(mfhi_3_1_alt == 1 | mfhi_3_2 == 1 | mfhi_3_3 == 1, 1, 0),
  ndomains = domain1 + domain2 + domain3,
  
  all3domains = ifelse(ndomains == 3, 1, 0),
  # Percentage of financially healthy with true indicators from all 3 domains
  ndomains_v01 = ifelse(mfhi_o_c >= 0.1, all3domains, NA),
  ndomains_v02 = ifelse(mfhi_o_c >= 0.2, all3domains, NA),
  ndomains_v03 = ifelse(mfhi_o_c >= 0.3, all3domains, NA),
  ndomains_v04 = ifelse(mfhi_o_c >= 0.4, all3domains, NA),
  ndomains_v05 = ifelse(mfhi_o_c >= 0.5, all3domains, NA),
  ndomains_v06 = ifelse(mfhi_o_c >= 0.6, all3domains, NA),
  ndomains_v07 = ifelse(mfhi_o_c >= 0.7, all3domains, NA),
  ndomains_v08 = ifelse(mfhi_o_c >= 0.8, all3domains, NA),
  ndomains_v09 = ifelse(mfhi_o_c >= 0.9, all3domains, NA),
  
  # Alternative head count (any 1 from each domain)
  
  mfhi_o_h_alt = ifelse(mfhi_1_c >= 1/9 & mfhi_2_c >= 1/9 & mfhi_3_c >= 1/9, 1, 0),
  
  mfhi_o_h_high = ifelse(mfhi_o_c >= k, 1, 0),
  mfhi_o_h_mid = ifelse(mfhi_o_c >= 0.3 & mfhi_o_c < k, 1, 0),
  mfhi_o_h_low = ifelse(mfhi_o_c < 0.3, 1, 0),
  
  mfhi_o_h_level = case_when(
    mfhi_o_h_high == 1 ~ "High",
    mfhi_o_h_mid == 1 ~ "Mid",
    mfhi_o_h_low == 1 ~ "Low"
  ),
  mfhi_o_h_level_fct = factor(mfhi_levels[mfhi_o_h_level], mfhi_levels, ordered = TRUE),
  
  # Censored variables
  mfhi_1_1_dk = ifelse(mfhi_o_c >= k, mfhi_1_1, 0),
  mfhi_1_2_dk = ifelse(mfhi_o_c >= k, mfhi_1_2, 0),
  mfhi_1_3_dk = ifelse(mfhi_o_c >= k, mfhi_1_3, 0),
  
  mfhi_2_1_dk = ifelse(mfhi_o_c >= k, mfhi_2_1, 0),
  mfhi_2_2_dk = ifelse(mfhi_o_c >= k, mfhi_2_2, 0),
  mfhi_2_3_dk = ifelse(mfhi_o_c >= k, mfhi_2_3, 0),
  
  mfhi_3_1_dk = ifelse(mfhi_o_c >= k, mfhi_3_1_alt, 0),
  mfhi_3_2_dk = ifelse(mfhi_o_c >= k, mfhi_3_2, 0),
  mfhi_3_3_dk = ifelse(mfhi_o_c >= k, mfhi_3_3, 0),
  
  mfhi_o_ck = ifelse(mfhi_o_c >= k, mfhi_o_c, 0),
  
  mfhi_o_a = ifelse(mfhi_o_c >= k, mfhi_o_ck, NA),
  
  # Alternative specification that excludes 2 problematic indicators (inconsistent over time)
  
  mfhi_1_c_alt = mfhi_1_1*w_alt + mfhi_1_2*w_alt + mfhi_1_3*w_alt,
  mfhi_2_c_alt = mfhi_2_1*w_alt + mfhi_2_3*w_alt,
  mfhi_3_c_alt = mfhi_3_2*w_alt + mfhi_3_3*w_alt,
  mfhi_o_c_alt = mfhi_1_c_alt + mfhi_2_c_alt + mfhi_3_c_alt,
  
  mfhi_o_h_alt2 = ifelse(mfhi_o_c_alt >= k, 1, 0),
  
  # Alternative specification that excludes lumpsum indicator but includes intentional savings
  
  mfhi_1_c_alt2 = mfhi_1_1*w_alt2 + mfhi_1_2*w_alt2 + mfhi_1_3*w_alt2,
  mfhi_2_c_alt2 = mfhi_2_1*w_alt2 + mfhi_2_3*w_alt2,
  mfhi_3_c_alt2 = mfhi_3_1_alt*w_alt2 + mfhi_3_2*w_alt2 + mfhi_3_3*w_alt2,
  mfhi_o_c_alt2 = mfhi_1_c_alt2 + mfhi_2_c_alt2 + mfhi_3_c_alt2,
  
  mfhi_o_h_alt3 = ifelse(mfhi_o_c_alt2 >= k, 1, 0),
  
  # Alternative specification that includes lumpsum indicator but excludes intentional savings
  
  mfhi_1_c_alt3 = mfhi_1_1*w_alt2 + mfhi_1_2*w_alt2 + mfhi_1_3*w_alt2,
  mfhi_2_c_alt3 = mfhi_2_1*w_alt2 + mfhi_2_2*w_alt2 + mfhi_2_3*w_alt2,
  mfhi_3_c_alt3 = mfhi_3_2*w_alt2 + mfhi_3_3*w_alt2,
  mfhi_o_c_alt3 = mfhi_1_c_alt3 + mfhi_2_c_alt3 + mfhi_3_c_alt3,
  
  mfhi_o_h_alt4 = ifelse(mfhi_o_c_alt3 >= k, 1, 0),
  
  # Alternative specification that aligns with what CBK/KNBS did in 2017
  
  mfhi_o_c_alt4 = mfhi_1_c + mfhi_2_c + (mfhi_3_1_alt2*w + mfhi_3_2*w + mfhi_3_3*w),
  mfhi_o_h_alt5 = ifelse(mfhi_o_c_alt4 >= k, 1, 0),
  
  # liquidity distress in past 12 months
  
  liqdist_any = ifelse(R1A == 1, 1, 0),
  liqdist_any = ifelse(R1A %in% c(98, 99) | is.na(R1A), NA, liqdist_any),
  liqdist_reg = ifelse(liqdist_any == 1 & R1B %in% c(1,2), 1, 0),
  liqdist_reg = ifelse(is.na(liqdist_any), NA, liqdist_reg),
  
  # Months of savings incase income lost
  
  mfhi_savingsgt1mo = ifelse(B1K %in% c(3, 4), 1, 0),
  
  # Child sent home from school
  
  mfhi_senthome = ifelse(B1C4 %in% c(1,2), 1, 0),
  mfhi_senthome = ifelse(B1C4 %in% c(98, 99), NA, mfhi_senthome),
  
  # Alternative specification that only includes outcome indicators:
  
  mfhi_1_c_outcome = mfhi_1_1*w_alt3 + mfhi_1_3*w_alt3,
  mfhi_2_c_outcome = mfhi_2_1*w_alt3,
  mfhi_o_c_outcome = mfhi_1_c_outcome + mfhi_2_c_outcome,
  mfhi_o_h_outcome = ifelse(mfhi_o_c_outcome >= k, 1, 0),
  
  mfhi_o_h_outcome_v2 = ifelse(mfhi_2_1 == 1 & mfhi_1_3 == 1, 1, 0),
  mfhi_o_h_outcome_v2 = ifelse(is.na(mfhi_o_h_outcome_v2), 0, mfhi_o_h_outcome_v2),
  
  # At this point in your life what is your most important goal:
  
  goals_group = case_when(
    B1A == 1 ~ "food",
    B1A == 2 ~ "educ",
    B1A %in% c(3, 11) ~ "live",
    B1A %in% c(4, 5, 6, 12) ~ "other"
  ),
  goals_group_fct = factor(goals_levels[goals_group], goals_levels, ordered = TRUE),
  
  # --- Time consistent indicators, mumber of positive outcomes: 
  
  mfhi_tc_sum = mfhi_1_2 + mfhi_1_3 + mfhi_2_2 + mfhi_2_3 + mfhi_3_2, 
  mfhi_tc_0 = ifelse(mfhi_tc_sum == 0, 1, 0), 
  mfhi_tc_0 = ifelse(is.na(mfhi_tc_sum), NA, mfhi_tc_0), 
  mfhi_tc_any1 = ifelse(mfhi_tc_sum == 1, 1, 0), 
  mfhi_tc_any1 = ifelse(is.na(mfhi_tc_any1), NA, mfhi_tc_any1), 
  mfhi_tc_any2 = ifelse(mfhi_tc_sum == 2, 1, 0), 
  mfhi_tc_any2 = ifelse(is.na(mfhi_tc_sum), NA, mfhi_tc_any2), 
  mfhi_tc_any3 = ifelse(mfhi_tc_sum == 3, 1, 0), 
  mfhi_tc_any3 = ifelse(is.na(mfhi_tc_sum), NA, mfhi_tc_any3), 
  mfhi_tc_any4 = ifelse(mfhi_tc_sum == 4, 1, 0), 
  mfhi_tc_any4 = ifelse(is.na(mfhi_tc_sum), NA, mfhi_tc_any4), 
  mfhi_tc_all5 = ifelse(mfhi_tc_sum == 5, 1, 0), 
  mfhi_tc_all5 = ifelse(is.na(mfhi_tc_sum), NA, mfhi_tc_all5)
  
) %>% dummy_cols(select_columns = c('goals_group'))

# Shocks

shocks_levels <- c(
  "clm" = "Climate shock (including crop diseases/pests)",
  "ill" = "Illness or accident",
  "dthi" = "Death of main income earner",
  "dtho" = "Death of other family member",
  "thf" = "Theft or fire",
  "brth" = "Birth of child",
  "job" = "Job or income loss",
  "infl" = "Cost of living")

shocks_levels2 <- c(
  "clm" = "Climate shock (including crop diseases/pests)",
  "ill" = "Illness or accident",
  "dth" = "Death of family member or relative",
  "thf" = "Theft or fire",
  "brth" = "Birth of child",
  "job" = "Job or income loss",
  "infl" = "Cost of living")

coping_levels <- c(
  "borr_for" = "Borrowed from Bank/SACCO/MFI",
  "borr_for_d" = "Borrowed from mobile or app",
  "borr_inf" = "Borrowed from social network",
  "sav_for" = "Savings held at Bank/SACCO/MFI",
  "sav_for_d" ="Savingss held in mobile wallet" ,
  "sav_inf_s" = "Savings held socially",
  "sav_cash" = "Savings held in cash",
  "sell_ass" = "Sold assets",
  "trnsf_inf" = "Transfers from social network",
  "cut_exp" = "Cut back on expenses",
  "job_chng" = "Changed job",
  "oth" = "Other"
)

data <- data %>% mutate(
  
  # Has respondent experience any of the following adverse shocks in past year:
  shocks_illness = case_when(R2A__1 == 1 ~ 1, R2A__1 == 0 ~ 0, is.na(R2A__1) ~ NA_real_),
  shocks_climate = case_when(R2A__2 == 1 ~ 1, R2A__2 == 0 ~ 0, is.na(R2A__2) ~ NA_real_),
  shocks_pests = case_when(R2A__9 == 1 ~ 1, R2A__9 == 0 ~ 0, is.na(R2A__9) ~ NA_real_),
  shocks_deathinc = case_when(R2A__3 == 1 ~ 1, R2A__3 == 0 ~ 0, is.na(R2A__3) ~ NA_real_),
  shocks_deathoth = case_when(R2A__4 == 1 ~ 1, R2A__4 == 0 ~ 0, is.na(R2A__4) ~ NA_real_),
  shocks_asset = case_when(R2A__5 == 1 ~ 1, R2A__5 == 0 ~ 0, is.na(R2A__5) ~ NA_real_),
  shocks_income = case_when(R2A__7 == 1 ~ 1, R2A__7 == 0 ~ 0, is.na(R2A__7) ~ NA_real_),
  shocks_prices = case_when(R2A__8 == 1 ~ 1, R2A__8 == 0 ~ 0, is.na(R2A__8) ~ NA_real_),
  shocks_other = 0,
  
  #shocks_other = case_when(R2A__10 == 1 ~ 1, R2A__10 == 2 ~ 0, is.na(R2A__10) ~ NA_real_),
  shocks_N = shocks_illness + shocks_climate + shocks_pests + shocks_deathinc + shocks_deathoth + shocks_asset + shocks_income + shocks_prices + shocks_other,
  shocks_health = ifelse(shocks_illness == 1 | shocks_deathinc == 1 | shocks_deathoth == 1, 1, 0),
  shocks_health = ifelse(is.na(shocks_illness) & is.na(shocks_deathinc) & is.na(shocks_deathoth), NA, shocks_health),
  shocks_natural = ifelse(shocks_climate == 1 | shocks_pests == 1, 1, 0),
  shocks_natural = ifelse(is.na(shocks_climate) & is.na(shocks_pests), NA, shocks_natural),
  shocks_deathany = ifelse(shocks_deathinc == 1 | shocks_deathoth == 1, 1, 0),
  shocks_deathany = ifelse(is.na(shocks_deathinc) & is.na(shocks_deathoth), NA, shocks_deathany),
  shocks_any = ifelse(shocks_illness == 1 | shocks_climate == 1 | shocks_deathinc == 1 | shocks_deathoth == 1 | shocks_asset == 1 | shocks_income == 1 | shocks_prices == 1 | shocks_pests == 1 | shocks_other == 1, 1, 0),
  shocks_any = ifelse(is.na(shocks_illness) & is.na(shocks_climate) & is.na(shocks_deathinc) & is.na(shocks_deathoth) & is.na(shocks_asset) & is.na(shocks_income) & is.na(shocks_prices) & is.na(shocks_pests) & is.na(shocks_other), NA, shocks_any),
  
  # Is shock attributable to COVID:
  
  shocks_illness_covid = R2Ai__1,
  shocks_illness_covid = ifelse(shocks_illness == 0, NA, shocks_illness_covid),
  
  shocks_deathany_covid = ifelse(R2Ai__3 == 1 | R2Ai__4 == 1, 1, 0),
  shocks_deathany_covid = ifelse(shocks_deathany == 0, NA, shocks_deathany_covid),
  
  shocks_health_covid = ifelse(shocks_deathany_covid == 1 | shocks_illness_covid == 1, 1, 0),
  shocks_health_covid = ifelse(shocks_health == 0, NA, shocks_health_covid),
  
  shocks_income_covid = ifelse(R2Ai__7 == 1, 1, 0),
  shocks_income_covid = ifelse(shocks_income == 0, NA, shocks_income_covid),
  
  shocks_prices_covid = ifelse(R2Ai__8 == 1, 1, 0),
  shocks_prices_covid = ifelse(shocks_prices == 0, NA, shocks_prices_covid),
  
  shocks_natural_covid = ifelse(R2Ai__2 == 1 | R2Ai__9 == 1, 1, 0),
  shocks_natural_covid = ifelse(shocks_natural == 0, NA, shocks_natural_covid),
  
  # Main shock
  
  shocks_main_group = case_when(
    R2B == 1 ~ "ill",
    R2B %in% c(2, 9) ~ "clm",
    R2B == 3 ~ "dthi",
    R2B == 4 ~ "dtho",
    R2B == 5 ~ "thf",
    R2B == 6 ~ "brth",
    R2B == 7 ~ "job",
    R2B == 8 ~ "infl"
  ),
  shocks_main_group_fct = factor(shocks_levels[shocks_main_group], shocks_levels, ordered = TRUE),
  
  shocks_main_group2 = case_when(
    R2B == 1 ~ "ill",
    R2B %in% c(2, 9) ~ "clm",
    R2B %in% c(3, 4) ~ "dth",
    R2B == 5 ~ "thf",
    R2B == 6 ~ "brth",
    R2B == 7 ~ "job",
    R2B == 8 ~ "infl"
  ),
  shocks_main_group2_fct = factor(shocks_levels2[shocks_main_group2], shocks_levels2, ordered = TRUE),
  
  
  #
  shocks_maincop_group = case_when(
    
    R2Di %in% c(1,6) ~ "borr_for",
    R2Di %in% c(2,3, 10) ~ "borr_for_d",
    R2Di %in% c(4,5,7,8, 9, 21) ~ "borr_inf",
    R2Di %in% c(11) ~ "sav_for",
    R2Di %in% c(12, 13) ~ "sav_for_d",
    R2Di %in% c(14, 15) ~ "sav_inf_s",
    R2Di %in% c(16) ~ "sav_cash",
    R2Di %in% c(17, 18) ~ "sell_ass",
    R2Di %in% c(19) ~ "trnsf_inf",
    R2Di %in% c(20) ~ "cut_exp",
    R2Di %in% c(25) ~ "job_chng",
    R2Di %in% c(22, 23, 24, 26, 27) ~ "oth"
    
  ),
  
  shocks_maincop_group_fct = factor(coping_levels[shocks_maincop_group], coping_levels, ordered = TRUE)
  
) %>% dummy_cols(select_columns = c('shocks_main_group', "shocks_maincop_group", "shocks_main_group2"))


# Creating socio-economic, demographic variables -----------------------

# Family structure, age, gender

ASAL_counties_FA <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 15, 16, 17, 23, 24, 25, 28, 30, 31, 33, 34, 19, 13, 12, 14, 44, 43, 32, 22)

geo_levels <- c("rur_asal" = "Rural, ASAL county",
                "urb_nbo" = "Urban, Nairobi county",
                "urb_oth" = "Urban, other",
                "rur_oth" = "Rural, other")

geo2_levels <- c("rur" = "Rural",
                 "urb_nbo" = "Nairobi",
                 "urb_oth" = "Non-Nairobi Urban")

geo3_levels <- c("rur_asal" = "Rural, ASAL county",
                 "rur_oth" = "Rural, other",
                 "urb" = "Urban")

hhtype_levels <- c("smh_nmr" = "Single member HH: Never married",
                   "smh_div" = "Single member HH: Divorced or widowed",
                   "smh_mar" = "Single member HH: Married (living alone)",
                   "mmh_wch" = "Multi member HH: With school-age children",
                   "mmh_nch" = "Multi member HH: Without school-age children")


hhtype2_levels <- c("smh" = "Single member HH",
                    "mmh_nuc" = "Multi-member HH: Nuclear family",
                    "mmh_mnc" = "Multi-member HH: Married, no children",
                    "mmh_ext" = "Multi-member HH: Extended family",
                    "mmh_sip" = "Multi member HH: Single parents")

# Merging hh_type variable from roster:

#FA2018 <- FA2018 %>% left_join(FA2018_roster, by = "hh_id")

data <- data %>%
  mutate(
    sample_psu = ClusterNo,
    sample_weights = IndWeight,
    hh_urbrur = ifelse(A9 == 1, "Rural", "Urban"),
    hh_geo = case_when(
      hh_urbrur == "Rural" & (County %in% ASAL_counties_FA) ~ "rur_asal",
      County == 47 ~ "urb_nbo",
      hh_urbrur == "Urban" ~ "urb_oth",
      hh_urbrur == "Rural" ~ "rur_oth"
    ),
    hh_geo_2 = case_when(
      County == 47 ~ "urb_nbo",
      hh_urbrur == "Urban" ~ "urb_oth",
      hh_urbrur == "Rural" ~ "rur"
    ),
    hh_geo_3 = case_when(
      hh_urbrur == "Rural" & (County %in% ASAL_counties_FA) ~ "rur_asal",
      hh_urbrur == "Urban" ~ "urb",
      hh_urbrur == "Rural" ~ "rur_oth"
    ),
    hh_geo_fct = factor(geo_levels[hh_geo], levels = geo_levels, ordered = TRUE),
    hh_geo2_fct = factor(geo2_levels[hh_geo_2], levels = geo2_levels, ordered = TRUE),
    hh_geo3_fct = factor(geo2_levels[hh_geo_2], levels = geo3_levels, ordered = TRUE),
    hh_size_all = NHM,
    hh_size_all_c = hh_size_all - mean(hh_size_all),
    hh_urbrur = case_when(
      hh_urbrur == "Urban" ~ "urb",
      hh_urbrur == "Rural" ~ "rur"
    )
    #hh_size_children = a11,
    #hh_size_adults = a10 - a11,
    #hh_singlemember = ifelse(a10 == 1, 1, 0),
    #hh_multimember = ifelse(a10 > 1, 1, 0),
    #hh_children = ifelse(a11 > 0, 1, 0),
    #hh_head_female = ifelse(HeadofHousehold_sex == 2, 1, 0),
    #hh_head_sex_fct = ifelse(HeadofHousehold_sex == 2, "Female", "Male"),
    #hh_head_age = Headofhousehold_Age,
    #hh_type = ifelse(hh_singlemember == 1 & a17 == 1, "smh_nmr", NA),
    #hh_type = ifelse(hh_singlemember == 1 & a17 %in% c(2,3), "smh_div", hh_type),
    #hh_type = ifelse(hh_singlemember == 1 & a17 == 4, "smh_mar", hh_type),
    #hh_type = ifelse(hh_singlemember == 0 & hh_children == 1, "mmh_wch", hh_type),
    #hh_type = ifelse(hh_singlemember == 0 & hh_children == 0, "mmh_nch", hh_type),
    #hh_type_fct = factor(hhtype_levels[hh_type], levels = hhtype_levels),
    #hh_type_2_fct = factor(hhtype2_levels[hh_type_2], levels = hhtype2_levels),
  ) %>% dummy_cols(select_columns = c("hh_type", "hh_urbrur", "hh_geo", "hh_geo_3"))

age_levels <- c("age_18_25" = "[18-25)",
                "age_25_45" = "[25-45)",
                "age_45_65" = "[45-65)",
                "age_65" = "[65+]")

age2_levels <- c("age_18_25" = "[18-25)",
                 "age_25_65" = "[25-65)",
                 "age_65" = "[65+]")

marstat_levels <- c("nmr" = "Never married",
                    "div" = "Separated, divorced or widowed",
                    "mar" = "Married")

edu_levels <- c("none" = "No formal education",
                "smpr" = "Some primary",
                "cmpr" = "Primary complete",
                "smsc" = "Some secondary",
                "cmsc" = "Secondary complete",
                "smtr" = "Some or complete tertiary")

edu_levels2 <- c("none" = "No formal education",
                 "pri" = "Some or complete primary",
                 "sec" = "Some or complete secondary",
                 "trt" = "Some or complete tertiary")

edu_levels3 <- c("prinone" = "Less then complete primary",
                 "sec" = "Some or complete secondary",
                 "trt" = "Some or complete tertiary")

live_levels <- c("farm" = "Agriculture",
                 "empl" = "Employed",
                 "cwrk" = "Casual",
                 "owbs" = "Own business",
                 "trns" = "Transfers",
                 "othr" = "Other")

live_levels2 <- c("farm" = "Farming",
                  "empl" = "Employment",
                  "cwrk" = "Casual work",
                  "owbs" = "Own business",
                  "trns" = "Transfers",
                  "inv" = "Investment",
                  "othr" = "Other")

gender_levels <- c("men" = "Men", "wmn" = "Women")

inc_levels <- c("inc_1" = "< KSh 2,500",
                "inc_2" = "KSh 2,500 - 5,000",
                "inc_3" = "KSh 5,000 - 10,000",
                "inc_4" = "KSh 10,000 - 15,000",
                "inc_5" = "KSh 15,000+")


data <- data %>%
  mutate(
    
    # Did financial status improve, stay the same or worsen in past year?
    fin_status_impr = ifelse(B1G == 1, 1, 0),
    fin_status_impr = ifelse(B1G %in% c(98,99), NA, fin_status_impr),
    fin_status_worse = ifelse(B1G == 3, 1, 0),
    fin_status_worse = ifelse(B1G %in% c(98,99), NA, fin_status_worse),
    
    fin_status_fct = ifelse(fin_status_impr == 1, "Financial status: Improved", "Financial status: Worsened"),
    fin_status_fct = ifelse(is.na(fin_status_impr) & is.na(fin_status_worse), NA, fin_status_fct),
    
    resp_gender_group = ifelse(A18 == 1, "men", "wmn"),
    resp_gender_fct = factor(gender_levels[resp_gender_group], gender_levels, ordered = TRUE),
    
    resp_age_yrs = A19,
    
    resp_age_group = case_when(
      resp_age_yrs < 25 ~ "age_18_25",
      resp_age_yrs < 45 ~ "age_25_45",
      resp_age_yrs < 65 ~ "age_45_65",
      resp_age_yrs >= 65 ~ "age_65"
    ),
    resp_age_group_fct = factor(age_levels[resp_age_group], levels = age_levels, ordered = TRUE),
    
    resp_age_group2 = case_when(
      resp_age_yrs < 25 ~ "age_18_25",
      resp_age_yrs < 65 ~ "age_25_65",
      resp_age_yrs >= 65 ~ "age_65"
    ),
    resp_age_group2_fct = factor(age2_levels[resp_age_group2], levels = age2_levels, ordered = TRUE),
    
    resp_marstat = case_when(
      A22 == 1 ~ "nmr",
      A22 %in% c(2,3)  ~ "div",
      A22 == 4  ~ "mar"
    ),
    resp_marstat_fct = factor(marstat_levels[resp_marstat], levels = marstat_levels, ordered = TRUE),
    resp_edu_group = case_when(
      A21 == 1 ~ "none",
      A21 == 2 ~ "smpr",
      A21 == 3 ~ "cmpr",
      A21 == 4 ~ "smsc",
      A21 == 5 ~ "cmsc",
      A21 %in% c(6, 7, 8, 9) ~ "smtr"
    ),
    resp_edu_group_fct = factor(edu_levels[resp_edu_group], levels = edu_levels, ordered = TRUE),
    resp_edu_group2 = case_when(
      A21 == 1 ~ "none",
      A21 %in% c(2, 3) ~ "pri",
      A21 %in% c(4, 5) ~ "sec",
      A21 %in% c(6, 7, 8, 9) ~ "trt",
    ),
    resp_edu_group2_fct = factor(edu_levels2[resp_edu_group2], levels = edu_levels2, ordered = TRUE),
    
    resp_edu_group3 = case_when(
      A21 %in% c(1,2,3) ~ "prinone",
      A21 %in% c(4, 5) ~ "sec",
      A21 %in% c(6, 7, 8, 9) ~ "trt",
      A21 %in% c(98,99) ~ NA
    ),
    resp_edu_group3_fct = factor(edu_levels3[resp_edu_group3], levels = edu_levels3, ordered = TRUE),
    
    # Main income source
    resp_live_group = case_when(
      B3B == 1 ~ "farm",
      B3B == 2 ~ "empl",
      B3B == 3 ~ "cwrk",
      B3B == 4 ~ "owbs",
      B3B %in% c(5, 8, 9) ~ "trns",
      B3B %in% c(6, 7, 10) ~ "othr"
    ),
    resp_live_group_fct = factor(live_levels[resp_live_group], levels = live_levels, ordered = TRUE),
    
    resp_live_group2 = case_when(
      B3B == 1 ~ "farm",
      B3B == 2 ~ "empl",
      B3B == 3 ~ "cwrk",
      B3B == 4 ~ "owbs",
      B3B %in% c(5, 8, 9) ~ "trns",
      B3B %in% c(6, 7) ~ "inv",
      B3B == 10 ~ "othr"
    ),
    resp_live_group2_fct = factor(live_levels2[resp_live_group2], levels = live_levels2, ordered = TRUE),
    
    # Income from any source
    resp_live_agri = ifelse(B3A__1 == 1, 1, 0),
    resp_live_emp =  ifelse(B3A__2 == 1, 1, 0),
    resp_live_cwrk =  ifelse(B3A__3 == 1, 1, 0),
    resp_live_owbs =  ifelse(B3A__4 == 1, 1, 0),
    resp_live_inv =  ifelse(B3A__6 == 1 | B3A__7 == 1, 1, 0),
    resp_live_trnsf = ifelse(B3A__5 == 1 | B3A__8 == 1 | B3A__9 == 1, 1, 0),
    resp_live_other = 0,
    
    resp_live_trnsf_sn = ifelse(B3A__9 == 1, 1, 0),
    resp_live_trnsf_in = ifelse(B3A__5 == 1 | B3A__8 == 1, 1, 0),
    
    # Secondary income source
    resp_live_agri_sec = ifelse(resp_live_agri == 1 & B3B != 1, 1, 0),
    resp_live_emp_sec = ifelse(resp_live_emp == 1 & B3B != 2, 1, 0),
    resp_live_cwrk_sec = ifelse(resp_live_cwrk == 1 & B3B != 3, 1, 0),
    resp_live_owbs_sec = ifelse(resp_live_owbs == 1 & B3B != 4, 1, 0),
    resp_live_trnsf_sec = ifelse(resp_live_trnsf == 1 & B3B %not_in% c(5, 8, 9), 1, 0),
    resp_live_inv_sec = ifelse(resp_live_inv == 1 & B3B %not_in% c(6, 7), 1, 0),
    resp_live_other_sec = ifelse(resp_live_other == 1 & B3B != 10, 1, 0),
    
    resp_live_agricon = ifelse(B3C == 1, 1, 0),
    
    resp_live_farm_any = ifelse(resp_live_agri == 1 | resp_live_agricon == 1, 1, 0),
    
    resp_live_informal_any = ifelse(resp_live_agri == 1 | resp_live_cwrk == 1 | resp_live_owbs == 1, 1, 0),
    resp_live_informal_main = ifelse(B3B %in% c(1,3,4), 1, 0),
    resp_live_informal_sec = ifelse(resp_live_agri_sec == 1 | resp_live_cwrk_sec == 1 | resp_live_owbs_sec == 1, 1, 0),
    
    resp_live_sec_any = ifelse(resp_live_agri_sec == 1 | resp_live_emp_sec == 1 | resp_live_cwrk_sec == 1 | resp_live_owbs_sec == 1 | resp_live_trnsf_sec == 1 | resp_live_other_sec == 1, 1, 0),
    
    resp_income = B3I,
    resp_income = ifelse(B3I %in% c(98,99), NA, resp_income),
    
    resp_incsource_N = resp_live_agri + resp_live_emp + resp_live_cwrk + resp_live_owbs + resp_live_inv + resp_live_trnsf + resp_live_other,
    
    resp_inc_group = case_when(
      resp_income < 2500 ~ "inc_1",
      resp_income < 5000 ~ "inc_2",
      resp_income < 10000 ~ "inc_3",
      resp_income < 15000 ~ "inc_4",
      resp_income >= 15000 ~ "inc_5"
    ),
    resp_inc_group_fct = factor(inc_levels[resp_inc_group], levels = inc_levels, ordered = TRUE),
    
    
    resp_income_decile = cut(resp_income,
                             breaks=quantile(resp_income, probs=seq(0,1, by=0.1), na.rm=TRUE),
                             labels=c("Porest 10%","Q2","Q3","Q4","Q5", "Q6", "Q7", "Q8", "Q9", "Richest 10%"),
                             include.lowest=TRUE),
    
    resp_income_quintile = cut(resp_income,
                               breaks=quantile(resp_income, probs=seq(0,1, by=0.2), na.rm=TRUE),
                               labels=c("Porest 20%","Q2","Q3","Q4","Richest 20%"),
                               include.lowest=TRUE),
    
    resp_income_agg3_str = case_when(
      resp_income_quintile %in% c("Poorest 20%", "Q2") ~ "Poorest 40%", 
      resp_income_quintile %in% c("Q3", "Q4") ~ "Middle 40%", 
      resp_income_quintile %in% c("Richest 20%") ~ "Richest 20%", 
    ), 
    
    # Is income below minimum wage?
    resp_income_lt_ag = ifelse(resp_income < 9014, 1, 0),
    resp_income_lt_ag = ifelse(hh_geo2_fct == "Rural", resp_income_lt_ag, NA),
    
    resp_income_lt_nbo = ifelse(resp_income < 21311, 1, 0),
    resp_income_lt_nbo = ifelse(hh_geo2_fct == "Nairobi", resp_income_lt_nbo, NA),
    
    resp_income_lt_our = ifelse(resp_income < 16841, 1, 0),
    resp_income_lt_our = ifelse(hh_geo2_fct == "Non-Nairobi Urban", resp_income_lt_our, NA),
    
    resp_income_lt_mw = ifelse(hh_geo2_fct == "Nairobi", resp_income_lt_nbo,
                               ifelse(hh_geo2_fct == "Rural", resp_income_lt_ag, resp_income_lt_our)),
    
    resp_live_trnsf_sn_only = ifelse(resp_live_trnsf_sn == 1 & resp_live_trnsf_in == 0, 1, 0),
    resp_live_trnsf_in_only = ifelse(resp_live_trnsf_sn == 0 & resp_live_trnsf_in == 1, 1, 0),
    resp_live_trnsf_both = ifelse(resp_live_trnsf_sn == 1 & resp_live_trnsf_in == 1, 1, 0),
    
    # resp_ppi_pred_npl = probability_pov_npl,
    # resp_ppi_decile_npl = cut(probability_pov_npl,
    #                           breaks=quantile(probability_pov_npl, probs=seq(0,1, by=0.1), na.rm=TRUE),
    #                           labels=c("Richest 10%","Q9","Q8","Q7","Q6", "Q5", "Q4", "Q3", "Q2", "Poorest 10%"),
    #                           include.lowest=TRUE),
    # resp_ppi_group_npl = ifelse(resp_ppi_decile_npl %in% c("Q4", "Q3", "Q2", "Poorest 10%"), "Bottom 40%",
    #                             ifelse(resp_ppi_decile_npl %in% c("Q8","Q7","Q6", "Q5"), "Middle 40%", "Top 20%"))
    
    # Has social support
    resp_socialfinsupport = ifelse(B1B1 == 1, 1, 0),
    
    # Has disability
    resp_disability = ifelse(A25 == 1, 1, 0),
    
    # ANyone in household has chronic disease
    hh_chronicdisease = ifelse(A24 == 1, 1, 0),
    
  ) %>% dummy_cols(select_columns = c("resp_age_group2", "resp_gender_group", "resp_age_group", "resp_marstat", "resp_edu_group", "resp_edu_group2", "resp_live_group", "resp_live_group2", "resp_inc_group"))

# Imputing missing personal monthly income variables

# data %>%
#   group_by(hh_urbrur, resp_gender_group, resp_live_group_fct) %>%
#   mutate(
#     mean_income = mean(resp_income, na.rm = TRUE),
#     resp_income = ifelse(is.na(resp_income), mean_income, resp_income),
#     resp_income = ifelse(resp_income == 0, 1, resp_income),
#     resp_income_std = (resp_income - mean(resp_income, na.rm = TRUE))/ sd(resp_income, na.rm = TRUE),
#     resp_income_log = log(resp_income),
#     resp_income_log_std = (resp_income_log - mean(resp_income_log, na.rm = TRUE))/ sd(resp_income_log, na.rm = TRUE),
#     resp_income_1000 = resp_income/1000,
#     resp_income_1000_c = resp_income_1000 - mean(resp_income_1000)
#   ) %>% ungroup() -> FA2021

# Behavioral variables
# Financial numeracy
# Digital numeracy

data <- data %>%
  mutate(
    know_fin_numeracy = ifelse(B2F == 1, 1, 0),
    #know_fin_numeracy = ifelse(B2F %in% c(98,99), NA, know_fin_numeracy),
    know_dig_literacy = ifelse(B2G == 1, 1, 0 ),
    #know_dig_literacy = ifelse(B2G %in% c(9998, 9999), NA, know_dig_literacy)
  )


# Creating financial service usage variables -----------------------

clean_NAs <- function(x) { ifelse(is.na(x), 99, x) }

data <- data %>%
  mutate_at(vars(starts_with("C1_")), clean_NAs) %>%
  mutate_at(vars(starts_with("E2B")), clean_NAs)

data <- data %>%
  mutate(
    
    # Account ownership/ informal device usage
    fin_account_mm = ifelse(C1_3 == 1 | C1_9 == 1, 1, 0),
    
    fin_account_bank = ifelse(C1_28 == 1  | C1_32 == 1 |
                                C1_11 == 1 | C1_33 == 1 | C1_31 == 1 |
                                C1_29 == 1 | C1_27 == 1 | C1_30 == 1 |
                                C1_12 == 1 | C1_2 == 1 | C1_10 == 1, 1, 0),
    
    fin_account_tbank =  ifelse(C1_28 == 1  | C1_32 == 1 |
                                  C1_11 == 1 | C1_33 == 1 | C1_31 == 1 |
                                  C1_29 == 1 | C1_27 == 1 | C1_30 == 1, 1, 0),
    
    fin_account_nbfi = ifelse(C1_14 == 1 | C1_15 == 1 | C1_1 == 1 | C1_4 == 1, 1, 0),
    
    fin_account_tbanknbfi = ifelse(fin_account_tbank == 1 | fin_account_nbfi == 1, 1, 0),
    
    fin_account_any = ifelse(fin_account_mm == 1 | fin_account_bank == 1 | fin_account_nbfi == 1, 1, 0),
    
    fin_chama_user = ifelse(C1_17 == 1 | C1_5 == 1, 1, 0),
    
    fin_socialnetwork_user = ifelse(C1_16 == 1 | C1_19 == 1 | C1_20  == 1 | C1_21 == 1 | C1_22 == 1 | C1_24 == 1 | C1_6 == 1 | C1_7 == 1, 1, 0),
    fin_socialnetwork_user_ff = ifelse( C1_6 == 1 | C1_7 == 1 | C1_20 == 1, 1, 0),
    
    fin_cash_user = ifelse(C1_8 == 1, 1, 0),
    
    fin_digital_user = ifelse(fin_account_mm == 1 | C1_2 == 1 | C1_10 == 1 | C1_12 == 1 | C1_13 == 1 | C1_23 == 1, 1, 0),
    
    fin_digital_mm = ifelse(C1_9 == 1, 1, 0),
    fin_digital_mb = ifelse(C1_10 == 1, 1, 0),
    fin_digital_app = ifelse(C1_23 == 1, 1, 0),
    
    fin_account_nobank = ifelse(C1_10 %in% c(2,3) & C1_28 %in% c(2,3) & C1_29 %in% c(2,3)  & C1_30 %in% c(2,3) & C1_31 %in% c(2,3) & C1_32 %in% c(2,3) & C1_33 %in% c(2,3) & C1_34 %in% c(2,3), 1, 0),
    
    # Savings:
    
    fst_savings_tbank = ifelse(C1_28 == 1 | C1_29 == 1 | C1_30 == 1 | C1_31 == 1, 1, 0),
    fst_savings_nbfi = ifelse(C1_1 == 1 | C1_4 == 1, 1, 0),
    fst_savings_mm = ifelse(C1_3 == 1, 1, 0),
    fst_savings_mbank = ifelse(C1_2 == 1, 1, 0),
    fst_savings_cash = ifelse(C1_8 == 1, 1, 0),
    fst_savings_network = ifelse(C1_7 == 1 | C1_6 == 1, 1, 0),
    fst_savings_sg = ifelse(C1_5 == 1, 1, 0),
    fst_savings_social = ifelse(fst_savings_sg == 1 | fst_savings_network == 1, 1, 0),
    fst_savings_any = ifelse(fst_savings_tbank == 1 | fst_savings_nbfi == 1 | fst_savings_mm == 1 | fst_savings_mbank == 1 | fst_savings_cash == 1 | fst_savings_network == 1 | fst_savings_sg == 1, 1, 0),
    fst_savings_informal_any = ifelse(fst_savings_cash == 1 | fst_savings_network == 1 | fst_savings_sg == 1, 1, 0),
    fst_savings_formaloth_any = ifelse(fst_savings_tbank == 1 | fst_savings_mbank == 1 | fst_savings_nbfi == 1, 1, 0),
    
    #Debt
    fin_debt_total = rowSums(.[grepl("E1x[a-zA-Z0-9]", names(.))], na.rm = T),
    fin_debt_any = ifelse(fin_debt_total > 0, 1, 0),
    fin_debt_inc = fin_debt_total/(ifelse(resp_income == 0, 1, resp_income)),
    fin_debt_high = ifelse(fin_debt_inc > 0.5, 1, 0),
    
    fin_debt_repaystress = ifelse(E2B1 == 1 | E2B3 == 1 | E2B4 == 1, 1, 0),
    fin_debt_repaystress_noborrow = ifelse(E2B3 == 1 | E2B4 == 1, 1, 0),
    fin_debt_repaystress_sellassets = ifelse(E2B3 == 1, 1, 0),
    fin_debt_repaystress_sellassets_inv = ifelse(fin_debt_repaystress_sellassets == 1, 0, 1),
    fin_debt_default = ifelse(E2Cii %in% c(1, 2), 1, 0),
    fin_debt_stressany = ifelse(fin_debt_repaystress == 1 | fin_debt_default == 1, 1, 0),
    fin_debt_stressany_inv = ifelse(fin_debt_stressany == 1, 0, 1),
    
    fin_digital_borrower = ifelse((C1_12 == 1 | C1_23 == 1 | C2_12 == 1), 1, 0),
    fin_digital_borrower = ifelse(is.na(fin_digital_borrower), 0, fin_digital_borrower),
    
    fst_loans_fint = ifelse(C1_11 == 1 | C1_14 == 1 | C1_15 == 1, 1, 0),
    fst_loans_mbank = ifelse(C1_12 == 1 | C1_23 == 1, 1, 0),
    fst_loans_fuliza = ifelse(C1_13 == 1, 1, 0),
    fst_loans_shopcredit = ifelse(C1_21  == 1 | C1_22 == 1 , 1, 0),
    fst_loans_sg = ifelse(C1_17 == 1, 1, 0),
    fst_loans_family = ifelse(C1_20 == 1, 1, 0),
    
    fst_loans_informal = ifelse(fst_loans_sg == 1 | fst_loans_family == 1 | fst_loans_shopcredit == 1, 1, 0),
    
    fst_loans_agg_social = ifelse(fst_loans_shopcredit == 1 | fst_loans_family == 1, 1, 0),
    fst_loans_agg_trad = ifelse(fst_loans_sg == 1 | fst_loans_fint == 1, 1, 0),
    fst_loans_agg_dig = ifelse(fst_loans_fuliza == 1 | fst_loans_mbank == 1, 1, 0),
    
    access_strand = case_when(
      access == 1 ~ "Formal_prudential",
      access %in% c(2, 3) ~ "Formal_other",
      access == 4 ~ "Informal",
      access == 5 ~ "Excluded"
    ),
    
    fin_fh_cmpscore_3c = mfhi_o_h_outcome_v2 + mfhi_2_2 + fin_debt_stressany_inv,
    fin_fh_cmpscore_4c = mfhi_o_h_outcome_v2 + mfhi_2_2 + fin_debt_stressany_inv + mfhi_2_3,
    fin_fh_cmpscore_5c = mfhi_o_h_outcome_v2 + mfhi_2_2 + fin_debt_stressany_inv + mfhi_2_3 + mfhi_3_2,
    
    fin_fh_cmpscore_3c_alt = mfhi_o_h_outcome_v2 + mfhi_2_2 + fin_debt_repaystress_sellassets_inv,
    fin_fh_cmpscore_4c_alt = mfhi_o_h_outcome_v2 + mfhi_2_2 + fin_debt_repaystress_sellassets_inv + mfhi_2_3,
    fin_fh_cmpscore_5c_alt = mfhi_o_h_outcome_v2 + mfhi_2_2 + fin_debt_repaystress_sellassets_inv + mfhi_2_3 + mfhi_3_2,
    
    fin_fh_cmpscore_3c = scale_values_2(fin_fh_cmpscore_3c, min = 0, max = 3),
    fin_fh_cmpscore_4c = scale_values_2(fin_fh_cmpscore_4c, min = 0, max = 4),
    fin_fh_cmpscore_5c = scale_values_2(fin_fh_cmpscore_5c, min = 0, max = 5),
    
    fin_fh_cmpscore_3c_alt = scale_values_2(fin_fh_cmpscore_3c_alt, min = 0, max = 3),
    fin_fh_cmpscore_4c_alt = scale_values_2(fin_fh_cmpscore_4c_alt, min = 0, max = 4),
    fin_fh_cmpscore_5c_alt = scale_values_2(fin_fh_cmpscore_5c_alt, min = 0, max = 5)
    
  )  %>% dummy_cols(select_columns = c("access_strand"))

# Assets:

data <- data %>%
  mutate(
    
    # habitable rooms per person
    rooms_per_person = U6/NHM,
    rooms_per_person = ifelse(is.na(rooms_per_person), mean(rooms_per_person, na.rm = TRUE), rooms_per_person),
    
    dwelling_type = U4,
    dwelling_type = ifelse(is.na(dwelling_type), round(mean(dwelling_type, na.rm = TRUE), 0), dwelling_type),
    
    toilet_comp = case_when(
      U14a %in% c(1, 2, 3, 9) ~ 3,
      U14a %in% c(4, 5, 6) ~ 2,
      U14a %in% c(7,8) ~ 1,
      is.na(U14a) ~ 1
    ),
    asset_flushtoilet = ifelse(toilet_comp == 3, 1, 0),
    
    # Piped into home or plot/yard or bottled
    water_comp = case_when(
      U13 %in% c(10, 11, 12) ~ 3,
      U13 %in% c(5, 7, 9, 13, 14, 15) ~ 2,
      U13 %in% c(1, 2, 3, 4, 6, 8) ~ 1,
      is.na(U13) ~ 1
    ),
    asset_pipedwater = ifelse(water_comp == 3, 1, 0),
    
    # Finished wall (brick, cement, stone/lime)
    wall_comp = case_when(
      U10 %in% c(11, 12, 13, 16, 17) ~ 3,
      U10 %in% c(5, 6, 7, 10, 14) ~ 2,
      U10 %in% c(1, 2, 3, 4, 8, 9, 15) ~ 1,
      is.na(U10) ~ 1
    ),
    asset_brickwall = ifelse(wall_comp == 3, 1, 0),
    
    # Elecricity main source of lighting:
    lighting_comp = case_when(
      U12 %in% c(1, 7) ~ 3,
      U12 %in% c(2, 3, 4, 5, 11, 12, 13, 8, 9) ~ 2,
      U12 %in% c(6, 10) ~ 1,
      is.na(U12) ~ 1
    ),
    asset_electricity =  ifelse(lighting_comp == 3, 1, 0),
    
    # Electricity, gas or solar cooking fuel
    
    cookingfuel_comp = case_when(
      U11 %in% c(1, 3, 7) ~ 3,
      U11 %in% c(2, 4, 6) ~ 2,
      U11 %in% c(5) ~ 1,
      is.na(U11) ~ 1
    ),
    asset_elcgascookfuel = ifelse(cookingfuel_comp == 3, 1, 0),
    
    asset_radio = ifelse(U16__1 == 1, 1, 0),
    asset_tv = ifelse(U16__2 == 1 | U16__3 == 1 | U16__4 == 1 | U16__5 == 1, 1, 0),
    asset_bicycle = ifelse(U16__9 == 1, 1, 0),
    asset_motorcycle = ifelse(U16__10 == 1, 1, 0),
    asset_car = ifelse(U16__11 == 1, 1, 0),
    asset_fridge = ifelse(U16__13 == 1, 1, 0)
    
  ) %>% dummy_cols(select_columns = c("toilet_comp", "water_comp", "lighting_comp", "wall_comp", "cookingfuel_comp", "dwelling_type"))


# Computing percentage of adults by livelihood ----------------

#cnt(FA2021, probweights, resp_live_group2_fct)

# Creating analysis dataset -----------------------

statvars <- c("hh_id", "mem_id", "resp_all", "strata", "interview__id", "sample_")
vars <- c(statvars, "year", "County", "mfhi_", "ndomains_", "goals_", "hh_", "resp_", "know_", "fin_", "shocks_", "liqdist_", "_comp", "asset_", "rooms_", "dwelling_", "fin_", "fst_", "access_")

# Merging finance for education variable

data <- data %>% select(matches(paste(vars, collapse = "|")))

# data <- data %>% left_join(FA2021_edu, by = "interview__id")  %>%
#   left_join(FA2021_live, by = "interview__id") %>%
#   left_join(FA2021_livebus, by = "interview__id") %>%
#   left_join(FA2021_con, by = "interview__id")  %>%
#   left_join(FA2021_emer, by = "interview__id") %>%
#   left_join(FA2021_dur, by = "interview__id") %>%
#   left_join(FA2021_house, by = "interview__id") %>%
#   mutate(
#     mfhi_conemer_fin = ifelse(mfhi_con_fin == 1 | mfhi_emer_fin == 1, 1, 0),
#     mfhi_investdur_fin = ifelse(mfhi_3_2 == 1 | mfhi_dur_fin == 1, 1, 0)
#   )

pr.out <- prcomp(~ rooms_per_person + toilet_comp_1 + toilet_comp_2 + toilet_comp_3 +
                   water_comp_1 + water_comp_2 + water_comp_3 +
                   lighting_comp_1 + lighting_comp_2 + lighting_comp_3 +
                   cookingfuel_comp_1 + cookingfuel_comp_2 + cookingfuel_comp_3 +
                   wall_comp_1 + wall_comp_2 + wall_comp_3 +
                   dwelling_type_1 + dwelling_type_2 + dwelling_type_3 + dwelling_type_4, scale = TRUE, data = data)

hist(pr.out$x[, 1])

#biplot(pr.out, scale = 0)

data <- data %>% mutate(
  hh_wlth_index = -1*pr.out$x[, 1],
  hh_wlth_decile = cut(hh_wlth_index,
                       breaks=quantile(hh_wlth_index, probs=seq(0, 1, by=0.1), na.rm=TRUE),
                       labels=c("Poorest 10%","Q9","Q8","Q7","Q6", "Q5", "Q4", "Q3", "Q2", "Richest 10%"),
                       include.lowest=TRUE),
  hh_wlth_quintile = cut(hh_wlth_index,
                         breaks=quantile(hh_wlth_index, probs=seq(0,1, by=0.2), na.rm=TRUE),
                         labels=c("Poorest 20%","Q2","Q3","Q4","Richest 20%"),
                         include.lowest=TRUE),
  hh_wlth_group = case_when(
    hh_wlth_quintile %in% c("Poorest 20%","Q2") ~ "Poorest 40%",
    hh_wlth_quintile %in% c("Q3","Q4") ~ "Middle 40%",
    hh_wlth_quintile %in% c("Richest 20%") ~ "Richest 20%"
  ),
  hh_wlth_group = factor(hh_wlth_group, levels = c("Poorest 40%", "Middle 40%", "Richest 20%"), ordered = TRUE),
  hh_wlth_bottom40 = ifelse(hh_wlth_group == "Poorest 40%", 1, 0)
)

return(data)


}
