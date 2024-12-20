


prep_2019 <- function(data) { 

  
# Creating components of multi-dimensional financial health index -----------------------

# Dimension 1: Day to day money management

sav_uses <- c(2, 4:5, 7:9, 14:20, 22:23)
crd_uses <- c(2, 4:9, 10:12, 17:23, 26)
oldage <- c(1, 2, 6)

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
  
  hh_id = paste(a1, a8, a8_1, a8_2, sep = "_"),
  resp_all = "All adults",
  
  # - Dimension 1: Day to day money management
  
  # -- No trouble making money last
  mfhi_1_1 = ifelse(b1b_5 == 2, 1, 0),
  mfhi_1_1 = ifelse(b1b_5 %in% c(98, 99), NA, mfhi_1_1),
  
  # -- Plan for how to spend money
  mfhi_1_2 = ifelse(b1b_3 == 1, 1, 0),
  mfhi_1_2 = ifelse(b1b_3 %in% c(98, 99), NA, mfhi_1_2),
  
  # -- Does not go without food
  mfhi_1_3 = ifelse(b1c1_1 == 2, 1, 0),
  mfhi_1_3 = ifelse(b1c1_1 %in% c(98, 99), NA, mfhi_1_3),
  
  # - Dimension 2: Coping with risk
  
  # -- Does not go without medicine
  mfhi_2_1 = ifelse(b1c1_2 == 2, 1, 0),
  mfhi_2_1 = ifelse(b1c1_2 %in% c(98, 99), NA, mfhi_2_1),
  
  # -- Can access a lump sum (If you needed Ksh 5,500 for rural, Ksh 9,000 for urban in 3 days, would you be able to get it?)
  mfhi_2_2 = ifelse(b1g == 1, 1, 0),
  mfhi_2_2 = ifelse(b1g %in% c(98, 99), NA, mfhi_2_2),
  
  # -- Keeps money aside for an emergency
  mfhi_2_3 = ifelse(b1b_2 == 1, 1, 0),
  mfhi_2_3 = ifelse(b1b_2 %in% c(98, 99), NA, mfhi_2_3),
  
  # - Dimension 3: Investing in the future/livelihood
  
  # -- Keeps money aside for a specific purpose
  mfhi_3_1 = ifelse(b1b_4 == 1, 1, 0),
  mfhi_3_1 = ifelse(b1b_4 %in% c(98, 99), NA, mfhi_3_1),
  
  # Alternative for forward compatibility
  #-- For 2021, using Section 3 meeting goals data
  
  mfhi_3_1_alt = ifelse(s4a == 1 & (s4c_1 %in% c(10, 11, 12, 13, 14, 15) |
                                      s4c_2 %in% c(10, 11, 12, 13, 14, 15) |
                                      s4c_3 %in% c(10, 11, 12, 13, 14, 15) |
                                      s4c_4 %in% c(10, 11, 12, 13, 14, 15) |
                                      s4c_5 %in% c(10, 11, 12, 13, 14, 15)), 1, 0)
  
) %>%
  
  # -- Uses savings or credit for mainly for productive purposes
  
  mutate_at(vars(starts_with("f2_")), ~ ifelse(. %in% sav_uses, 1, 0)) %>%
  mutate(mfhi_3_2_sav = ifelse(rowSums(.[grepl("f2_", names(.))]) > 0, 1, 0)) %>%
  
  mutate_at(vars(matches("e1[a-zA-Z0-9]3")), ~ ifelse(. %in% crd_uses, 1, 0)) %>%
  mutate(mfhi_3_2_crd = ifelse(rowSums(.[grepl("e1[a-zA-Z0-9]3", names(.))]) > 0, 1, 0)) %>%
  
  mutate(mfhi_3_2 = ifelse(mfhi_3_2_sav == 1 | mfhi_3_2_crd == 1, 1, 0)) %>%
  
  # -- Is using or planning to use savings, investment or pension during old age
  
  mutate_at(vars(matches("b1e_")), ~ ifelse(. %in% oldage, 1, 0)) %>%
  mutate(mfhi_3_3 = ifelse(rowSums(.[grepl("b1e_", names(.))]) > 0, 1, 0)) %>%
  
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
  
  # Main indicator that is backwards consistent with 2016
  
  mfhi_1_c_v16 = mfhi_1_1*w + mfhi_1_2*w + mfhi_1_3*w,
  mfhi_2_c_v16 = mfhi_2_1*w + mfhi_2_2*w + mfhi_2_3*w,
  mfhi_3_c_v16 = mfhi_3_1*w + mfhi_3_2*w + mfhi_3_3*w,
  mfhi_o_c_v16 = mfhi_1_c_v16 + mfhi_2_c_v16 + mfhi_3_c_v16,
  
  mfhi_o_h_v16 = ifelse(mfhi_o_c_v16 >= k, 1, 0),
  
  # Alternative specification that excludes problematic indicators (inconsistent over time)
  
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
  
  
  mfhi_o_h_alt5 = mfhi_o_h_v16,
  
  # Alternative specification that only includes outcome indicators:
  
  mfhi_1_c_outcome = mfhi_1_1*w_alt3 + mfhi_1_3*w_alt3,
  mfhi_2_c_outcome = mfhi_2_1*w_alt3,
  mfhi_o_c_outcome = mfhi_1_c_outcome + mfhi_2_c_outcome,
  mfhi_o_h_outcome = ifelse(mfhi_o_c_outcome >= k, 1, 0),
  
  mfhi_o_h_outcome_v2 = ifelse(mfhi_2_1 == 1 & mfhi_1_3 == 1, 1, 0),
  mfhi_o_h_outcome_v2 = ifelse(is.na(mfhi_o_h_outcome_v2), 0, mfhi_o_h_outcome_v2),
  
  # liquidity distress:
  
  liqdist_any = ifelse(s2a == 1, 1, 0),
  liqdist_any = ifelse(s2a %in% c(98, 99), NA, liqdist_any),
  
  goals_group = case_when(
    b1_1 == 1 ~ "food",
    b1_1 == 2 ~ "educ",
    b1_1 %in% c(3) ~ "live",
    b1_1 %in% c(4, 5, 6, 7) ~ "other"
  ),
  goals_group_fct = factor(goals_levels[goals_group], goals_levels, ordered = TRUE),
  
  
  # -- Does not go without medicine
  mfhi_senthome = ifelse(b1c1_4 == 1, 1, 0),
  mfhi_senthome = ifelse(b1c1_4 %in% c(98, 99), NA, mfhi_senthome),
  
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
  
  
) %>% dummy_cols(select_columns = c('goals_group')) %>%
  
  # Shocks
  mutate(
    # Has respondent experience any of the following adverse shocks in past year:
    shocks_illness = case_when(s3a_1 == 1 ~ 1, s3a_1 == 2 ~ 0, s3a_1 == 98 ~ NA_real_),
    shocks_climate = case_when(s3a_2 == 1 ~ 1, s3a_2 == 2 ~ 0, s3a_2 == 98 ~ NA_real_),
    shocks_deathinc = case_when(s3a_3 == 1 ~ 1, s3a_3 == 2 ~ 0, s3a_3 == 98 ~ NA_real_),
    shocks_deathoth = case_when(s3a_4 == 1 ~ 1, s3a_4 == 2 ~ 0, s3a_4 == 98 ~ NA_real_),
    shocks_asset = case_when(s3a_5 == 1 ~ 1, s3a_5 == 2 ~ 0, s3a_5 == 98 ~ NA_real_),
    shocks_other = case_when(s3a_7 == 1 ~ 1, s3a_7 == 2 ~ 0, s3a_7 == 98 ~ NA_real_),
    shocks_N = shocks_illness + shocks_climate + shocks_deathinc + shocks_deathoth + shocks_asset + shocks_other,
    shocks_health = ifelse(shocks_illness == 1 | shocks_deathinc == 1 | shocks_deathoth == 1, 1, 0),
    shocks_health = ifelse(is.na(shocks_illness) & is.na(shocks_deathinc) & is.na(shocks_deathoth), NA, shocks_health),
    shocks_deathany = ifelse(shocks_deathinc == 1 | shocks_deathoth == 1, 1, 0),
    shocks_deathany = ifelse(is.na(shocks_deathinc) & is.na(shocks_deathoth), NA, shocks_deathany),
    shocks_any = ifelse(shocks_illness == 1 | shocks_climate == 1 | shocks_deathinc == 1 | shocks_deathoth == 1 | shocks_asset == 1 | shocks_other == 1, 1, 0),
    shocks_any = ifelse(is.na(shocks_illness) & is.na(shocks_climate) & is.na(shocks_deathinc) & is.na(shocks_deathoth) & is.na(shocks_asset) & is.na(shocks_other), NA, shocks_any)
  )


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

data <- data %>%
  mutate(
    sample_psu = a8_1,
    sample_weights = Pop_Wt_r,
    hh_urbrur = ifelse(cluster_type == 1, "Rural", "Urban"),
    hh_geo = case_when(
      hh_urbrur == "Rural" & (a1 %in% ASAL_counties_FA) ~ "rur_asal",
      a1 == 47 ~ "urb_nbo",
      hh_urbrur == "Urban" ~ "urb_oth",
      hh_urbrur == "Rural" ~ "rur_oth"
    ),
    hh_geo_2 = case_when(
      a1 == 47 ~ "urb_nbo",
      hh_urbrur == "Urban" ~ "urb_oth",
      hh_urbrur == "Rural" ~ "rur"
    ),
    hh_geo_fct = factor(geo_levels[hh_geo], levels = geo_levels, ordered = TRUE),
    hh_geo2_fct = factor(geo2_levels[hh_geo_2], levels = geo2_levels, ordered = TRUE),
    hh_size_all = a10,
    hh_size_all_c = hh_size_all - mean(hh_size_all),
    hh_urbrur = case_when(
      hh_urbrur == "Urban" ~ "urb",
      hh_urbrur == "Rural" ~ "rur"
    )
    #hh_type_2_fct = factor(hhtype2_levels[hh_type_2], levels = hhtype2_levels),
  ) %>% dummy_cols(select_columns = c("hh_urbrur", "hh_geo"))

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
    fin_status_impr = ifelse(b1f == 1, 1, 0),
    fin_status_worse = ifelse(b1f == 3, 1, 0),
    
    fin_status_fct = ifelse(fin_status_impr == 1, "Financial status: Improved", "Financial status: Worsened"),
    fin_status_fct = ifelse(is.na(fin_status_impr) & is.na(fin_status_worse), NA, fin_status_fct),
    
    resp_gender_group = ifelse(gender == 1, "men", "wmn"),
    resp_gender_fct = factor(gender_levels[resp_gender_group], gender_levels, ordered = TRUE),
    
    resp_age_yrs = a13,
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
      a17 == 1 ~ "nmr",
      a17 %in% c(2,3)  ~ "div",
      a17 == 4  ~ "mar"
    ),
    resp_marstat_fct = factor(marstat_levels[resp_marstat], levels = marstat_levels, ordered = TRUE),
    resp_edu_group = case_when(
      a16 == 1 ~ "none",
      a16 == 2 ~ "smpr",
      a16 == 3 ~ "cmpr",
      a16 == 4 ~ "smsc",
      a16 == 5 ~ "cmsc",
      a16 %in% c(6, 7, 8, 9) ~ "smtr"
    ),
    resp_edu_group_fct = factor(edu_levels[resp_edu_group], levels = edu_levels, ordered = TRUE),
    resp_edu_group2 = case_when(
      a16 == 1 ~ "none",
      a16 %in% c(2, 3) ~ "pri",
      a16 %in% c(4, 5) ~ "sec",
      a16 %in% c(6, 7, 8, 9) ~ "trt",
    ),
    resp_edu_group2_fct = factor(edu_levels2[resp_edu_group2], levels = edu_levels2, ordered = TRUE),
    
    resp_edu_group3 = case_when(
      a16 %in% c(1,2,3) ~ "prinone",
      a16 %in% c(4, 5) ~ "sec",
      a16 %in% c(6, 7, 8, 9) ~ "trt",
      a16 %in% c(98,99) ~ NA
    ),
    resp_edu_group3_fct = factor(edu_levels3[resp_edu_group3], levels = edu_levels3, ordered = TRUE),
    
    resp_live_group = case_when(
      b3b == 1 ~ "farm",
      b3b == 2 ~ "empl",
      b3b == 3 ~ "cwrk",
      b3b == 4 ~ "owbs",
      b3b %in% c(5, 8, 9) ~ "trns",
      b3b %in% c(6, 7, 10) ~ "othr"
    ),
    resp_live_group_fct = factor(live_levels[resp_live_group], levels = live_levels, ordered = TRUE),
    
    resp_live_group2 = case_when(
      b3b == 1 ~ "farm",
      b3b == 2 ~ "empl",
      b3b == 3 ~ "cwrk",
      b3b == 4 ~ "owbs",
      b3b %in% c(5, 8, 9) ~ "trns",
      b3b %in% c(6, 7) ~ "inv",
      b3b == 10 ~ "othr"
    ),
    resp_live_group2_fct = factor(live_levels2[resp_live_group2], levels = live_levels2, ordered = TRUE),
    
    # Incom from any source
    resp_live_agri = ifelse(b3a_1 == 1 | b3a_2 == 1 | b3a_3 == 1 | b3a_4 == 1 | b3a_5 == 1, 1, 0),
    resp_live_agri = ifelse(is.na(resp_live_agri), 0, resp_live_agri),
    resp_live_emp =  ifelse(b3a_1 == 2 | b3a_2 == 2 | b3a_3 == 2 | b3a_4 == 2 | b3a_5 == 2, 1, 0),
    resp_live_emp = ifelse(is.na(resp_live_emp), 0, resp_live_emp),
    resp_live_cwrk =  ifelse(b3a_1 == 3 | b3a_2 == 3 | b3a_3 == 3 | b3a_4 == 3 | b3a_5 == 3, 1, 0),
    resp_live_cwrk = ifelse(is.na(resp_live_cwrk), 0, resp_live_cwrk),
    resp_live_owbs =  ifelse(b3a_1 == 4 | b3a_2 == 4 | b3a_3 == 4 | b3a_4 == 4 | b3a_5 == 4, 1, 0),
    resp_live_owbs = ifelse(is.na(resp_live_owbs), 0, resp_live_owbs),
    resp_live_inv =  ifelse(b3a_1 %in% c(6,7) | b3a_2 %in% c(6,7) | b3a_3 %in% c(6,7) | b3a_4 %in% c(6,7) | b3a_5 %in% c(6,7), 1, 0),
    resp_live_inv = ifelse(is.na(resp_live_inv), 0, resp_live_inv),
    resp_live_trnsf =  ifelse(b3a_1 %in% c(5, 8, 9) | b3a_2 %in% c(5, 8, 9) | b3a_3 %in% c(5, 8, 9) | b3a_4 %in% c(5, 8, 9) | b3a_5 %in% c(5, 8, 9), 1, 0),
    resp_live_trnsf = ifelse(is.na(resp_live_trnsf), 0, resp_live_trnsf),
    resp_live_other = ifelse(b3a_1 == 10 | b3a_2 == 10 | b3a_3 == 10 | b3a_4 == 10 | b3a_5 == 10, 1, 0),
    resp_live_other = ifelse(is.na(resp_live_other), 0, resp_live_other),
    
    resp_live_trnsf_sn =  ifelse(b3a_1 %in% c(9) | b3a_2 %in% c(9) | b3a_3 %in% c(9) | b3a_4 %in% c(9) | b3a_5 %in% c(9), 1, 0),
    resp_live_trnsf_sn = ifelse(is.na(resp_live_trnsf_sn), 0, resp_live_trnsf_sn),
    
    resp_live_trnsf_in =  ifelse(b3a_1 %in% c(5, 8) | b3a_2 %in% c(5, 8) | b3a_3 %in% c(5, 8) | b3a_4 %in% c(5, 8) | b3a_5 %in% c(5, 8), 1, 0),
    resp_live_trnsf_in = ifelse(is.na(resp_live_trnsf_in), 0, resp_live_trnsf_in),
    
    resp_incsource_N = resp_live_agri + resp_live_emp + resp_live_cwrk + resp_live_owbs + resp_live_inv + resp_live_trnsf + resp_live_other,
    
    resp_live_agri_sec = ifelse(resp_live_agri == 1 & b3b != 1, 1, 0),
    resp_live_emp_sec = ifelse(resp_live_emp == 1 & b3b != 2, 1, 0),
    resp_live_cwrk_sec = ifelse(resp_live_cwrk == 1 & b3b != 3, 1, 0),
    resp_live_owbs_sec = ifelse(resp_live_owbs == 1 & b3b != 4, 1, 0),
    resp_live_trnsf_sec = ifelse(resp_live_trnsf == 1 & b3b %not_in% c(5, 8, 9), 1, 0),
    resp_live_inv_sec = ifelse(resp_live_inv == 1 & b3b %not_in% c(6, 7), 1, 0),
    resp_live_other_sec = ifelse(resp_live_other == 1 & b3b != 10, 1, 0),
    
    resp_live_agricon = ifelse(b3c == 1, 1, 0),
    
    resp_live_farm_any = ifelse(resp_live_agri == 1 | resp_live_agricon == 1, 1, 0),
    
    resp_live_informal_any = ifelse(resp_live_agri == 1 | resp_live_cwrk == 1 | resp_live_owbs == 1, 1, 0),
    resp_live_informal_main = ifelse(b3b %in% c(1,3,4), 1, 0),
    resp_live_informal_sec = ifelse(resp_live_agri_sec == 1 | resp_live_cwrk_sec == 1 | resp_live_owbs_sec == 1, 1, 0),
    
    resp_live_sec_any = ifelse(resp_live_agri_sec == 1 | resp_live_emp_sec == 1 | resp_live_cwrk_sec == 1 | resp_live_owbs_sec == 1 | resp_live_trnsf_sec == 1 | resp_live_other_sec == 1, 1, 0),
    
    x = case_when(
      b3h == 1 ~ 100/2,
      b3h == 2 ~ (1500 - 100)/2,
      b3h == 3 ~ (3000 - 1500)/2,
      b3h == 4 ~ (7500 - 3000)/2,
      b3h == 5 ~ (15000 - 7500)/2,
      b3h == 6 ~ (30000 - 15000)/2,
      b3h == 7 ~ (70000 - 30000)/2,
      b3h == 8 ~ (200000 - 70000)/2,
      b3h == 9 ~ (400000 - 200000)/2,
      b3h == 10 ~ (1000000 - 400000)/2,
      b3h == 11 ~ 1000000
    ),
    resp_income = b3h1,
    resp_income = ifelse(is.na(resp_income), x, resp_income),
    
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
    
    resp_ppi_pred_npl = probability_pov_npl,
    resp_ppi_decile_npl = cut(probability_pov_npl,
                              breaks=quantile(probability_pov_npl, probs=seq(0,1, by=0.1), na.rm=TRUE),
                              labels=c("Richest 10%","Q9","Q8","Q7","Q6", "Q5", "Q4", "Q3", "Q2", "Poorest 10%"),
                              include.lowest=TRUE),
    resp_ppi_group_npl = ifelse(resp_ppi_decile_npl %in% c("Q4", "Q3", "Q2", "Poorest 10%"), "Bottom 40%",
                                ifelse(resp_ppi_decile_npl %in% c("Q8","Q7","Q6", "Q5"), "Middle 40%", "Top 20%")),
    
    # Has social support
    resp_socialfinsupport = ifelse(b1b_1 == 1, 1, 0),
    
    
  ) %>% dummy_cols(select_columns = c("resp_age_group2", "resp_gender_group", "resp_age_group", "resp_marstat", "resp_edu_group", "resp_edu_group2", "resp_live_group", "resp_live_group2", "resp_inc_group"))

# Behavioral variables
# Financial numeracy
# Digital numeracy

data <- data %>%
  mutate(
    know_fin_numeracy = ifelse(b2g == 1, 1, 0),
    know_dig_literacy = ifelse(b2h == 1, 1, 0 )
  )

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
#   ) %>% ungroup() -> data


# Creating financial service usage variables -----------------------

clean_NAs <- function(x) { ifelse(is.na(x), 99, x) }

data <- data %>%
  mutate_at(vars(starts_with("c1")), clean_NAs) %>%
  mutate_at(vars(starts_with("e2b")), clean_NAs)

data <- data %>%
  mutate(
    
    # Account ownership/ informal device usage
    fin_account_mm = ifelse(c1a3 == 1 | c1a9 == 1, 1, 0),
    fin_account_bank = ifelse(c1a28 == 1  | c1a32 == 1 |
                                c1a11 == 1 | c1a33 == 1 | c1a31 == 1 |
                                c1a29 == 1 | c1a27 == 1 | c1a30 == 1 | c1a12 == 1 | c1a2 == 1 | c1a10 == 1, 1, 0),
    
    fin_account_tbank = ifelse(c1a28 == 1  | c1a32 == 1 |
                                 c1a11 == 1 | c1a33 == 1 | c1a31 == 1 |
                                 c1a29 == 1 | c1a27 == 1 | c1a30 == 1, 1, 0),
    
    fin_account_nbfi = ifelse(c1a13 == 1 | c1a14 == 1 | c1a1 == 1 | c1a4 == 1, 1, 0),
    fin_account_tbanknbfi = ifelse(fin_account_tbank == 1 | fin_account_nbfi == 1, 1, 0),
    fin_account_any = ifelse(fin_account_mm == 1 | fin_account_bank == 1 | fin_account_nbfi == 1, 1, 0),
    fin_chama_user = ifelse(c1a16 == 1 | c1a5 == 1, 1, 0),
    fin_socialnetwork_user = ifelse(c1a15 == 1 | c1a18 == 1 | c1a19 == 1 | c1a20  == 1 | c1a21 == 1 | c1a23 == 1 | c1a6 == 1 | c1a7 == 1, 1, 0),
    fin_socialnetwork_user_ff = ifelse(c1a6 == 1 | c1a7 == 1 | c1a19 == 1, 1, 0),
    
    fin_cash_user = ifelse(c1a8 == 1, 1, 0),
    
    fin_digital_user = ifelse(fin_account_mm == 1 | c1a2 == 1 | c1a10 == 1 | c1a12 == 1 | c1a22 == 1, 1, 0),
    
    fin_digital_mm = ifelse(c1a9 == 1, 1, 0),
    fin_digital_mb = ifelse(c1a10 == 1, 1, 0),
    fin_digital_app = ifelse(c1a22 == 1, 1, 0),
    
    # Savings:
    
    fst_savings_tbank = ifelse(c1a27 == 1 | c1a28 == 1 | c1a29 == 1 | c1a30 == 1, 1, 0),
    fst_savings_nbfi = ifelse(c1a1 == 1 | c1a4 == 1, 1, 0),
    fst_savings_mm = ifelse(c1a3 == 1, 1, 0),
    fst_savings_mbank = ifelse(c1a2 == 1, 1, 0),
    fst_savings_cash = ifelse(c1a8 == 1, 1, 0),
    fst_savings_network = ifelse(c1a7 == 1 | c1a6 == 1, 1, 0),
    fst_savings_sg = ifelse(c1a5 == 1, 1, 0),
    fst_savings_social = ifelse(fst_savings_sg == 1 | fst_savings_network == 1, 1, 0),
    
    fst_savings_any = ifelse(fst_savings_tbank == 1 | fst_savings_nbfi == 1 | fst_savings_mm == 1 | fst_savings_mbank == 1 | fst_savings_cash == 1 | fst_savings_network == 1 | fst_savings_sg == 1, 1, 0),
    
    fst_savings_informal_any = ifelse(fst_savings_cash == 1 | fst_savings_network == 1 | fst_savings_sg == 1, 1, 0),
    fst_savings_formaloth_any = ifelse(fst_savings_tbank == 1 | fst_savings_mbank == 1 | fst_savings_nbfi == 1, 1, 0),
    
    #Debt
    fin_debt_total = rowSums(.[grepl("e1[a-zA-Z0-9]10", names(.))], na.rm = T),
    fin_debt_any = ifelse(fin_debt_total > 0, 1, 0),
    fin_debt_inc = fin_debt_total/(ifelse(resp_income == 0, 1, resp_income)),
    fin_debt_high = ifelse(fin_debt_inc > 0.5, 1, 0),
    
    fin_debt_repaystress = ifelse(e2b1 == 1 | e2b3 == 1 | e2b4 == 1, 1, 0),
    fin_debt_repaystress_noborr = ifelse(e2b3 == 1 | e2b4 == 1, 1, 0),
    fin_debt_repaystress_sellassets = ifelse(e2b3 == 1, 1, 0),
    fin_debt_repaystress_sellassets_inv = ifelse(fin_debt_repaystress_sellassets == 1, 0, 1),
    fin_debt_default = ifelse(e2c > 0, 1, 0),
    fin_debt_default= ifelse(is.na(e2c), 0, fin_debt_default),
    fin_debt_stressany = ifelse(fin_debt_repaystress == 1 | fin_debt_default == 1, 1, 0),
    fin_debt_stressany_inv = ifelse(fin_debt_stressany == 1, 0, 1),
    
    fin_digital_borrower = ifelse((c1a12 == 1 | c1a22 == 1 | c2_12 == 1 | c2_22 == 1), 1, 0),
    fin_digital_borrower = ifelse(is.na(fin_digital_borrower), 0, fin_digital_borrower),
    
    fst_loans_fint = ifelse(c1a11 == 1 | c1a13 == 1| c1a14 == 1, 1, 0),
    fst_loans_mbank = ifelse(c1a12 == 1 | c1a22 == 1, 1, 0),
    fst_loans_shopcredit = ifelse(c1a20  == 1 | c1a21 == 1 , 1, 0),
    fst_loans_sg = ifelse(c1a16 == 1, 1, 0),
    fst_loans_family = ifelse(c1a19 == 1, 1, 0),
    fst_loans_fuliza = 0,
    
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
    rooms_per_person = y13/a10,
    rooms_per_person = ifelse(is.na(rooms_per_person), mean(rooms_per_person, na.rm = TRUE), rooms_per_person),
    
    dwelling_type = y4,
    
    toilet_comp = case_when(
      y11 %in% c(1) ~ 3,
      y11 %in% c(2, 3, 4) ~ 2,
      y11 %in% c(5, 6, 7, 99) ~ 1
    ),
    asset_flushtoilet = ifelse(toilet_comp == 3, 1, 0),
    
    # Piped into home or plot/yard or bottled
    water_comp = case_when(
      y10 %in% c(1, 2, 11, 16) ~ 3,
      y10 %in% c(3, 4, 5, 6, 7, 10) ~ 2,
      y10 %in% c(8, 9, 12, 13, 14, 15, 17, 18, 99) ~ 1
    ),
    asset_pipedwater = ifelse(water_comp == 3, 1, 0),
    
    # Finished wall (brick, cement, stone/lime)
    wall_comp = case_when(
      y7 %in% c(1) ~ 3,
      y7 %in% c(2) ~ 2,
      y7 %in% c(3) ~ 1
    ),
    asset_brickwall = ifelse(wall_comp == 3, 1, 0),
    
    # Elecricity main source of lighting:
    lighting_comp = case_when(
      y9 %in% c(5, 6) ~ 3,
      y9 %in% c(4, 7, 8, 9, 10, 11, 12, 13) ~ 2,
      y9 %in% c(1, 2, 3, 14, 15, 99) ~ 1
    ),
    asset_electricity =  ifelse(lighting_comp == 3, 1, 0),
    
    # Electricity, gas or solar cooking fuel
    
    cookingfuel_comp = case_when(
      y8 %in% c(5, 6) ~ 3,
      y8 %in% c(4, 7, 9, 10, 11, 13) ~ 2,
      y8 %in% c(1, 2, 3, 8, 99) ~ 1
    ),
    asset_elcgascookfuel = ifelse(cookingfuel_comp == 3, 1, 0)
    
  ) %>% dummy_cols(select_columns = c("toilet_comp", "water_comp", "lighting_comp", "wall_comp", "cookingfuel_comp", "dwelling_type"))


# Computing percentage of adults by livelihood ----------------

#cnt(data, probweights, resp_live_group2_fct)

# Creating analysis dataset -----------------------


statvars <- c("resp_all", "strata", "sample_")
vars <- c(statvars, "year", "mfhi_", "ndomains_", "goals_", "hh_", "resp_", "know_", "fin_", "shocks_", "liqdist_", "_comp", "asset_", "rooms_", "dwelling_", "fst_", "access_")

data <- data %>% select(matches(paste(vars, collapse = "|")))

# Computing household wealth index

pr.out <- prcomp(~ rooms_per_person + toilet_comp_1 + toilet_comp_2 + toilet_comp_3 +
                   water_comp_1 + water_comp_2 + water_comp_3 +
                   lighting_comp_1 + lighting_comp_2 + lighting_comp_3 +
                   cookingfuel_comp_1 + cookingfuel_comp_2 + cookingfuel_comp_3 +
                   wall_comp_1 + wall_comp_2 + wall_comp_3 +
                   dwelling_type_1 + dwelling_type_2 + dwelling_type_3 + dwelling_type_4, scale = TRUE, data = data)

hist(pr.out$x[, 1])

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


# Merging in finance for education variable

# data <- data %>% left_join(FA2018_edu, by = "hh_id") %>%
#   left_join(FA2018_live, by = "hh_id") %>%
#   left_join(FA2018_livebus, by = "hh_id") %>%
#   left_join(FA2018_con, by = "hh_id") %>%
#   left_join(FA2018_emer, by = "hh_id") %>%
#   left_join(FA2018_dur, by = "hh_id") %>%
#   left_join(FA2018_house, by = "hh_id") %>%
#   mutate(
#     mfhi_conemer_fin = ifelse(mfhi_con_fin == 1 | mfhi_emer_fin == 1, 1, 0),
#     mfhi_investdur_fin = ifelse(mfhi_3_2 == 1 | mfhi_dur_fin == 1, 1, 0)
#   )

}
