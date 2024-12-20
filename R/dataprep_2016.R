

prep_2016 <- function(data) { 
  
# MFHI parameters:
w <- 1/9
k <- 0.6

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

data <- data %>%
  
  rename(
    hh_size_all = hhsize, 
    resp_age_yrs = age,
    
    # -- No trouble making money last
    mfhi_1_1 =  fh131_moneylast,
    
    # -- Plan for how to spend money
    mfhi_1_2 = fh121_moneyplan,
    
    # -- Does not go without food
    mfhi_1_3 = fh134_nofood,
    
    # - Dimension 2: Coping with risk
    
    # -- Does not go without medicine
    mfhi_2_1 = fh135_nomeds,
    
    # -- Can access a lump sum (If you needed Ksh 5,500 for rural, Ksh 9,000 for urban in 3 days, would you be able to get it?)
    mfhi_2_2 = fh211_raisefndin3,
    
    # -- Keeps money aside for an emergency
    mfhi_2_3 = fh221_emergencyfnd,
    
    # - Dimension 3: Investing in the future/livelihood
    
    # -- Keeps money aside for a specific purpose
    mfhi_3_1 = fh321_regsavfuture,
    
    # -- Uses savings or credit for mainly for productive purposes (v2 of this variable uses multiple responses)
    mfhi_3_2 = fh326_fintoinvest_v2,
    
    # Uses savings or loans for education
    mfhi_edu_fin = fh326_finedu,
    # Uses savings or loans for livelihood
    mfhi_live_fin = fh326_finlive,
    # Uses savings or loans for emergencies
    mfhi_emer_fin = fh326_finemer,
    # Uses savings or laons for consumption
    mfhi_con_fin = fh326_fincon,
    # Uses savings or laons for consumer durables
    mfhi_dur_fin = fh326_findur,
    # Uses savings or laons for housing
    mfhi_house_fin = fh326_finhouse,
    
    # Child sent
    mfhi_senthome = fh136_chldsnthme,
    
    sav_invest = fh324_savingsforinvest_v2,
    sav_edu = fh324_savingsforedu,
    sav_live = fh324_savingsforlive,
    sav_con = fh324_savingsforcon,
    sav_emer = fh324_savingsforemer,
    sav_dur = fh324_savingsfordur,
    sav_house = fh324_savingsforhouse,
    
    crd_invest = fh325_creditforinvest,
    crd_edu =  fh325_creditforedu,
    crd_live = fh325_creditforlive,
    crd_con = fh325_creditforcon,
    crd_emer = fh325_creditforemer,
    crd_house = fh325_creditforhouse,
    crd_dur = fh325_creditfordur,
    
    # -- Is using or planning to use savings, investment or pension during old age
    mfhi_3_3 = fh313_oldagefin
    
  ) %>%
  
  mutate(
    
    sample_psu = clusters,
    sample_weights = probweights, 
    resp_all = "All adults",
    
    mfhi_1_c = mfhi_1_1*w + mfhi_1_2*w + mfhi_1_3*w,
    mfhi_2_c = mfhi_2_1*w + mfhi_2_2*w + mfhi_2_3*w,
    mfhi_3_c = mfhi_3_1*w + mfhi_3_2*w + mfhi_3_3*w,
    mfhi_o_c = mfhi_1_c + mfhi_2_c + mfhi_3_c,
    
    # Head counts
    mfhi_1_h = ifelse(mfhi_1_c >= k/3, 1, 0),
    mfhi_2_h = ifelse(mfhi_2_c >= k/3, 1, 0),
    mfhi_3_h = ifelse(mfhi_3_c >= k/3, 1, 0),
    
    mfhi_o_h = ifelse(mfhi_o_c >= k, 1, 0),
    mfhi_o_h_v16 = mfhi_o_h,
    
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
    
    mfhi_3_1_dk = ifelse(mfhi_o_c >= k, mfhi_3_1, 0),
    mfhi_3_2_dk = ifelse(mfhi_o_c >= k, mfhi_3_2, 0),
    mfhi_3_3_dk = ifelse(mfhi_o_c >= k, mfhi_3_3, 0),
    
    mfhi_o_ck = ifelse(mfhi_o_c >= k, mfhi_o_c, 0),
    
    mfhi_o_a = ifelse(mfhi_o_c >= k, mfhi_o_ck, NA),
    
    mfhi_conemer_fin = ifelse(mfhi_con_fin == 1 | mfhi_emer_fin == 1, 1, 0),
    mfhi_investdur_fin = ifelse(mfhi_3_2 == 1 | mfhi_dur_fin == 1, 1, 0),
    
    # Number of domains represented if healthy
    domain1 = ifelse(mfhi_1_1 == 1 | mfhi_1_2 == 1 | mfhi_1_3 == 1, 1, 0),
    domain2 = ifelse(mfhi_2_1 == 1 | mfhi_2_2 == 1 | mfhi_2_3 == 1, 1, 0),
    domain3 = ifelse(mfhi_3_1 == 1 | mfhi_3_2 == 1 | mfhi_3_3 == 1, 1, 0),
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
    
    # Alternative specification that excludes problematic indicators (inconsistent over time)
    
    mfhi_1_c_alt = mfhi_1_1*w_alt + mfhi_1_2*w_alt + mfhi_1_3*w_alt,
    mfhi_2_c_alt = mfhi_2_1*w_alt + mfhi_2_3*w_alt,
    mfhi_3_c_alt = mfhi_3_2*w_alt + mfhi_3_3*w_alt,
    mfhi_o_c_alt = mfhi_1_c_alt + mfhi_2_c_alt + mfhi_3_c_alt,
    
    mfhi_o_h_alt2 = ifelse(mfhi_o_c_alt >= k, 1, 0),
    
    # Alternative specification that excludes lumpsum indicator but includes intentional savings
    
    mfhi_1_c_alt2 = mfhi_1_1*w_alt2 + mfhi_1_2*w_alt2 + mfhi_1_3*w_alt2,
    mfhi_2_c_alt2 = mfhi_2_1*w_alt2 + mfhi_2_3*w_alt2,
    mfhi_3_c_alt2 = mfhi_3_1*w_alt2 + mfhi_3_2*w_alt2 + mfhi_3_3*w_alt2,
    mfhi_o_c_alt2 = mfhi_1_c_alt2 + mfhi_2_c_alt2 + mfhi_3_c_alt2,
    
    mfhi_o_h_alt3 = ifelse(mfhi_o_c_alt2 >= k, 1, 0),
    
    # Alternative specification that includes lumpsum indicator but includes intentional savings
    
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
    
    # Numeracy
    know_fin_numeracy = correctpercent,
    
    # Goals
    
    goals_group = case_when(
      goals == 1 ~ "food",
      goals == 4 ~ "educ",
      goals %in% c(5) ~ "live",
      goals %in% c(2, 3, 6, 7, 8, 9) ~ "other"
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
    
  )  %>% dummy_cols(select_columns = c('goals_group'))

live_levels <- c("farm" = "Agriculture",
                 "empl" = "Employed",
                 "cwrk" = "Casual",
                 "owbs" = "Own business",
                 "trns" = "Transfers",
                 "othr" = "Other")

gender_levels <- c("men" = "Men", "wmn" = "Women")

age_levels <- c("age_18_25" = "[18-25)",
                "age_25_45" = "[25-45)",
                "age_45_65" = "[45-65)",
                "age_65" = "[65+]")

age2_levels <- c("age_18_25" = "[18-25)",
                 "age_25_65" = "[25-65)",
                 "age_65" = "[65+]")

edu_levels2 <- c("none" = "No formal education",
                 "pri" = "Some or complete primary",
                 "sec" = "Some or complete secondary",
                 "trt" = "Some or complete tertiary")

edu_levels3 <- c("prinone" = "Less then complete primary",
                 "sec" = "Some or complete secondary",
                 "trt" = "Some or complete tertiary")

live_levels2 <- c("farm" = "Farming",
                  "empl" = "Employment",
                  "cwrk" = "Casual work",
                  "owbs" = "Own business",
                  "trns" = "Transfers",
                  "inv" = "Investment",
                  "othr" = "Other")

inc_levels <- c("inc_1" = "< KSh 2,500",
                "inc_2" = "KSh 2,500 - 5,000",
                "inc_3" = "KSh 5,000 - 10,000",
                "inc_4" = "KSh 10,000 - 15,000",
                "inc_5" = "KSh 15,000+")

marstat_levels <- c("nmr" = "Never married",
                    "div" = "Separated, divorced or widowed",
                    "mar" = "Married")

data <- data %>%
  rename(
    hh_geo2_fct = geogroup,
    hh_urbrur = urbrurgroup,
    resp_age_group_fct = agegroup,
    resp_income = total_income
  ) %>%
  mutate(
    hh_size_all_c = hh_size_all - mean(hh_size_all),
    resp_marstat = case_when(
      marstat == 1 ~ "nmr",
      marstat %in% c(2,3)  ~ "div",
      marstat == 4  ~ "mar"
    ),
    resp_marstat_fct = factor(marstat_levels[resp_marstat], levels = marstat_levels, ordered = TRUE),
    
    resp_age_group2 = case_when(
      resp_age_group_fct == "[18-25)" ~ "age_18_25",
      resp_age_group_fct %in% c("[25-45)", "[45-65)") ~ "age_25_65",
      resp_age_group_fct == "[65+]" ~ "age_65"
    ),
    resp_age_group2_fct = factor(age2_levels[resp_age_group2], levels = age2_levels, ordered = TRUE),
    
    resp_gender_group = ifelse(gendergroup == "Male", "men", "wmn"),
    resp_gender_fct = factor(gender_levels[resp_gender_group], gender_levels, ordered = TRUE),
    
    resp_edu_group2 = case_when(
      edugroup2 == "None" ~ "none",
      edugroup2 %in% "Primary" ~ "pri",
      edugroup2 %in% "Secondary" ~ "sec",
      edugroup2 %in% "Tertiary" ~ "trt",
    ),
    resp_edu_group2_fct = factor(edu_levels2[resp_edu_group2], levels = edu_levels2, ordered = TRUE),
    
    resp_edu_group3 = case_when(
      edugroup2 %in% c("None", "Primary") ~ "prinone",
      edugroup2 %in% "Secondary" ~ "sec",
      edugroup2 %in% "Tertiary" ~ "trt",
    ),
    resp_edu_group3_fct = factor(edu_levels3[resp_edu_group3], levels = edu_levels3, ordered = TRUE),
    
    resp_live_group = case_when(
      livegroup2 == "Agriculture" ~ "farm",
      livegroup2 == "Employed" ~ "empl",
      livegroup2 == "Casual" ~ "cwrk",
      livegroup2 == "Own business" ~ "owbs",
      livegroup2 == "Dependent" ~ "trns",
      livegroup2 == "Other" ~ "othr"
    ),
    resp_live_group_fct = factor(live_levels[resp_live_group], levels = live_levels, ordered = TRUE),
    
    resp_live_group2 = case_when(
      livegroup6 == "Agriculture" ~ "farm",
      livegroup6 == "Employed" ~ "empl",
      livegroup6 == "Casual" ~ "cwrk",
      livegroup6 == "Own business" ~ "owbs",
      livegroup6 == "Dependent" ~ "trns",
      livegroup6 == "Investments" ~ "inv",
      livegroup6 == "Other" ~ "othr"
    ),
    resp_live_group2_fct = factor(live_levels2[resp_live_group2], levels = live_levels2, ordered = TRUE),
    
    resp_income_lt_ag = ifelse(resp_income < 7284, 1, 0),
    resp_income_lt_ag = ifelse(hh_geo2_fct == "Rural", resp_income_lt_ag, NA),
    
    resp_income_lt_nbo = ifelse(resp_income < 17200, 1, 0),
    resp_income_lt_nbo = ifelse(hh_geo2_fct == "Nairobi", resp_income_lt_nbo, NA),
    
    resp_income_lt_our = ifelse(resp_income < 13593, 1, 0),
    resp_income_lt_our = ifelse(hh_geo2_fct == "Non-Nairobi Urban", resp_income_lt_our, NA),
    
    resp_income_lt_mw = ifelse(hh_geo2_fct == "Nairobi", resp_income_lt_nbo,
                               ifelse(hh_geo2_fct == "Rural", resp_income_lt_ag, resp_income_lt_our)),
    
    resp_incsource_N = resp_live_agri + resp_live_emp + resp_live_cwrk + resp_live_owbs + resp_live_inv + resp_live_trnsf + resp_live_other,
    
    resp_inc_group = case_when(
      resp_income < 2500 ~ "inc_1",
      resp_income < 5000 ~ "inc_2",
      resp_income < 10000 ~ "inc_3",
      resp_income < 15000 ~ "inc_4",
      resp_income >= 15000 ~ "inc_5"
    ),
    resp_inc_group_fct = factor(inc_levels[resp_inc_group], levels = inc_levels, ordered = TRUE),
    
    resp_income_quintile = cut(resp_income,
                               breaks=quantile(resp_income, probs=seq(0,1, by=0.2), na.rm=TRUE),
                               labels=c("Porest 20%","Q2","Q3","Q4","Richest 20%"),
                               include.lowest=TRUE),
    
    resp_income_agg3_str = case_when(
      resp_income_quintile %in% c("Poorest 20%", "Q2") ~ "Poorest 40%", 
      resp_income_quintile %in% c("Q3", "Q4") ~ "Middle 40%", 
      resp_income_quintile %in% c("Richest 20%") ~ "Richest 20%", 
    ), 
    
    resp_live_trnsf_sn_only = ifelse(resp_live_trnsf_sn == 1 & resp_live_trnsf_in == 0, 1, 0),
    resp_live_trnsf_in_only = ifelse(resp_live_trnsf_sn == 0 & resp_live_trnsf_in == 1, 1, 0),
    resp_live_trnsf_both = ifelse(resp_live_trnsf_sn == 1 & resp_live_trnsf_in == 1, 1, 0),
    
    # Financial accounts:
    
    fin_account_any = fsp_formalorMM,
    fin_account_bank = fsp_bank,
    fin_account_tbank = fsp_bank_trd,
    fin_account_nbfi = ifelse(fsp_sacco == 1 | fsp_mfi == 1, 1, 0),
    fin_account_mm = fsp_mobilemoney,
    fin_account_tbanknbfi = ifelse(fin_account_tbank == 1 | fin_account_nbfi == 1, 1, 0),
    fin_chama_user = fsp_sg,
    
    fin_digital_user = ifelse(fin_account_mm == 1 | fst_loans_mbank == 1 | fst_savings_mbank == 1, 1, 0) ,
    
    fin_digital_mm = fsp_mobilemoney,
    fin_digital_mb = fst_mshwarikcb,
    fin_digital_app = fst_loans_app,
    
    # Household-level characteristics
    
    fin_socialnetwork_user = fsp_informal_social,
    fin_socialnetwork_user_ff = fsp_informal_social_ff,
    
    hh_urbrur = case_when(
      hh_urbrur == "Urban" ~ "urb",
      hh_urbrur == "Rural" ~ "rur"
    )
    
    # hh_geo_3 = case_when(
    #   hh_urbrur == "Rural" & (a2 %in% ASAL_counties_data) ~ "rur_asal",
    #   hh_urbrur == "Urban" ~ "urb",
    #   hh_urbrur == "Rural" ~ "rur_oth"
    # ),
    
  ) %>% dummy_cols(select_columns = c("hh_urbrur",  "resp_gender_group", "resp_marstat", "resp_live_group", "resp_live_group2", "resp_age_group2", "resp_edu_group2","resp_inc_group"))


data <- data %>%
  mutate(rooms_per_person = ifelse(is.na(rooms_per_person), mean(rooms_per_person, na.rm = TRUE), rooms_per_person)) %>%
  dummy_cols(select_columns = c("toilet_comp", "water_comp", "lighting_comp", "wall_comp", "cookingfuel_comp", "dwelling_type"))

pr.out <- prcomp(~ rooms_per_person + toilet_comp_1 + toilet_comp_2 + toilet_comp_3 +
                   water_comp_1 + water_comp_2 + water_comp_3 +
                   lighting_comp_1 + lighting_comp_2 + lighting_comp_3 +
                   cookingfuel_comp_1 + cookingfuel_comp_2 + cookingfuel_comp_3 +
                   wall_comp_1 + wall_comp_2 + wall_comp_3 +
                   dwelling_type_1 + dwelling_type_2 + dwelling_type_3 + dwelling_type_4, scale = TRUE, data = data)

hist(pr.out$x[, 1])

data <- data %>% mutate(
  hh_wlth_index = pr.out$x[, 1],
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
  hh_wlth_bottom40 = ifelse(hh_wlth_group == "Poorest 40%", 1, 0),
  
  fst_loans_fuliza = 0,
  
  fst_loans_informal = ifelse(fst_loans_sg == 1 | fst_loans_family == 1 | fst_loans_shopcredit == 1, 1, 0),
  
  fst_loans_agg_social = ifelse(fst_loans_shopcredit == 1 | fst_loans_family == 1, 1, 0),
  fst_loans_agg_trad = ifelse(fst_loans_sg == 1 | fst_loans_fint == 1, 1, 0),
  fst_loans_agg_dig = ifelse(fst_loans_fuliza == 1 | fst_loans_mbank == 1, 1, 0),
  
  # Broad savings groups:
  
  fst_savings_social = ifelse(fst_savings_sg == 1 | fst_savings_network == 1, 1, 0),
  
  fst_savings_informal_any = ifelse(fst_savings_cash == 1 | fst_savings_network == 1 | fst_savings_sg == 1, 1, 0),
  fst_savings_formaloth_any = ifelse(fst_savings_tbank == 1 | fst_savings_mbank == 1 | fst_savings_nbfi == 1, 1, 0),
  
  fst_savings_any = ifelse(fst_savings_tbank == 1 | fst_savings_nbfi == 1 | fst_savings_mm == 1 | fst_savings_mbank == 1 | fst_savings_cash == 1 |
                             fst_savings_network == 1 | fst_savings_sg == 1, 1, 0),
  
  
  fin_status_fct = ifelse(fin_status_impr == 1, "Financial status: Improved", "Financial status: Worsened"),
  fin_status_fct = ifelse(is.na(fin_status_impr) & is.na(fin_status_worse), NA, fin_status_fct),
  
  # Renaming access strand categories:
  access_strand = ifelse(access_strand == "Formal other", "Formal_other", access_strand),
  access_strand = ifelse(access_strand == "Formal prudential", "Formal_prudential", access_strand)
  
)  %>% dummy_cols(select_columns = c("access_strand"))

# Imputing missing personal monthly income variables
# 
# data %>%
#   group_by(hh_urbrur, resp_gender_group, resp_live_group_fct) %>%
#   mutate(
#     mean_income = mean(resp_income, na.rm = TRUE),
#     resp_income = ifelse(is.na(resp_income), mean_income, resp_income),
#     resp_income = ifelse(resp_income == 0, 1, resp_income),
#     resp_income_log = log(resp_income),
#     resp_income_log_std = (resp_income_log - mean(resp_income_log))/ sd(resp_income_log),
#     resp_income_std = (resp_income - mean(resp_income, na.rm = TRUE))/ sd(resp_income, na.rm = TRUE),
#     resp_income_1000 = resp_income/1000,
#     resp_income_1000_c = resp_income_1000 - mean(resp_income_1000)
#   ) %>% ungroup() %>%
#   mutate(
#     fin_debt_inc = total_debt/(ifelse(resp_income == 0, 1, resp_income)),
#     fin_debt_high = ifelse(fin_debt_inc > 0.5, 1, 0),
#     fin_debt_stressany = NA,
#     fin_debt_stressany_inv = NA,
#     fin_debt_repaystress = NA,
#     fin_debt_repaystress_sellassets = j16,
#     fin_debt_repaystress_sellassets = ifelse(is.na(fin_debt_repaystress_sellassets), 0, fin_debt_repaystress_sellassets),
#     fin_debt_repaystress_sellassets_inv = ifelse(fin_debt_repaystress_sellassets == 1, 0, 1),
#     fin_fh_cmpscore_3c_alt = mfhi_o_h_outcome_v2 + mfhi_2_2 + fin_debt_repaystress_sellassets_inv,
#     fin_fh_cmpscore_4c_alt = mfhi_o_h_outcome_v2 + mfhi_2_2 + fin_debt_repaystress_sellassets_inv + mfhi_2_3,
#     fin_fh_cmpscore_5c_alt = mfhi_o_h_outcome_v2 + mfhi_2_2 + fin_debt_repaystress_sellassets_inv + mfhi_2_3 + mfhi_3_2,
#     fin_fh_cmpscore_3c = NA,
#     fin_fh_cmpscore_4c = NA,
#     fin_fh_cmpscore_5c = NA,
#     fin_fh_cmpscore_3c_alt = scale_values_2(fin_fh_cmpscore_3c_alt, min = 0, max = 3),
#     fin_fh_cmpscore_4c_alt = scale_values_2(fin_fh_cmpscore_4c_alt, min = 0, max = 4),
#     fin_fh_cmpscore_5c_alt = scale_values_2(fin_fh_cmpscore_5c_alt, min = 0, max = 5)
#   ) -> data

return(data)

}