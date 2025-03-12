
# Age weight adjustment parameters

# Parameters
ALPHA = 0.01 # scaling parameter
BETA = 1.75 # Dertermines how quickly the declien accelerates at older ages
A_0 = 60 # Reference age where decline starts

prep_2024 <- function(data) { 
  
  # Function-specific parameters
  
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
  
  goals_levels = c(
    "food" = "Putting food on the table",
    "edu" = "Educating myself or my family",
    "live"= "Developing my livelihood/career",
    "other" = "Other"
  )
  
  sav_uses <- c(3:7, 9)
  crd_uses <- c(2:3, 5:9, 11:13)
  
  data <- data %>% 
    mutate(
      
      # Samplng variables --------------
      
      sample_psu = A07, # Primary sampling unit: cluster
      sample_weights = indWeight, # Sampling weights 
      
      # Household characteristics ----------------
      
      hh_county = str_to_title(A01), 
      hh_urbrur = ifelse(A08 == 1, "Rural", "Urban"), 
      
      hh_chronicdisease = ifelse(A23 == 1, 1, 0),
      hh_chronicdisease = ifelse(A23 == 99, NA, hh_chronicdisease), 
      
      hh_geo = case_when(
        hh_urbrur == "Rural" & (county %in% ASAL_counties_FA) ~ "rur_asal",
        county == 47 ~ "urb_nbo",
        hh_urbrur == "Urban" ~ "urb_oth",
        hh_urbrur == "Rural" ~ "rur_oth"
      ),
      hh_geo_2 = case_when(
        county == 47 ~ "urb_nbo",
        hh_urbrur == "Urban" ~ "urb_oth",
        hh_urbrur == "Rural" ~ "rur"
      ),
      hh_geo_3 = case_when(
        hh_urbrur == "Rural" & (county %in% ASAL_counties_FA) ~ "rur_asal",
        hh_urbrur == "Urban" ~ "urb",
        hh_urbrur == "Rural" ~ "rur_oth"
      ),
      hh_geo_fct = factor(geo_levels[hh_geo], levels = geo_levels, ordered = TRUE),
      hh_geo2_fct = factor(geo2_levels[hh_geo_2], levels = geo2_levels, ordered = TRUE),
      hh_geo3_fct = factor(geo2_levels[hh_geo_2], levels = geo3_levels, ordered = TRUE),
      hh_size_all = A12i,
      hh_size_all_c = hh_size_all - mean(hh_size_all),
      
      # Respondent characteristics -----------
      
      resp_all = "All adults",
      
      resp_disability = ifelse(A24i %in% c(3,4) | A24ii %in% c(3,4) | A24iii %in% c(3,4) | A24iv %in% c(3,4) | A24v %in% c(3,4) | A24vi %in% c(3,4), 1, 0), 
      
      resp_gender_group = ifelse(A13 == 1, "men", "wmn"),
      resp_gender_fct = factor(gender_levels[resp_gender_group], gender_levels, ordered = TRUE),
      
      resp_age_yrs = A18, 
      resp_age_yrs_c = resp_age_yrs - mean(resp_age_yrs, na.rm = TRUE), 
      resp_age_yrs_c_5 = resp_age_yrs_c/5,
      resp_age_yrs_c_5_sq = resp_age_yrs_c_5^2,
        
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
      
      # Respondent education
      
      resp_edu_group = case_when(
        A20 == 1 ~ "none",
        A20 == 2 ~ "smpr",
        A20 == 3 ~ "cmpr",
        A20 == 4 ~ "smsc",
        A20 == 5 ~ "cmsc",
        A20 %in% c(6, 7, 8, 9) ~ "smtr",
        A20 %in% c(98,99) ~ NA
      ),
      resp_edu_group_fct = factor(edu_levels[resp_edu_group], levels = edu_levels, ordered = TRUE),
      resp_edu_group2 = case_when(
        A20 == 1 ~ "none",
        A20 %in% c(2, 3) ~ "pri",
        A20 %in% c(4, 5) ~ "sec",
        A20 %in% c(6, 7, 8, 9) ~ "trt",
        A20 %in% c(98,99) ~ NA
      ),
      resp_edu_group2_fct = factor(edu_levels2[resp_edu_group2], levels = edu_levels2, ordered = TRUE),
      
      resp_edu_group3 = case_when(
        A20 %in% c(1,2,3) ~ "prinone",
        A20 %in% c(4, 5) ~ "sec",
        A20 %in% c(6, 7, 8, 9) ~ "trt",
        A20 %in% c(98,99) ~ NA
      ),
      resp_edu_group3_fct = factor(edu_levels3[resp_edu_group3], levels = edu_levels3, ordered = TRUE),
      
      
      # Income sources: 
      
      # Main income source
      resp_live_group = case_when(
        B3B == 1 ~ "farm",
        B3B == 2 ~ "empl",
        B3B == 3 ~ "cwrk",
        B3B == 4 ~ "owbs",
        B3B %in% c(5, 8, 9) ~ "trns",
        B3B %in% c(6, 7, 10, 11) ~ "othr",
        B3B %in% c(98, 99) ~ NA
      ),
      resp_live_group_fct = factor(live_levels[resp_live_group], levels = live_levels, ordered = TRUE),
      
      resp_live_group2 = case_when(
        B3B == 1 ~ "farm",
        B3B == 2 ~ "empl",
        B3B == 3 ~ "cwrk",
        B3B == 4 ~ "owbs",
        B3B %in% c(5, 8, 9) ~ "trns",
        B3B %in% c(6, 7) ~ "inv",
        B3B %in% c(10, 11) ~ "othr", 
        B3B %in% c(98, 99) ~ NA
      ),
      resp_live_group2_fct = factor(live_levels2[resp_live_group2], levels = live_levels2, ordered = TRUE),
      
      resp_live_employment_str = ifelse(B3B == 2, "Employment", "Other source"), 
      
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
      resp_live_agricon = ifelse(B3C %in% c(98,99), NA, resp_live_agricon),
      
      resp_live_farm_any = ifelse(resp_live_agri == 1 | resp_live_agricon == 1, 1, 0),
      
      resp_live_informal_any = ifelse(resp_live_agri == 1 | resp_live_cwrk == 1 | resp_live_owbs == 1, 1, 0),
      resp_live_informal_main = ifelse(B3B %in% c(1,3,4), 1, 0),
      resp_live_informal_sec = ifelse(resp_live_agri_sec == 1 | resp_live_cwrk_sec == 1 | resp_live_owbs_sec == 1, 1, 0),
      
      resp_live_sec_any = ifelse(resp_live_agri_sec == 1 | resp_live_emp_sec == 1 | resp_live_cwrk_sec == 1 | resp_live_owbs_sec == 1 | resp_live_trnsf_sec == 1 | resp_live_other_sec == 1, 1, 0),
      
      
      # Respondent income last month
      resp_income = B3Ii,
      resp_income = ifelse(B3Ii %in% c(98,99), NA, resp_income),
      
      resp_income_max = B3Iii, 
      resp_income_max = ifelse(B3Iii %in% c(98,99), NA, resp_income),
      
      resp_income_min = B3Iiii, 
      resp_income_max = ifelse(B3Iiii %in% c(98,99), NA, resp_income),
      
      resp_income = ifelse(is.na(resp_income) & !is.na(resp_income_max), resp_income_max, resp_income), 
      resp_income = ifelse(is.na(resp_income) & !is.na(resp_income_min), resp_income_min, resp_income), 
      resp_income_min = ifelse(is.na(resp_income_min) & resp_income == resp_income_max, resp_income_max, resp_income_min), 
      resp_income_max = ifelse(is.na(resp_income_max) & resp_income == resp_income_min, resp_income_min, resp_income_max), 
      
      x = resp_income_max, 
      y = resp_income_min, 
      
      resp_income_max = ifelse(y > x, resp_income_min, resp_income_max), 
      resp_income_min = ifelse(y > x, x, y),
      
      resp_income_var = abs(resp_income_max - resp_income_min)/resp_income_min,
      resp_income_var_c = resp_income_var - mean(resp_income_var, na.rm = TRUE),
      
      resp_income_c = resp_income - mean(resp_income, na.rm = TRUE), 
      resp_income_c_1000 = resp_income_c/1000, 
      resp_income_c_5000 = resp_income_c/5000, 
      resp_income_c_5000_2 = resp_income_c_5000^2, 
      resp_income_log = log(resp_income),  
      resp_income_c_log = log(resp_income_c),  
      
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
                               labels=c("Poorest 10%","Q2","Q3","Q4","Q5", "Q6", "Q7", "Q8", "Q9", "Richest 10%"),
                               include.lowest=TRUE),
      
      resp_income_quintile = cut(resp_income,
                                 breaks=quantile(resp_income, probs=seq(0,1, by=0.2), na.rm=TRUE),
                                 labels=c("Poorest 20%","Q2","Q3","Q4","Richest 20%"),
                                 include.lowest=TRUE),
      
      resp_income_agg3_str = case_when(
        resp_income_quintile %in% c("Poorest 20%", "Q2") ~ "Poorest 40%", 
        resp_income_quintile %in% c("Q3", "Q4") ~ "Middle 40%", 
        resp_income_quintile %in% c("Richest 20%") ~ "Richest 20%", 
        ), 
      
      resp_wealth_quintile = Quintiles, 
      resp_wealth_agg3_str = case_when(
        resp_wealth_quintile %in% c(1, 2) ~ "Poorest 40%", 
        resp_wealth_quintile %in% c(3, 4) ~ "Middle 40%", 
        resp_wealth_quintile %in% c(5) ~ "Richest 20%", 
      ), 
      
      # FINANCIAL HEALTH OBJECTIVE OUTCOMES ---------
      
      # Food security -------------
      # f = food
      # p12mos = 12 month recall period
      # p1mo = 1 months recall period
      # wwo = went without food
      # ti = number of times 
      
      mfhi_f_p1mo_wwo_tiany = ifelse(B1Di == 1, 1, 0), 
      
      mfhi_f_p1mo_wwo_yes = ifelse(B1Di == 1, 1, 0), 
      mfhi_f_p1mo_wwo_no = ifelse(B1Di == 2, 1, 0), 
      mfhi_f_p1mo_wwo_dkr = ifelse(B1Di %in% c(9,99), 1, 0), 
      
      mfhi_f_p1mo_wwo_ti12 = ifelse(B1Di == 1 & B1Dii == 1, 1, 0), 
      mfhi_f_p1mo_wwo_ti310 = ifelse(B1Di == 1 & B1Dii == 2, 1, 0),
      mfhi_f_p1mo_wwo_ti10 = ifelse(B1Di == 1 & B1Dii == 3, 1, 0),
      mfhi_f_p1mo_wwo_napp = ifelse(B1Di == 2, 1, 0),
      
      mfhi_f_p12mos_wwo_tiany = ifelse(B1Di == 2 & B1Diii == 1, 1, 0), 
      
      mfhi_f_p12mos_wwo_yes = ifelse(B1Diii == 1, 1, 0), 
      mfhi_f_p12mos_wwo_yes = ifelse(mfhi_f_p1mo_wwo_yes == 1, 0, mfhi_f_p12mos_wwo_yes), 
      mfhi_f_p12mos_wwo_no = ifelse(B1Diii == 2, 1, 0), 
      mfhi_f_p12mos_wwo_no = ifelse(mfhi_f_p1mo_wwo_yes == 1, 0, mfhi_f_p12mos_wwo_no), 
      mfhi_f_p12mos_wwo_dkr = ifelse(B1Diii == 99, 1, 0), 
      mfhi_f_p12mos_wwo_dkr = ifelse(mfhi_f_p1mo_wwo_yes == 1, 0, mfhi_f_p12mos_wwo_dkr), 
      mfhi_f_p12mos_wwo_napp = ifelse(mfhi_f_p1mo_wwo_yes == 1, 1, 0), 
      
      # 1.1: Can securely meet food needs ------
      
      mfhi_f_secure = ifelse(mfhi_f_p1mo_wwo_no == 1 & mfhi_f_p12mos_wwo_no == 1, 1, 0), 
      mfhi_f_secure = ifelse(mfhi_f_p1mo_wwo_dkr == 1 & mfhi_f_p12mos_wwo_dkr == 1, NA, mfhi_f_secure),
      
      # Graduated version
      mfhi_f_secure_c = case_when(
        mfhi_f_p1mo_wwo_ti10 == 1 ~ 0, 
        mfhi_f_p1mo_wwo_ti310 == 1 ~ 1/4, 
        mfhi_f_p1mo_wwo_ti12 == 1 ~ 2/4, 
        mfhi_f_p12mos_wwo_yes == 1 ~ 3/4,
        mfhi_f_secure == 4/4 ~ 1
      ), 
      mfhi_f_secure_c = ifelse(mfhi_f_p1mo_wwo_dkr == 1 & mfhi_f_p12mos_wwo_dkr == 1, NA, mfhi_f_secure_c),
      
        
      # Management of non-food spending -------------
      # nf = non-food
      # p1mo = recall period of 1 month
      # np = unable to pay / cp = able to, can pay / app = applicable to HH situation
      
      mfhi_nf_p1mo_np_hou = ifelse(B1E1 == 1, 1, 0),
      mfhi_nf_p1mo_cp_hou = ifelse(B1E1 == 2, 1, 0),
      mfhi_nf_p1mo_napp_hou = ifelse(B1E1 == 98, 1, 0), 
      mfhi_nf_p1mo_yapp_hou = ifelse(B1E1 != 98, 1, 0), 
      
      mfhi_nf_p1mo_np_enr = ifelse(B1E2 == 1, 1, 0),
      mfhi_nf_p1mo_cp_enr = ifelse(B1E2 == 2, 1, 0),
      mfhi_nf_p1mo_napp_enr = ifelse(B1E2 == 98, 1, 0), 
      mfhi_nf_p1mo_yapp_enr = ifelse(B1E2 != 98, 1, 0), 
      
      mfhi_nf_p1mo_np_wtr = ifelse(B1E3 == 1, 1, 0), 
      mfhi_nf_p1mo_cp_wtr = ifelse(B1E3 == 2, 1, 0),
      mfhi_nf_p1mo_napp_wtr = ifelse(B1E3 == 98, 1, 0), 
      mfhi_nf_p1mo_yapp_wtr = ifelse(B1E3 != 98, 1, 0), 
      
      mfhi_nf_p1mo_np_trn = ifelse(B1E4 == 1, 1, 0), 
      mfhi_nf_p1mo_cp_trn = ifelse(B1E4 == 2, 1, 0),
      mfhi_nf_p1mo_napp_trn = ifelse(B1E4 == 98, 1, 0),
      mfhi_nf_p1mo_yapp_trn = ifelse(B1E4 != 98, 1, 0), 
      
      # Optional addition (school fees)
      mfhi_nf_p12mos_np_sf = ifelse(B1F2 %in% c(1,2), 1, 0), 
      mfhi_nf_p12mos_cp_sf = ifelse(B1F2 == 3, 1, 0),
      mfhi_nf_p12mos_napp_sf = ifelse(B1F2 == 4, 1, 0),
      mfhi_nf_p12mos_dkr_sf = ifelse(B1F2 %in% c(5,6,9), 1, 0), 
      mfhi_nf_p12mos_yapp_sf = ifelse(B1F2 %in% c(1,2,3), 1, 0), 
      
      mfhi_nf_tot_yapp = mfhi_nf_p1mo_yapp_hou +  mfhi_nf_p1mo_yapp_enr +  mfhi_nf_p1mo_yapp_wtr +  mfhi_nf_p1mo_yapp_trn + mfhi_nf_p12mos_yapp_sf,
      # Note: 350 observations where expenditures do not apply 
      
      mfhi_nf_tot_cp = mfhi_nf_p1mo_cp_hou +  mfhi_nf_p1mo_cp_enr +  mfhi_nf_p1mo_cp_wtr +  mfhi_nf_p1mo_cp_trn + mfhi_nf_p12mos_cp_sf,
      
      # Managing school fees --------
      
      mfhi_sf_p12mos_sh_often = ifelse(B1F2 == 1, 1, 0),
      mfhi_sf_p12mos_sh_some = ifelse(B1F2 == 2, 1, 0),
      mfhi_sf_p12mos_sh_nvr = ifelse(B1F2 == 3, 1, 0),
      mfhi_sf_p12mos_sh_napp = ifelse(B1F2 == 4, 1, 0),
      mfhi_sf_p12mos_sh_dkr = ifelse(B1F2 %in% c(5, 6, 9), 1, 0), 
      
      # Often or sometimes had child sent home due to lack of fees
      mfhi_sf_p12mos_sh_oforsm = ifelse(B1F2 %in% c(1,2), 1, 0), 
      mfhi_sf_p12mos_sh_oforsm = ifelse(mfhi_sf_p12mos_sh_napp == 1 | mfhi_sf_p12mos_sh_dkr == 1, NA, mfhi_sf_p12mos_sh_oforsm), 
      
      # Managing medical expenses ------------
      
      mfhi_mc_p12mos_wwo_often = ifelse(B1F1 == 1, 1, 0),
      mfhi_mc_p12mos_wwo_some = ifelse(B1F1 == 2, 1, 0),
      mfhi_mc_p12mos_wwo_nvr = ifelse(B1F1 == 3, 1, 0),
      mfhi_mc_p12mos_wwo_napp = ifelse(is.na(B1F1), 1, 0),
      mfhi_mc_p12mos_wwo_dkr = ifelse(B1F1 %in% c(98, 99, 9), 1, 0),
      
      # Often or sometimews went without medical care due to lack of money 
      mfhi_mc_p12mos_wwo_oforsm_lkm = ifelse(B1F1 %in% c(1,2) & B1FI == 1, 1, 0), 
      mfhi_mc_p12mos_wwo_oforsm_lkm = ifelse(mfhi_mc_p12mos_wwo_napp == 1 | mfhi_mc_p12mos_wwo_dkr == 1, NA, mfhi_mc_p12mos_wwo_oforsm_lkm), 
        
      # 1.2: Can securely meet all applicable non-food expenses ------
      mfhi_nf_secure = ifelse(mfhi_nf_tot_cp  == mfhi_nf_tot_yapp, 1, 0),
      mfhi_nf_secure = ifelse(mfhi_nf_tot_yapp  == 0, NA, mfhi_nf_secure),
  
      # Graduated version
      mfhi_nf_secure_c = mfhi_nf_tot_cp/mfhi_nf_tot_yapp, 
      mfhi_nf_secure_c = ifelse(mfhi_nf_tot_yapp  == 0, NA, mfhi_nf_secure_c),
      
      # Management of debt -------------
      # d = debt
      # p3mo = recall period of 3 months
      
      mfhi_d_p3mo_stress_yes = ifelse(B1G == 1, 1, 0),
      mfhi_d_p3mo_stress_yes = ifelse(B1G %in% c(98,99, 9), NA, mfhi_d_p3mo_stress_yes),
      mfhi_d_p3mo_stress_no = ifelse(B1G == 2, 1, 0),
      mfhi_d_p3mo_stress_no = ifelse(B1G %in% c(98,99, 9), NA, mfhi_d_p3mo_stress_no),
      mfhi_d_p3mo_stress_napp = ifelse(B1G == 3, 1, 0),
      mfhi_d_p3mo_stress_napp = ifelse(B1G %in% c(98,99, 9), NA, mfhi_d_p3mo_stress_napp),
      mfhi_d_p3mo_stress_dkr = ifelse(B1G %in% c(98,99, 9), 1, 0),
      
      mfhi_d_secure = mfhi_d_p3mo_stress_no, 
      mfhi_d_secure = ifelse(mfhi_d_p3mo_stress_napp == 1, 1, mfhi_d_secure), 
      
      # Access to emergency funds -------------
      # ef = emergencyfunds
      # src = source of funds
      # p = possible / np = not possible
      # dif = difficulty (any = any level of difficult, vry = very diffi ult, smeornot = somewhat OR not difficult at all)
      
      mfhi_ef_30d_srcany_dkr = ifelse(B1H %in% c(98,99), 1, 0),
      mfhi_ef_30d_srcany_np = ifelse(B1H == 9, 1, 0),
      
      mfhi_ef_30d_srcany_p_difany = ifelse(B1H %in% c(1, 2, 3, 4, 5, 6, 7, 8), 1, 0),
      mfhi_ef_30d_srcany_p_difany = ifelse(mfhi_ef_30d_srcany_dkr == 1, NA, mfhi_ef_30d_srcany_p_difany),
      mfhi_ef_30d_srcany_p_difvry = ifelse(B1H %in% c(1, 2, 3, 4, 5, 6, 7, 8) &  B1Ii == 1, 1, 0),
      mfhi_ef_30d_srcany_p_difvry = ifelse(mfhi_ef_30d_srcany_dkr == 1, NA, mfhi_ef_30d_srcany_p_difvry),
      mfhi_ef_30d_srcany_p_difsme = ifelse(B1H %in% c(1, 2, 3, 4, 5, 6, 7, 8) &  B1Ii %in% c(2), 1, 0),
      mfhi_ef_30d_srcany_p_difsme = ifelse(mfhi_ef_30d_srcany_dkr == 1, NA, mfhi_ef_30d_srcany_p_difsme),
      mfhi_ef_30d_srcany_p_difnot = ifelse(B1H %in% c(1, 2, 3, 4, 5, 6, 7, 8) &  B1Ii %in% c(3), 1, 0),
      mfhi_ef_30d_srcany_p_difnot = ifelse(mfhi_ef_30d_srcany_dkr == 1, NA, mfhi_ef_30d_srcany_p_difnot),
      
      mfhi_ef_30d_srcany_p_difsmeornot = ifelse(B1H %in% c(1, 2, 3, 4, 5, 6, 7, 8) &  B1Ii %in% c(2,3), 1, 0),
      mfhi_ef_30d_srcany_p_difsmeornot = ifelse(mfhi_ef_30d_srcany_dkr == 1, NA, mfhi_ef_30d_srcany_p_difsmeornot),
      
      mfhi_ef_secure = mfhi_ef_30d_srcany_p_difnot, 
      
      mfhi_ef_secure_c = case_when(
        mfhi_ef_30d_srcany_np == 1 ~ 0, 
        mfhi_ef_30d_srcany_p_difvry == 1 ~ 1/3, 
        mfhi_ef_30d_srcany_p_difsme == 1 ~ 2/3, 
        mfhi_ef_30d_srcany_p_difnot == 1 ~ 3/3
      ), 
      mfhi_ef_secure_c = ifelse(mfhi_ef_30d_srcany_dkr == 1, NA, mfhi_ef_secure_c), 
      
      # Source of funds
      mfhi_ef_30d_srcborr_p_difany = ifelse(B1H == 1, 1, 0), 
      mfhi_ef_30d_srcsav_p_difany = ifelse(B1H == 2, 1, 0), 
      mfhi_ef_30d_srcasst_p_difany = ifelse(B1H %in% c(3,4), 1, 0), 
      mfhi_ef_30d_srcff_p_difany = ifelse(B1H == 5, 1, 0), 
      mfhi_ef_30d_srccut_p_difany = ifelse(B1H == 6, 1, 0), 
      mfhi_ef_30d_srcwrk_p_difany = ifelse(B1H == 7, 1, 0), 
      mfhi_ef_30d_srcinc_p_difany = ifelse(B1H == 8, 1, 0), 
      mfhi_ef_30d_srcoth_p_difany = ifelse(B1H %in% c(10, 11), 1, 0),

      # Source of loans
      
      mfhi_ef_30d_srcborr_p_bnk = ifelse(B1Hi == 1, 1, 0), 
      mfhi_ef_30d_srcborr_p_bnk = ifelse(mfhi_ef_30d_srcborr_p_difany == 0, NA, mfhi_ef_30d_srcborr_p_bnk),
      mfhi_ef_30d_srcborr_p_mb = ifelse(B1Hi == 2, 1, 0), 
      mfhi_ef_30d_srcborr_p_mb = ifelse(mfhi_ef_30d_srcborr_p_difany == 0, NA, mfhi_ef_30d_srcborr_p_mb),
      mfhi_ef_30d_srcborr_p_mm = ifelse(B1Hi == 3, 1, 0), 
      mfhi_ef_30d_srcborr_p_mm = ifelse(mfhi_ef_30d_srcborr_p_difany == 0, NA, mfhi_ef_30d_srcborr_p_mm),
      mfhi_ef_30d_srcborr_p_nbfi = ifelse(B1Hi %in% c(4,5), 1, 0), 
      mfhi_ef_30d_srcborr_p_nbfi = ifelse(mfhi_ef_30d_srcborr_p_difany == 0, NA, mfhi_ef_30d_srcborr_p_nbfi),
      mfhi_ef_30d_srcborr_p_ml = ifelse(B1Hi == 6, 1, 0), 
      mfhi_ef_30d_srcborr_p_ml = ifelse(mfhi_ef_30d_srcborr_p_difany == 0, NA, mfhi_ef_30d_srcborr_p_ml),
      mfhi_ef_30d_srcborr_p_sg = ifelse(B1Hi == 7, 1, 0), 
      mfhi_ef_30d_srcborr_p_sg = ifelse(mfhi_ef_30d_srcborr_p_difany == 0, NA, mfhi_ef_30d_srcborr_p_sg),
      mfhi_ef_30d_srcborr_p_gov = ifelse(B1Hi %in% c(8,9), 1, 0), 
      mfhi_ef_30d_srcborr_p_gov = ifelse(mfhi_ef_30d_srcborr_p_difany == 0, NA, mfhi_ef_30d_srcborr_p_gov),
      mfhi_ef_30d_srcborr_p_sn = ifelse(B1Hi %in% c(10,11), 1, 0), 
      mfhi_ef_30d_srcborr_p_sn = ifelse(mfhi_ef_30d_srcborr_p_difany == 0, NA, mfhi_ef_30d_srcborr_p_sn),
      mfhi_ef_30d_srcborr_p_dig = ifelse(B1Hi %in% c(12), 1, 0), 
      mfhi_ef_30d_srcborr_p_dig = ifelse(mfhi_ef_30d_srcborr_p_difany == 0, NA, mfhi_ef_30d_srcborr_p_dig),
      mfhi_ef_30d_srcborr_p_shp = ifelse(B1Hi == 13, 1, 0), 
      mfhi_ef_30d_srcborr_p_shp = ifelse(mfhi_ef_30d_srcborr_p_difany == 0, NA, mfhi_ef_30d_srcborr_p_shp),
      mfhi_ef_30d_srcborr_p_oth = ifelse(B1Hi %in% c(14, 15), 1, 0), 
      mfhi_ef_30d_srcborr_p_oth = ifelse(mfhi_ef_30d_srcborr_p_difany == 0, NA, mfhi_ef_30d_srcborr_p_oth),
      
      # Capital investments -------------
      # i = investing for the future
      # pc = physical capital / fc = financial capital / hc = human capital
      # p12mos = recall period is past 12 months
      # any 
      
      ## Need to check if these have "Don't know/refused" options: 
      # Physical capital 
      
      mfhi_i_pc_p12mos_lndhou = ifelse(B1L__1 == 1 |  B1L__2 == 1 |  B1L__3 == 1, 1, 0),
      mfhi_i_pc_p12mos_nonfarmliv = ifelse(B1L__4 == 1, 1, 0),
      mfhi_i_pc_p12mos_farm = ifelse(B1L__5 == 1, 1, 0),
      mfhi_i_pc_p12mos_liv = ifelse(B1L__6 == 1, 1, 0),
      mfhi_i_pc_p12mos_farmliv = ifelse(B1L__5 == 1 | B1L__6 == 1, 1, 0),
      
      mfhi_i_pc_p12mos_allmiss = ifelse(is.na(B1L__1) & is.na(B1L__2) & is.na(B1L__3) & is.na(B1L__4) & is.na(B1L__5) & is.na(B1L__6), 1, 0), 
      
      mfhi_i_pc_p12mos_any = ifelse(B1L__1 == 1 |  B1L__2 == 1 |  B1L__3 == 1 | B1L__4 == 1 | B1L__5 == 1 | B1L__6 == 1, 1, 0),
      mfhi_i_pc_p12mos_any = ifelse(is.na(mfhi_i_pc_p12mos_any), 0, mfhi_i_pc_p12mos_any), 
      mfhi_i_pc_p12mos_any = ifelse(mfhi_i_pc_p12mos_allmiss == 1, NA, mfhi_i_pc_p12mos_any), 
      
      # Financial capital
      
      mfhi_i_fc_p12mos_pen = ifelse(B1M__1 == 1, 1, 0), # Pension or IRA
      mfhi_i_fc_p12mos_lts = ifelse(B1M__2 == 1, 1, 0), # Long term savings
      mfhi_i_fc_p12mos_sec = ifelse(B1M__3 == 1, 1, 0), # Financial securities
      
      mfhi_i_fc_p12mos_allmiss = ifelse(is.na(B1M__1) & is.na(B1M__2) & is.na(B1M__3), 1, 0), 
      
      mfhi_i_fc_p12mos_any = ifelse(B1M__1 == 1 | B1M__2 == 1 | B1M__3 == 1, 1, 0),
      mfhi_i_fc_p12mos_any = ifelse(is.na(mfhi_i_fc_p12mos_any), 0, mfhi_i_fc_p12mos_any), 
      mfhi_i_fc_p12mos_any = ifelse(mfhi_i_fc_p12mos_allmiss == 1, NA, mfhi_i_fc_p12mos_any), 
      
      # Investing in financial or physical capital 
      mfhi_i_pcfc_p12mos_any = ifelse(mfhi_i_fc_p12mos_any == 1 | mfhi_i_pc_p12mos_any == 1, 1, 0), 
      mfhi_i_pcfc_p12mos_any = ifelse(mfhi_i_pc_p12mos_allmiss == 1 & mfhi_i_fc_p12mos_allmiss == 1, NA, mfhi_i_pcfc_p12mos_any),
      
      # Human capital
      
      mfhi_i_hc_p12mos_any = ifelse(B1M__4 == 1, 1, 0),
      mfhi_i_hc_p12mos_allmiss = ifelse(is.na(B1M__4), 1, 0), 
      
      mfhi_i_hc_p12mos_any = ifelse(is.na(mfhi_i_hc_p12mos_any), 0, mfhi_i_hc_p12mos_any), 
      mfhi_i_hc_p12mos_any = ifelse(mfhi_i_hc_p12mos_allmiss == 1, NA, mfhi_i_hc_p12mos_any),
      
      # Any investments in physical, financial or human capital
      mfhi_i_p12mos_any = ifelse(mfhi_i_pc_p12mos_any == 1 | mfhi_i_fc_p12mos_any == 1 | mfhi_i_hc_p12mos_any == 1, 1, 0),
      mfhi_i_p12mos_any = ifelse(is.na(mfhi_i_p12mos_any), 0, mfhi_i_p12mos_any),
      mfhi_i_p12mos_any = ifelse(mfhi_i_pc_p12mos_allmiss == 1 & mfhi_i_fc_p12mos_allmiss == 1 & mfhi_i_hc_p12mos_allmiss == 1 , NA, mfhi_i_p12mos_any),
      
      # Graduated version 
      mfhi_i_p12mos_any_c = (mfhi_i_pc_p12mos_any + mfhi_i_fc_p12mos_any + mfhi_i_hc_p12mos_any)/3,
      mfhi_i_p12mos_any_c = ifelse(mfhi_i_pc_p12mos_allmiss == 1 & mfhi_i_fc_p12mos_allmiss == 1 & mfhi_i_hc_p12mos_allmiss == 1 , NA, mfhi_i_p12mos_any_c),

      
      # Aggregation: Multidimensional financial health index -------------
      
      # Number of observations not missing 
      mfhi_f_secure_notmiss = ifelse(!is.na(mfhi_f_secure), 1, 0), 
      mfhi_nf_secure_notmiss = ifelse(!is.na(mfhi_nf_secure), 1, 0), 
      mfhi_d_secure_notmiss = ifelse(!is.na(mfhi_d_secure), 1, 0), 
      mfhi_ef_secure_notmiss = ifelse(!is.na(mfhi_ef_secure), 1, 0), 
      mfhi_i_p12mos_any_notmiss = ifelse(!is.na(mfhi_i_p12mos_any), 1, 0), 
      mfhi_i_pcfc_p12mos_any_notmiss = ifelse(!is.na(mfhi_i_pcfc_p12mos_any), 1, 0), 
      mfhi_i_hc_p12mos_any_notmiss = ifelse(!is.na(mfhi_i_hc_p12mos_any), 1, 0), 
      
      mfhi_md2d_notmiss = mfhi_f_secure_notmiss + mfhi_nf_secure_notmiss + mfhi_d_secure_notmiss, 
      mfhi_risk_notmiss = mfhi_ef_secure_notmiss, 
      #mfhi_inv_notmiss = mfhi_i_p12mos_any_notmiss, 
      mfhi_inv_notmiss = mfhi_i_pcfc_p12mos_any_notmiss + mfhi_i_hc_p12mos_any_notmiss,
      mfhi_all_N_notmiss = mfhi_f_secure_notmiss + mfhi_nf_secure_notmiss + mfhi_d_secure_notmiss + mfhi_ef_secure_notmiss + mfhi_i_pcfc_p12mos_any_notmiss + mfhi_i_hc_p12mos_any_notmiss, 
      mfhi_all_N_miss = 6 - mfhi_all_N_notmiss, 
      
      # Dimension and indicator weights
      
      n_dim = 3, # Total number of dimensions in index
      w_dim = 1/n_dim, #Each dimension has equal weight in the overall index
      
      n_md2d = mfhi_md2d_notmiss, # Number of non-missing indicators in dimension 1: Managing day to day
      w_md2d = w_dim*(1/n_md2d), # Weight of each indicator in managing day to day
      w_md2d = ifelse(n_md2d == 0, 0, w_md2d), 
      
      n_risk = mfhi_risk_notmiss, # Number of non-missing indicators in dimension 2: Protecting against shocks
      w_risk = w_dim*(1/n_risk), # Weight of each indicator in dimension 2
      w_risk = ifelse(n_risk == 0, 0, w_risk), 
      
      n_inv = mfhi_inv_notmiss, # Number of non-missing indicators in dimension 3: Building capital
      w_inv = w_dim*(1/n_inv), # Weight of each indicator in dimension 3, 
      w_inv = ifelse(n_inv == 0, 0, w_inv), 
    
      # Restandardizing weights to account for cases where all indicators in a domain are missing
      w_total = w_md2d*n_md2d + w_risk*n_risk + w_inv*n_inv, 
      w_md2d_adj = w_md2d/w_total, 
      w_risk_adj = w_risk/w_total, 
      w_inv_adj = w_inv/w_total, 
      w_total_adj = w_md2d_adj*n_md2d + w_risk_adj*n_risk + w_inv_adj*n_inv, 
      
      # Age adjusted weights
      
      aw = ifelse(resp_age_yrs < A_0, 1, exp(-ALPHA*(resp_age_yrs - A_0)^BETA)), 
      w_dim_inv_aw = w_dim*aw, 
      w_inv_aw = w_dim_inv_aw*(1/n_inv), 
      w_inv_aw = ifelse(n_inv == 0, 0, w_inv_aw), 
      w_dim_oth_aw = (1-w_dim_inv_aw)/2, 
      w_risk_aw = w_dim_oth_aw*(1/n_risk), 
      w_risk_aw = ifelse(n_risk == 0, 0, w_risk_aw),
      w_md2d_aw = w_dim_oth_aw*(1/n_md2d), 
      w_md2d_aw = ifelse(n_md2d == 0, 0, w_md2d_aw), 
      
      # Restandardizing weights to account for cases where all indicators in a domain are missing
      w_total_aw = w_md2d_aw*n_md2d + w_risk_aw*n_risk + w_inv_aw*n_inv, 
      w_md2d_adj_aw = w_md2d_aw/w_total_aw, 
      w_risk_adj_aw = w_risk_aw/w_total_aw, 
      w_inv_adj_aw = w_inv_aw/w_total_aw, 
      w_total_adj_aw = w_md2d_adj_aw*n_md2d + w_risk_adj_aw*n_risk + w_inv_adj_aw*n_inv, 
      
    ) %>% 
        
    rowwise() %>% 

      mutate(
        mfhi_score_md2d = w_md2d_adj*sum(mfhi_f_secure, mfhi_nf_secure, mfhi_d_secure, na.rm = TRUE),
        mfhi_score_risk = w_risk_adj*sum(mfhi_ef_secure, na.rm = TRUE), 
        #mfhi_score_inv = w_inv_adj*sum(mfhi_i_p12mos_any, na.rm = TRUE), 
        mfhi_score_inv = w_inv_adj*sum(mfhi_i_pcfc_p12mos_any, mfhi_i_hc_p12mos_any, na.rm = TRUE), 
        
        mfhi_score_md2d_c = w_md2d_adj*sum(mfhi_f_secure_c, mfhi_nf_secure_c, mfhi_d_secure, na.rm = TRUE),
        mfhi_score_risk_c = w_risk_adj*sum(mfhi_ef_secure_c, na.rm = TRUE), 
        mfhi_score_inv_c = w_inv_adj*sum(mfhi_i_pcfc_p12mos_any, mfhi_i_hc_p12mos_any, na.rm = TRUE), 
       # mfhi_score_inv_c = w_inv_adj*sum(mfhi_i_p12mos_any_c, na.rm = TRUE)
        
       mfhi_score_md2d_c_aw = w_md2d_adj_aw*sum(mfhi_f_secure_c, mfhi_nf_secure_c, mfhi_d_secure, na.rm = TRUE),
       mfhi_score_risk_c_aw = w_risk_adj_aw*sum(mfhi_ef_secure_c, na.rm = TRUE), 
       mfhi_score_inv_c_aw = w_inv_adj_aw*sum(mfhi_i_pcfc_p12mos_any, mfhi_i_hc_p12mos_any, na.rm = TRUE), 
       
      ) %>% 
    
      ungroup() %>% 
    
    mutate(
      
      # Weighted sum of all component indicators
      mfhi_score_overall = mfhi_score_md2d + mfhi_score_risk + mfhi_score_inv,
      mfhi_score_overall_c = mfhi_score_md2d_c + mfhi_score_risk_c + mfhi_score_inv_c,
      mfhi_score_overall_c_aw = mfhi_score_md2d_c_aw + mfhi_score_risk_c_aw + mfhi_score_inv_c_aw,
        
      # Headcount ratios to calculate % of population with high, medium and low financial health scores
      
      # Discrete
      mfhi_score_hi = ifelse(mfhi_score_overall > 0.6, 1, 0),
      mfhi_score_med = ifelse(mfhi_score_overall > 0.3 & mfhi_score_overall <= 0.6, 1, 0),
      mfhi_score_low = ifelse(mfhi_score_overall <= 0.3, 1, 0),
    
      # Gradated
      mfhi_score_hi_c = ifelse(mfhi_score_overall_c > 0.6, 1, 0),
      mfhi_score_med_c = ifelse(mfhi_score_overall_c > 0.3 & mfhi_score_overall_c <= 0.6, 1, 0),
      mfhi_score_low_c = ifelse(mfhi_score_overall_c <= 0.3, 1, 0),
      
      # Gradated, Age weighted 
      mfhi_score_hi_c_aw = ifelse(mfhi_score_overall_c_aw > 0.6, 1, 0),
      mfhi_score_med_c_aw = ifelse(mfhi_score_overall_c_aw > 0.3 & mfhi_score_overall_c_aw <= 0.6, 1, 0),
      mfhi_score_low_c_aw = ifelse(mfhi_score_overall_c_aw <= 0.3, 1, 0),
      
      # Perception of financial health (pfh) ---------
      
      # Daily needs
      
      pfh_dailyneeds_vw = ifelse(B1N1 == 1, 1, 0), # very worried
      pfh_dailyneeds_sw = ifelse(B1N1 == 2, 1, 0), # somewhat worried
      pfh_dailyneeds_nw = ifelse(B1N1 == 3, 1, 0), # not worried at all
      pfh_dailyneeds_dkr = ifelse(B1N1 %in% c(98,99), 1, 0),
      
      # Managing existing debts
      
      pfh_debt_vw = ifelse(B1N2 == 1, 1, 0), # very worried
      pfh_debt_sw = ifelse(B1N2 == 2, 1, 0), # somewhat worried
      pfh_debt_nw = ifelse(B1N2 == 3, 1, 0), # not worried at all
      pfh_debt_napp = ifelse(B1N2 == 4, 1, 0), # Not applicable
      pfh_debt_dkr = ifelse(B1N2 %in% c(98,99), 1, 0),
      
      # Can cope with an unexpected large expense such as a medical bill 
      
      pfh_risk_ag = ifelse(B1C4 == 1, 1, 0), 
      pfh_risk_di = ifelse(B1C4 == 2, 1, 0), 
      pfh_risk_nagdi = ifelse(B1C4 == 3, 1, 0), 
      pfh_risk_dkr = ifelse(B1C4 %in% c(5,9), 1, 0), 
      
      # Long term financial goals
      
      pfh_goals_vc = ifelse(B1O == 1, 1, 0), # very confident
      pfh_goals_sc = ifelse(B1O == 2, 1, 0), # Somewhat confident
      pfh_goals_nc = ifelse(B1O == 3, 1, 0), # Not at all confident
      pfh_goals_dkr = ifelse(B1O %in% c(98,99), 1, 0),
      
      # Change in financial status
      pfh_finstatus_imp = ifelse(B1P == 1, 1, 0), # Improved
      pfh_finstatus_sam = ifelse(B1P == 2, 1, 0), # Stayed the same
      pfh_finstatus_wor = ifelse(B1P == 3, 1, 0),  # Worsened
      pfh_finstatus_dkr = ifelse(B1P %in% c(98,99), 1, 0), 
      
      pfh_score_dailyneeds = case_when(
        pfh_dailyneeds_vw == 1 ~ 0/2, 
        pfh_dailyneeds_sw == 1 ~ 1/2, 
        pfh_dailyneeds_nw == 1 ~ 2/2, 
      ),
      
      pfh_score_debt= case_when(
        pfh_debt_vw == 1 ~ 0/2, 
        pfh_debt_sw == 1 ~ 1/2, 
        pfh_debt_nw == 1 ~ 2/2, 
      ),
      
      pfh_score_goals = case_when(
        pfh_goals_nc == 1 ~ 0/2, 
        pfh_goals_sc == 1 ~ 1/2, 
        pfh_goals_vc == 1 ~ 2/2, 
      ),
      
      pfh_score_finstatus = case_when(
        pfh_finstatus_wor == 1 ~ 0/2, 
        pfh_finstatus_sam == 1 ~ 1/2, 
        pfh_finstatus_imp == 1 ~ 2/2, 
      ), 
      
      pfh_score_overall = (pfh_score_dailyneeds + pfh_score_debt + pfh_score_goals + pfh_score_finstatus)/4,
      
      
      # At this point in your life what is your most important goal:
      
      goals_group = case_when(
        B1A == 1 ~ "food",
        B1A == 2 ~ "educ",
        B1A %in% c(3, 7) ~ "live",
        B1A %in% c(4, 5, 6, 8) ~ "other", 
        B1A %in% c(99) ~ NA, 
      ),
      goals_group_fct = factor(goals_levels[goals_group], goals_levels, ordered = TRUE),
      
      
      # Time compatible indicators ---------
      
      # -- Plan for how to spend money
      mfhi_1_2 = ifelse(B1C3 == 1, 1, 0),
      mfhi_1_2 = ifelse(B1C3 %in% c(3, 5, 9), NA, mfhi_1_2),
      
      # -- Does not go without food
      mfhi_1_3 = mfhi_f_secure,
      
      # -- Can access a lump sum (If you needed Ksh 3,500 for rural, Ksh 6,000 for urban in 3 days, would you be able to get it?)
      mfhi_2_2 = B1J,
      mfhi_2_2 = ifelse(is.na(B1J), B1K, mfhi_2_2),
      mfhi_2_2 = ifelse(mfhi_2_2 == 2, 0, mfhi_2_2),
      mfhi_2_2 = ifelse(mfhi_2_2 %in% c(98,99, 9), NA, mfhi_2_2),
    
      mfhi_ef_3d_srcany_dkr = ifelse(is.na(mfhi_2_2), 1, 0),
      mfhi_ef_3d_srcany_np = ifelse(mfhi_2_2 == 0, 1, 0),
      mfhi_ef_3d_srcany_p = ifelse(mfhi_2_2 == 1, 1, 0),
      
      # -- Keeps money aside for an emergency
      mfhi_2_3 = ifelse(B1C2 == 1, 1, 0),
      mfhi_2_3 = ifelse(B1C2 %in% c(5, 9), NA, mfhi_2_3),
      
      # Did financial status improve, stay the same or worsen in past year?
      fin_status_impr = pfh_finstatus_imp,
      fin_status_impr = ifelse(pfh_finstatus_dkr == 1, NA, fin_status_impr),
      fin_status_worse = pfh_finstatus_wor,
      fin_status_worse = ifelse(pfh_finstatus_dkr ==1, NA, fin_status_worse),
      
      # Financial behaviors & social capital  -----------
      
      fb_decisions = ifelse(B1Bi %in% c(1, 3, 4) | B1Bii %in% c(1, 3, 4), 1, 0), 
      fb_financial_plan = mfhi_1_2,
      fb_emergency_savings = mfhi_2_3, 
      
      fb_gambling = ifelse(B2B == 1, 1, 0), 
      fb_gambling = ifelse(B2B %in% c(98,99), NA, fb_gambling), 
      
      fb_gambling_spend = ifelse(fb_gambling == 1, B2Ci, 0),  
      fb_gambling_spend_inc = fb_gambling_spend/ifelse(resp_income == 0, 1, resp_income), 
      
      # Financial literacy --------------
      
      fl_interest_correct = ifelse(B2Ei == 1, 1, 0), 
      fl_interest_correct = ifelse(B2Ei %in% c(98,99), NA, fl_interest_correct), 
      
      fl_inflation_correct = ifelse(B2Eii == 3, 1, 0), 
      fl_inflation_correct = ifelse(B2Eii %in% c(98,99), NA, fl_inflation_correct),
      
      fl_digliteracy = ifelse(B2F == 1, 1, 0), 
      fl_digliteracy = ifelse(B2F %in% c(4, 98,99), NA, fl_digliteracy),
      
      fl_literacy_all = ifelse(fl_interest_correct == 1 & fl_inflation_correct == 1 & fl_digliteracy == 1, 1, 0), 
      
      # Social capital
      sc_ff_finhelp = ifelse(B1C1 == 1, 1, 0),
      sc_ff_finhelp = ifelse(B1C1 %in% c(5, 9), NA, sc_ff_finhelp),
      
      # Wealth (assets)
      
      resp_wlth_any = ifelse(F1_1 == 1 | F1__2 == 1 | F1__3 == 1 | F1__4 == 1 | F1__5 == 1 | F1__96 == 1, 1, 0), 
      resp_wlth_any = ifelse(is.na(F1_1) & is.na(F1__2) & is.na(F1__3) & is.na(F1__4) & is.na(F1__5) & is.na(F1__96), NA, resp_wlth_any),
      
      # Debt 
      
      fin_debt_total = rowSums(.[grepl("E1xi[A-Z]$", names(.))], na.rm = T),
      fin_debt_any = ifelse(fin_debt_total > 0, 1, 0),
      fin_debt_inc = fin_debt_total/(ifelse(resp_income == 0, 1, resp_income)),
      fin_debt_high = ifelse(fin_debt_inc > 0.5, 1, 0),
      
      fin_debtservice_spend = E2A, 
      fin_debtservice_spend = ifelse(E2A %in% c(98,99), NA, fin_debtservice_spend),
      fin_debtservice_spend = ifelse(fin_debt_any == 0 & is.na(fin_debtservice_spend), 0, fin_debtservice_spend), 
      fin_debtservice_inc = fin_debtservice_spend/(ifelse(resp_income == 0, 100, resp_income)),
      fin_debtservice_inc = ifelse(fin_debtservice_spend == 0 & is.na(resp_income), 0, fin_debtservice_inc), 
      fin_debtservice_high = ifelse(fin_debtservice_inc > 0.5, 1, 0), 
      
      fin_debt_defaulted_dnp = ifelse(E2E == 1, 1, 0), 
      fin_debt_defaulted_dnp = ifelse(E2E %in% c(98,99), NA, fin_debt_defaulted_dnp), 
      
      fin_debt_defaulted_any = ifelse(E2E %in% c(1, 2), 1, 0), 
      fin_debt_defaulted_any = ifelse(E2E %in% c(98,99), NA, fin_debt_defaulted_dnp), 
      
      # Usage of financial services -----------
      
      # currently uses different types of registered transaction device
      
      fin_reg_mobilemoney = ifelse(C1_10 == 1, 1, 0), 
      fin_reg_mobilemoney = ifelse(C1_10 == 9, NA, fin_reg_mobilemoney), 
      
      fin_reg_mobilebanking = ifelse(C1_11 == 1, 1, 0), 
      fin_reg_mobilebanking = ifelse(C1_11 == 9, NA, fin_reg_mobilebanking), 
      
      fin_reg_bankmfb = ifelse(C1_11 == 1, 1, 0), 
      fin_reg_bankmfb = ifelse(C1_11 == 9, NA, fin_reg_bankmfb), 
      
      fin_reg_bankmfb = ifelse(C1_12 == 1, 1, 0), 
      fin_reg_bankmfb = ifelse(C1_12 == 9, NA, fin_reg_bankmfb), 
      
      fin_reg_sacco = ifelse(C1_12a == 1, 1, 0), 
      fin_reg_sacco = ifelse(C1_12a == 9, NA, fin_reg_sacco), 
      
      fin_reg_chama = ifelse(C1_12b == 1, 1, 0), 
      fin_reg_chama = ifelse(C1_12b == 9, NA, fin_reg_chama),
      
      fin_reg_insurance = ifelse(C1_36 == 1 | C1_37 == 1 | C1_38 == 1 | C1_39 == 1, 1, 0), 
      fin_reg_insurance = ifelse(C1_36 == 9 &  C1_37 == 9 & C1_38 == 9 & C1_39 == 9, NA, fin_reg_insurance), 
      
      # Current usage of savings
      
      fin_sav_bankmfb = binarize_1else0(C1_1a), 
      
      fin_sav_mfi = binarize_1else0(C1_1b), 
      
      fin_sav_mobilebanking = binarize_1else0(C1_2), 
      
      fin_sav_mobilemoney = binarize_1else0(C1_3), 
      
      fin_sav_sacco = binarize_1else0(C1_4), 
      
      fin_sav_chama = binarize_1else0(C1_5), 
      
      fin_sav_ff = binarize_1else0(C1_6), 
      
      fin_sav_secret = binarize_1else0(C1_7), 
      
      fin_sav_digapp = binarize_1else0(C1_8),
      
      # Current usage of loans
      
      fin_loan_bank = binarize_1else0(C1_13a), 
      
      fin_loan_mfb = binarize_1else0(C1_13b), 
      
      fin_loan_overdraft = binarize_1else0(C1_13c), 
      
      fin_loan_creditcard = binarize_1else0(C1_14), 
      
      fin_loan_mobilebanking = binarize_1else0(C1_15), 
      
      fin_loan_mobilemoney = binarize_1else0(C1_16), 
      
      fin_loan_sacco = binarize_1else0(C1_17), 
      
      fin_laon_mfi = binarize_1else0(C1_18), 
      
      fin_laon_digapp = binarize_1else0(C1_19), 
      
      fin_loan_shylocks = binarize_1else0(C1_20), 
      
      fin_loan_chama = binarize_1else0(C1_21), 
      
      fin_loan_gov = binarize_1else0(C1_22), 
      
      fin_loan_hustler = binarize_1else0(C1_23), 
      
      fin_laon_employer = binarize_1else0(C1_24), 
      
      fin_loan_ff = binarize_1else0(C1_25), 
      
      fin_loan_shopkeepercash = binarize_1else0(C1_26), 
      
      fin_loan_shopkeepergoods = binarize_1else0(C1_27), 
      
      fin_loan_buyer = binarize_1else0(C1_28), 
      
      fin_loan_hirepurchase = binarize_1else0(C1_29), 
      
      fin_loan_insurance = binarize_1else0(C1_30), 
      
      fin_loan_mortgage = binarize_1else0(C1_31),
      
      fin_reg_ff = ifelse(fin_loan_ff == 1 | fin_sav_ff == 1, 1, 0), 
      
      # Aggregate categories: 
      
      fin_sav_agg4_formal_trd = ifelse(fin_sav_bankmfb == 1 | fin_sav_sacco == 1 | fin_sav_mfi == 1, 1, 0), 
      fin_sav_agg4_formal_dig = ifelse(fin_sav_mobilebanking == 1 | fin_sav_mobilemoney == 1 | fin_sav_digapp == 1, 1, 0), 
      fin_sav_agg4_informal = ifelse(fin_sav_chama == 1 | fin_sav_ff == 1, 1, 0), 
      fin_sav_agg4_informal_exchama = ifelse(fin_sav_secret == 1 | fin_sav_ff == 1, 1, 0), 
      fin_sav_agg4_cash = fin_sav_secret,
      
      fin_loan_agg4_formal_trd = ifelse(fin_loan_bank == 1 | fin_loan_mfb == 1 | fin_loan_overdraft == 1 | fin_loan_creditcard == 1 | fin_loan_sacco == 1 | fin_laon_mfi == 1 | fin_loan_mortgage == 1, 1, 0), 
      fin_loan_agg4_formal_dig = ifelse(fin_loan_mobilebanking == 1 | fin_loan_mobilemoney == 1 | fin_laon_digapp == 1, 1, 0), 
      fin_loan_agg4_informal = ifelse(fin_loan_shylocks == 1 | fin_loan_chama == 1 | fin_loan_ff == 1 | fin_loan_shopkeepercash == 1 | fin_loan_shopkeepergoods == 1, 1, 0), 
      fin_loan_agg4_other = ifelse(fin_loan_hirepurchase == 1 | fin_loan_insurance == 1 | fin_loan_buyer == 1, 1, 0), 
      
      fin_loan_agg4_informal_exchama = ifelse(fin_loan_shylocks == 1 | fin_loan_ff == 1 | fin_loan_shopkeepercash == 1 | fin_loan_shopkeepergoods == 1, 1, 0), 
      
    ) %>%
    
    
    # -- Uses savings or credit for mainly for productive purposes/ long-term assets

    mutate_at(vars(matches("F1i[A-Z]$")), ~ ifelse(. %in% sav_uses, 1, 0)) %>%
    mutate(mfhi_3_2_sav = ifelse(rowSums(.[grepl("F1i[A-Z]$", names(.))]) > 0, 1, 0)) %>%
    
    mutate_at(vars(matches("E1iii[A-Z]$")), ~ ifelse(. %in% crd_uses, 1, 0)) %>%
    mutate(mfhi_3_2_crd = ifelse(rowSums(.[grepl("E1iii[A-Z]$", names(.))]) > 0, 1, 0)) %>%
    
    mutate(mfhi_3_2 = ifelse(mfhi_3_2_sav == 1 | mfhi_3_2_crd == 1, 1, 0)) %>%
    
    # --- Number of positive outcomes: 
    mutate(
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
    ) %>% 
    
    dummy_cols(select_columns = c("hh_urbrur", "hh_geo", "hh_geo_3", "resp_age_group2", "resp_gender_group", "resp_age_group", "resp_edu_group", "resp_edu_group2", "resp_live_group", "resp_live_group2", "resp_inc_group", "goals_group")) %>%
    
    select(starts_with(c("year", "sample_", "resp_", "hh_", "mfhi_", "fin_", "pfh_", "fb_", "fl_", "sc_", "goals_", "fin_", "B1", "n_", "aw", "w_")))
  
  return(data)
  
}
