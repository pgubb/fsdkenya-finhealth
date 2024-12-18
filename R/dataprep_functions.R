

get_data <- function(year) { 
  
  path <- "data"
  file <- glue("{path}/FA_{year}_public.dta")
  
  data <- read_dta(file)
  
}

#prep_resp_demo <- function(data) { }

inc_levels <- c("inc_1" = "< KSh 2,500",
                "inc_2" = "KSh 2,500 - 5,000",
                "inc_3" = "KSh 5,000 - 10,000",
                "inc_4" = "KSh 10,000 - 15,000",
                "inc_5" = "KSh 15,000+")

prep_2024 <- function(data) { 
  
  data <- data %>% 
    select(!starts_with("mfhi")) %>% 
    mutate(
      
      # Samplng variables --------------
      
      sample_psu = A07, # Primary sampling unit: cluster
      sample_weights = indWeight, # Sampling weights 
      
      # Household characteristics ----------------
      
      hh_urbrur = ifelse(A8 == 1, "Rural", "Urban"), 
      
      # Respondent characteristics -----------
      
      resp_all = "All adults",
      resp_gender = ifelse(A13 == 1, "Men", "Women"), 
      resp_age_agg3_str = case_when(
        A18 < 25 ~ "< 25 yrs", 
        A18 >= 25 & A18 < 65 ~ "25-64 yrs", 
        A18 >= 65 ~ "65+ yrs", 
      ), 
      resp_age_agg4_str = case_when(
        A18 < 25 ~ "< 25 yrs", 
        A18 >= 25 & A18 < 45 ~ "25-44 yrs", 
        A18 >= 45 & A18 < 65 ~ "45-64 yrs", 
        A18 >= 65 ~ "65+ yrs", 
      ), 
      resp_age_agg3 = case_when(
        A18 < 25 ~ "lt25", 
        A18 >= 25 & A18 < 65 ~ "2565", 
        A18 >= 65 ~ "mt65", 
      ), 
      resp_age_agg4 = case_when(
        A18 < 25 ~ "lt25", 
        A18 >= 25 & A18 < 45 ~ "2545", 
        A18 >= 45 & A18 < 65 ~ "4565", 
        A18 >= 65 ~ "mt65", 
      ), 
      
      # Respondent income last month
      resp_income = B3Ii,
      resp_income = ifelse(B3Ii %in% c(98,99), NA, resp_income),
      
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
      
      mfhi_nf_tot_yapp = mfhi_nf_p1mo_yapp_hou +  mfhi_nf_p1mo_yapp_enr +  mfhi_nf_p1mo_yapp_wtr +  mfhi_nf_p1mo_yapp_trn,
      # Note: 350 observations where expenditures do not apply 
      
      mfhi_nf_tot_cp = mfhi_nf_p1mo_cp_hou +  mfhi_nf_p1mo_cp_enr +  mfhi_nf_p1mo_cp_wtr +  mfhi_nf_p1mo_cp_trn,
      
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
      
      mfhi_ef_secure = mfhi_ef_30d_srcany_p_difsmeornot, 
      
      mfhi_ef_secure_c = case_when(
        mfhi_ef_30d_srcany_np == 1 ~ 0, 
        mfhi_ef_30d_srcany_p_difvry == 1 ~ 1/3, 
        mfhi_ef_30d_srcany_p_difsme == 1 ~ 2/3, 
        mfhi_ef_30d_srcany_p_difnot == 1 ~ 3/3
      ), 
      
      # Capital investments -------------
      # i = investing for the future
      # pc = physical capital / fc = financial capital / hc = human capital
      # p12mos = recall period is past 12 months
      # any 
      
      ## Need to check if these have "Don't know/refused" options: 
      # Physical capital 
      
      mfhi_i_pc_p12mos_lndhou = ifelse(B1L__1 == 1 |  B1L__2 == 1 |  B1L__3 == 1, 1, 0),
      mfhi_i_pc_p12mos_nonfarmliv = ifelse(B1L__4 == 1, 1, 0),
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
      
      # Human capital
      
      mfhi_i_hc_p12mos_any = ifelse(B1M__4 == 1, 1, 0),
      mfhi_i_hc_p12mos_allmiss = ifelse(is.na(B1M__4), 1, 0), 
      
      mfhi_i_hc_p12mos_any = ifelse(is.na(mfhi_i_hc_p12mos_any), 0, mfhi_i_hc_p12mos_any), 
      mfhi_i_hc_p12mos_any = ifelse(mfhi_i_hc_p12mos_allmiss == 1, NA, mfhi_i_hc_p12mos_any),
      
      # Any investments in physical, financial or human capital
      mfhi_i_p12mos_any = ifelse(mfhi_i_pc_p12mos_any == 1 | mfhi_i_fc_p12mos_any == 1 | mfhi_i_fc_p12mos_any == 1, 1, 0),
      mfhi_i_p12mos_any = ifelse(is.na(mfhi_i_p12mos_any), 0, mfhi_i_p12mos_any),
      mfhi_i_p12mos_any = ifelse(mfhi_i_pc_p12mos_allmiss == 1 & mfhi_i_fc_p12mos_allmiss == 1 & mfhi_i_hc_p12mos_allmiss == 1 , NA, mfhi_i_p12mos_any),
      
      mfhi_i_p12mos_c = (mfhi_i_pc_p12mos_any + mfhi_i_fc_p12mos_any + mfhi_i_fc_p12mos_any)/3,
      mfhi_i_p12mos_c = ifelse(mfhi_i_pc_p12mos_allmiss == 1 & mfhi_i_fc_p12mos_allmiss == 1 & mfhi_i_hc_p12mos_allmiss == 1 , NA, mfhi_i_p12mos_c),

      
      # Aggregation: Multidimensional financial health index -------------
      
      # Number of observations not missing 
      mfhi_f_secure_notmiss = ifelse(!is.na(mfhi_f_secure), 1, 0), 
      mfhi_nf_secure_notmiss = ifelse(!is.na(mfhi_nf_secure), 1, 0), 
      mfhi_d_secure_notmiss = ifelse(!is.na(mfhi_d_secure), 1, 0), 
      mfhi_ef_secure_notmiss = ifelse(!is.na(mfhi_ef_secure), 1, 0), 
      mfhi_i_p12mos_any_notmiss = ifelse(!is.na(mfhi_i_p12mos_any), 1, 0), 
      
      mfhi_md2d_notmiss = mfhi_f_secure_notmiss + mfhi_nf_secure_notmiss + mfhi_d_secure_notmiss, 
      mfhi_risk_notmiss = mfhi_ef_secure_notmiss, 
      mfhi_inv_notmiss = mfhi_i_p12mos_any_notmiss, 
      mfhi_all_N_notmiss = mfhi_f_secure_notmiss + mfhi_nf_secure_notmiss + mfhi_d_secure_notmiss + mfhi_ef_secure_notmiss + mfhi_i_p12mos_any_notmiss, 
      mfhi_all_N_miss = 5 - mfhi_all_N_notmiss, 
      
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
      w_total_adj = w_md2d_adj*n_md2d + w_risk_adj*n_risk + w_inv_adj*n_inv
      
    ) %>% 
        
    rowwise() %>% 

      mutate(
        mfhi_score_md2d = w_md2d_adj*sum(mfhi_f_secure, mfhi_nf_secure, mfhi_d_secure, na.rm = TRUE),
        mfhi_score_risk = w_risk_adj*sum(mfhi_ef_secure, na.rm = TRUE), 
        mfhi_score_inv = w_inv_adj*sum(mfhi_i_p12mos_any, na.rm = TRUE), 
        
        mfhi_score_md2d_c = w_md2d_adj*sum(mfhi_f_secure_c, mfhi_nf_secure_c, mfhi_d_secure, na.rm = TRUE),
        mfhi_score_risk_c = w_risk_adj*sum(mfhi_ef_secure_c, na.rm = TRUE), 
        mfhi_score_inv_c = w_inv_adj*sum(mfhi_i_p12mos_c, na.rm = TRUE)
        
      ) %>% 
    
      ungroup() %>% 
    
    mutate(
      
      # Weighted sum of all component indicators
      mfhi_score_overall = mfhi_score_md2d + mfhi_score_risk + mfhi_score_inv,
      mfhi_score_overall_c = mfhi_score_md2d_c + mfhi_score_risk_c + mfhi_score_inv_c,
      
      # Headcount ratios to calculate % of population with high, medium and low financial health scores
      
      mfhi_score_hi = ifelse(mfhi_score_overall > 0.6, 1, 0),
      mfhi_score_med = ifelse(mfhi_score_overall > 0.3 & mfhi_score_overall <= 0.6, 1, 0),
      mfhi_score_low = ifelse(mfhi_score_overall <= 0.3, 1, 0),
    
      mfhi_score_hi_c = ifelse(mfhi_score_overall > 0.6, 1, 0),
      mfhi_score_med_c = ifelse(mfhi_score_overall > 0.3 & mfhi_score_overall <= 0.6, 1, 0),
      mfhi_score_low_c = ifelse(mfhi_score_overall <= 0.3, 1, 0),
      
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
      
      pfh_score_overall = (pfh_score_dailyneeds + pfh_score_debt + pfh_score_goals + pfh_score_finstatus)/4
      
    ) %>% 
    dummy_cols(select_columns = c("resp_age_agg4", "resp_gender", "hh_urbrur")) %>%
    
    select(starts_with(c("sample_", "resp_", "hh_", "mfhi_", "pfh_", "B1", "n_", "w_")))
  
  return(data)
  
}

