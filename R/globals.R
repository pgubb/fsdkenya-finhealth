
########################################################################
# Global parameters  --------------------------
#####################################################################################


select <- dplyr::select

SOURCE <- c("Source: 2024 FinAccess survey")
CAP_WRAP <- 145 
TI_WRAP <- 70

INDICATORS <- c(
  
  "mfhi_f_p12mos_wwo_tiany" = "Financial health outcomes/ Managing day-to-day: Food security: Went without food in past 12 months due to lack of money: Any number of times", 
  "mfhi_f_p12mos_wwo_yes" = "Financial health outcomes/ Managing day-to-day: Food security: Went without food in past 12 months due to lack of money: Yes", 
  "mfhi_f_p12mos_wwo_no" = "Financial health outcomes/ Managing day-to-day: Food security: Went without food in past 12 months due to lack of money: No", 
  "mfhi_f_p12mos_wwo_dkr" = "Financial health outcomes/ Managing day-to-day: Food security: Went without food in past 12 months due to lack of money: Don't know/refused", 
  "mfhi_f_p12mos_wwo_napp" = "Financial health outcomes/ Managing day-to-day: Food security: Went without food in past 12 months due to lack of money: Not applicable", 

  "mfhi_f_p1mo_wwo_tiany" = "Financial health outcomes/ Managing day-to-day: Food security: Went without food in past 1 month due to lack of money: Any number of times", 
  
  "mfhi_f_p1mo_wwo_dkr" = "Financial health outcomes/ Managing day-to-day: Food security: Went without food in past 1 month due to lack of money: Don't know/refused", 
  "mfhi_f_p1mo_wwo_no" = "Financial health outcomes/ Managing day-to-day: Food security: Went without food in past 1 month due to lack of money: No",
  "mfhi_f_p1mo_wwo_yes" = "Financial health outcomes/ Managing day-to-day: Food security: Went without food in past 1 month due to lack of money: Yes",
  
  "mfhi_f_p1mo_wwo_ti12" = "Financial health outcomes/ Managing day-to-day: Food security: Number of times went without food in past 1 month due to lack of money: 1-2 times", 
  "mfhi_f_p1mo_wwo_ti310" = "Financial health outcomes/ Managing day-to-day: Food security: Number of times went without food in past 1 month due to lack of money: 3-10 times", 
  "mfhi_f_p1mo_wwo_ti10" = "Financial health outcomes/ Managing day-to-day: Food security: Number of times went without food in past 1 month due to lack of money: More than 10 times", 
  "mfhi_f_p1mo_wwo_napp" = "Financial health outcomes/ Managing day-to-day: Food security: Number of times went without food in past 1 month due to lack of money: Not applicable", 
  
  "mfhi_f_secure" = "Financial health outcomes/ Managing day-to-day: Food security: Never went without food due to lack of money in past 12 months", 
  
  "mfhi_nf_p1mo_cp_hou" = "Finanical health outcomes/ Managing day-to-day: Housing expenses: Able to pay in past 1 month: Yes", 
  "mfhi_nf_p1mo_np_hou" = "Finanical health outcomes/ Managing day-to-day: Housing expenses: Able to pay in past 1 month: No", 
  "mfhi_nf_p1mo_napp_hou" = "Finanical health outcomes/ Managing day-to-day: Housing expenses: Able to pay in past 1 month: Not applicable", 
  
  "mfhi_nf_p1mo_cp_enr" = "Finanical health outcomes/ Managing day-to-day: Energy expenses: Able to pay in past 1 month: Yes", 
  "mfhi_nf_p1mo_np_enr" = "Finanical health outcomes/ Managing day-to-day: Energy expenses: Able to pay in past 1 month: No", 
  "mfhi_nf_p1mo_napp_enr" = "Finanical health outcomes/ Managing day-to-day: Energy expenses: Able to pay in past 1 month: Not applicable", 
  
  "mfhi_nf_p1mo_cp_wtr" = "Financial health outcomes/ Managing day-to-day: Water expenses: Able to pay in past 1 month: Yes", 
  "mfhi_nf_p1mo_np_wtr" = "Financial health outcomes/ Managing day-to-day: Water expenses: Able to pay in past 1 month: No", 
  "mfhi_nf_p1mo_napp_wtr" = "Financial health outcomes/ Managing day-to-day: Water expenses: Able to pay in past 1 month: Not applicable", 
  
  "mfhi_nf_p1mo_cp_trn" = "Financial health outcomes/ Managing day-to-day: Transportation expenses: Able to pay in past 1 month: Yes", 
  "mfhi_nf_p1mo_np_trn" = "Financial health outcomes/ Managing day-to-day: Transportation expenses: Able to pay in past 1 month: No", 
  "mfhi_nf_p1mo_napp_trn" = "Financial health outcomes/ Managing day-to-day: Transportation expenses: Able to pay in past 1 month: Not applicable", 
  
  "mfhi_nf_tot_app" = "Financial health outcomes/ Managing day-to-day: Core non-food expenses: Total applicable (N)", 
  "mfhi_nf_tot_cp" = "Financial health outcomes/ Managing day-to-day: Core non-food expenses: Total expenses that respondent can pay (N)", 
  
  "mfhi_nf_secure" = "Financial health outcomes/ Managing day-to-day: Core non-food expenses: Can cover all applicable core non-food expenses in past 1 month", 
  
  "mfhi_d_p3mo_stress_yes" = "Financial health outcomes/ Managing day-to-day: Debt: Reduced food expenses to repay debt in past 3 months: Yes", 
  "mfhi_d_p3mo_stress_no" = "Financial health outcomes/ Managing day-to-day: Debt: Reduced food expenses to repay debt in past 3 months: No", 
  "mfhi_d_p3mo_stress_napp" = "Financial health outcomes/ Managing day-to-day: Debt: Reduced food expenses to repay debt in past 3 months: Not applicable (Does not owe money)", 
  "mfhi_d_p3mo_stress_dkr" = "Financial health outcomes/ Managing day-to-day: Debt: Reduced food expenses to repay debt in past 3 months: Don't know/refused", 
  
  "mfhi_d_secure" = "Financial health outcomes/ Managing day-to-day: Debt: Can service debt without reducing food spending", 
    
  "mfhi_ef_30d_srcany_np" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise 1/20 of GNI per capita in 30 days: Not possible", 
  "mfhi_ef_30d_srcany_p_difany" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise 1/20 of GNI per capita in 30 days: Yes, any level of difficulty", 
  "mfhi_ef_30d_srcany_p_difvry" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise 1/20 of GNI per capita in 30 days: Yes, very difficult", 
  "mfhi_ef_30d_srcany_p_difsme" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise 1/20 of GNI per capita in 30 days: Yes, somewhat difficult", 
  "mfhi_ef_30d_srcany_p_difnot" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise 1/20 of GNI per capita in 30 days: Yes, not at all difficult", 
  "mfhi_ef_30d_srcany_p_difsmeornot" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise 1/20 of GNI per capita in 30 days: Yes, somewhat difficult or not at all difficult", 
  "mfhi_ef_30d_srcany_dkr" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise 1/20 of GNI per capita in 30 days: Don't know/refused", 

  "mfhi_ef_secure" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise 1/20 of GNI per capita in 30 days without major difficulty", 
  
  "mfhi_i_pc_p12mos_lndhou" = "Financial health outcomes/ Capital investments: Physical assets: Purchased land, house or materials to improve house in past 12 months: Yes", 
  "mfhi_i_pc_p12mos_nonfarmliv" = "Financial health outcomes/ Capital investments: Physical assets: Purchased machinery, vehicles, equipment, tools for business or self-employment activity in past 12 months: Yes", 
  "mfhi_i_pc_p12mos_farmliv" = "Financial health outcomes/ Capital investments: Physical assets: Purchased machinery, vehicles, equipment or tools for farming or raising livestock in past 12 months: Yes", 
  "mfhi_i_pc_p12mos_any" = "Financial health outcomes/ Capital investments: Physical assets: Purchased any physical asset that supports production, productivity or long-term security in past 12 months: Yes",  
  
  "mfhi_i_fc_p12mos_pen" = "Financial health outcomes/ Capital investments: Financial assets: Contributed to a pension or individual retirement account: Yes", 
  "mfhi_i_fc_p12mos_lts" = "Financial health outcomes/ Capital investments: Financial assets: Contributed to long-term savings accounts: Yes", 
  "mfhi_i_fc_p12mos_sec" = "Financial health outcomes/ Capital investments: Financial assets: Purchased securities (stocks or bonds): Yes", 
  "mfhi_i_fc_p12mos_any" = "Financial health outcomes/ Capital investments: Financial assets: Contributed to- or purchased- any financial asset that support long-term security: Yes", 
  
  "mfhi_i_hc_p12mos_any" = "Financial health outcomes/ Capital investments: Human capital: Paid for tuition for education or training for yourself or any family member: Yes",
  
  "mfhi_i_p12mos_any" = "Financial health outcomes/ Capital investments: Capital investments: Purchased- or contributed to- assets that support production, productivity or long-term financial security", 
  
  "mfhi_score_md2d" = "Financial health outcomes/ Summary: MFHI (discrete): Managing day to day: Score (mean)", 
  "mfhi_score_risk" = "Financial health outcomes/ Summary: MFHI (discrete): Coping with risk: Score (mean)", 
  "mfhi_score_inv" = "Financial health outcomes/ Summary: MFHI (discrete): Investing in capital: Score (mean)", 
  
  "mfhi_score_md2d_c" = "Financial health outcomes/ Summary: MFHI (gradated): Managing day to day: Score (mean)", 
  "mfhi_score_risk_c" = "Financial health outcomes/ Summary: MFHI (gradated): Coping with risk: Score (mean)", 
  "mfhi_score_inv_c" = "Financial health outcomes/ Summary: MFHI (gradated): Investing in capital: Score (mean)",
  
  "mfhi_score_hi" = "Financial health outcomes/ Summary: MFHI (discrete): Score category: High (0.6-1)", 
  "mfhi_score_med" = "Financial health outcomes/ Summary: MFHI (discrete): Score category: Medium (0.3-0.6)", 
  "mfhi_score_low" = "Financial health outcomes/ Summary: MFHI (discrete): Score category: Low (0-0.3)", 
  
  "mfhi_score_hi_c" = "Financial health outcomes/ Summary: MFHI (gradated): Score category: High (0.6-1)", 
  "mfhi_score_med_c" = "Financial health outcomes/ Summary: MFHI (gradated): Score category: Medium (0.3-0.6)", 
  "mfhi_score_low_c" = "Financial health outcomes/ Summary: MFHI (gradated): Score category: Low (0-0.3)", 
  
  "pfh_dailyneeds_vw" = "Financial health perceptions: Managing day to day: Worry about paying daily expenses: Very worried", 
  "pfh_dailyneeds_sw" = "Financial health perceptions: Managing day to day: Worry about paying daily expenses: Somewhat worried", 
  "pfh_dailyneeds_nw" = "Financial health perceptions: Managing day to day: Worry about paying daily expenses: Not worried at all",  
  "pfh_dailyneeds_dkr" = "Financial health perceptions: Managing day to day: Worry about paying daily expenses: Don't know/refused", 
  
  "pfh_debt_vw" = "Financial health perceptions: Managing day to day: Worry about paying off existing debts: Very worried",  
  "pfh_debt_sw" = "Financial health perceptions: Managing day to day: Worry about paying off existing debts: Somewhat worried", 
  "pfh_debt_nw" = "Financial health perceptions: Managing day to day: Worry about paying off existing debts: Not worried at all", 
  "pfh_debt_napp" = "Financial health perceptions: Managing day to day: Worry about paying off existing debts: Not applicable (No debt)", 
  "pfh_debt_dkr" = "Financial health perceptions: Managing day to day: Worry about paying off existing debts: Don't know/refused", 
  
  "pfh_goals_vc" = "Financial health perceptions: Confidence: Confidence in meeting long-term goals: Very confident", 
  "pfh_goals_sc" = "Financial health perceptions: Confidence: Confidence in meeting long-term goals: Somewhat confident", 
  "pfh_goals_nc" = "Financial health perceptions: Confidence: Confidence in meeting long-term goals: Not confident at all", 
  "pfh_goals_dkr" = "Financial health perceptions: Confidence: Confidence in meeting long-term goals: Don't know/refused", 
  
  "pfh_finstatus_imp" = "Financial health perceptions: Change assessment: Change in financial status since last year: Improved", 
  "pfh_finstatus_sam" = "Financial health perceptions: Change assessment: Change in financial status since last year: Remained the same",  
  "pfh_finstatus_wor" = "Financial health perceptions: Change assessment: Change in financial status since last year: Worsened", 
  "pfh_finstatus_dkr" = "Financial health perceptions: Change assessment: Change in financial status since last year: Don't know/refused"
  
)

GROUPS <- c("resp_all" = "National", 
            "resp_gender" = "Gender", 
            "resp_age_agg3_str" = "Age (categories)", 
            "hh_urbrur" = "Household location", 
            "resp_income_agg3_str" = "Income group")

GROUPCAT_LEVELS <- c("All adults", "Men", "Women", "< 25 yrs", "25-64 yrs", "65+ yrs", "Urban", "Rural", "Poorest 40%", "Middle 40%","Richest 20%")


