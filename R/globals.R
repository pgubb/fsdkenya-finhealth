
########################################################################
# Global parameters  --------------------------
#####################################################################################


select <- dplyr::select

SOURCE <- "Source: Author's own calculations using the 2024 FinAccess survey."
SOURCE_TS <- "Source: Author's own calculations using the 2016, 2019, 2021 and 2024 FinAccess survey."
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
  "mfhi_1_3" = "Financial health outcomes/ Managing day-to-day: Food security: Never went without food due to lack of money in past 12 months", 
  
  "mfhi_nf_p1mo_cp_hou" = "Finanical health outcomes/ Managing day-to-day: Housing expenses: Able to pay in past 1 month: Yes (can pay)", 
  "mfhi_nf_p1mo_np_hou" = "Finanical health outcomes/ Managing day-to-day: Housing expenses: Able to pay in past 1 month: No (unable to pay)", 
  "mfhi_nf_p1mo_napp_hou" = "Finanical health outcomes/ Managing day-to-day: Housing expenses: Able to pay in past 1 month: Not applicable", 
  
  "mfhi_nf_p1mo_cp_enr" = "Finanical health outcomes/ Managing day-to-day: Energy expenses: Able to pay in past 1 month: Yes (can pay)", 
  "mfhi_nf_p1mo_np_enr" = "Finanical health outcomes/ Managing day-to-day: Energy expenses: Able to pay in past 1 month: No (unable to pay)", 
  "mfhi_nf_p1mo_napp_enr" = "Finanical health outcomes/ Managing day-to-day: Energy expenses: Able to pay in past 1 month: Not applicable", 
  
  "mfhi_nf_p1mo_cp_wtr" = "Financial health outcomes/ Managing day-to-day: Water expenses: Able to pay in past 1 month: Yes (can pay)", 
  "mfhi_nf_p1mo_np_wtr" = "Financial health outcomes/ Managing day-to-day: Water expenses: Able to pay in past 1 month: No (unable to pay)", 
  "mfhi_nf_p1mo_napp_wtr" = "Financial health outcomes/ Managing day-to-day: Water expenses: Able to pay in past 1 month: Not applicable", 
  
  "mfhi_nf_p1mo_cp_trn" = "Financial health outcomes/ Managing day-to-day: Transportation expenses: Able to pay in past 1 month: Yes (can pay)", 
  "mfhi_nf_p1mo_np_trn" = "Financial health outcomes/ Managing day-to-day: Transportation expenses: Able to pay in past 1 month: No (unable to pay)", 
  "mfhi_nf_p1mo_napp_trn" = "Financial health outcomes/ Managing day-to-day: Transportation expenses: Able to pay in past 1 month: Not applicable", 
  
  "mfhi_nf_p12mos_cp_sf" = "Financial health outcomes/ Managing day-to-day: School fees: Able to pay in past 12 months: Yes (can pay)", 
  "mfhi_nf_p12mos_np_sf" = "Financial health outcomes/ Managing day-to-day: School fees: Able to pay in past 12 months: No (unable to pay)", 
  "mfhi_nf_p12mos_napp_sf" = "Financial health outcomes/ Managing day-to-day: School fees: Able to pay in past 12 months: Not applicable",
  "mfhi_nf_p12mos_dkr_sf" = "Financial health outcomes/ Managing day-to-day: School fees: Able to pay in past 12 months: Don't know/refused",
  
  "mfhi_nf_tot_app" = "Financial health outcomes/ Managing day-to-day: Core non-food expenses: Total applicable (N)", 
  "mfhi_nf_tot_cp" = "Financial health outcomes/ Managing day-to-day: Core non-food expenses: Total expenses that respondent can pay (N)", 
  
  "mfhi_nf_secure" = "Financial health outcomes/ Managing day-to-day: Core non-food expenses: Can cover all applicable core non-food expenses in past 1 month", 
  
  "mfhi_sf_p12mos_sh_often" = "Financial health outcomes/ Managing day-to-day: School fees: Child sent home from school in past 12 months due to unpaid school fees: Often", 
  "mfhi_sf_p12mos_sh_some" = "Financial health outcomes/ Managing day-to-day: School fees: Child sent home from school in past 12 months due to unpaid school fees: Sometimes", 
  "mfhi_sf_p12mos_sh_nvr" = "Financial health outcomes/ Managing day-to-day: School fees: Child sent home from school in past 12 months due to unpaid school fees: Never", 
  "mfhi_sf_p12mos_sh_napp" = "Financial health outcomes/ Managing day-to-day: School fees: Child sent home from school in past 12 months due to unpaid school fees: Not applicable", 
  "mfhi_sf_p12mos_sh_dkr" = "Financial health outcomes/ Managing day-to-day: School fees: Child sent home from school in past 12 months due to unpaid school fees: Don't know/refused", 
  
  "mfhi_mc_p12mos_wwo_often" = "Financial health outcomes/Coping with risk: Medical expenses: Gone without medicine or medical treatment: Often", 
  "mfhi_mc_p12mos_wwo_some" = "Financial health outcomes/Coping with risk: Medical expenses: Gone without medicine or medical treatment: Sometimes", 
  "mfhi_mc_p12mos_wwo_nvr" = "Financial health outcomes/Coping with risk: Medical expenses: Gone without medicine or medical treatment: Never", 
  "mfhi_mc_p12mos_wwo_napp" = "Financial health outcomes/Coping with risk: Medical expenses: Gone without medicine or medical treatment: Not applicable", 
  "mfhi_mc_p12mos_wwo_dkr" = "Financial health outcomes/Coping with risk: Medical expenses: Gone without medicine or medical treatment: Don't know/refused", 
  
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

  "mfhi_ef_secure" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise 1/20 of GNI per capita in 30 days without any difficulty", 
  "mfhi_2_2" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise 1 month of typical expenditure in 3 days: Yes", 
  "mfhi_2_3" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Keeps money aside for emergencies: Yes", 
  
  "mfhi_ef_30d_srcborr_p_difany" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise 1/20 of GNI per capita in 30 days: Borrowing",
  "mfhi_ef_30d_srcsav_p_difany" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise 1/20 of GNI per capita in 30 days: Savings",
  "mfhi_ef_30d_srcasst_p_difany" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise 1/20 of GNI per capita in 30 days: Selling assets",
  "mfhi_ef_30d_srcff_p_difany" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise 1/20 of GNI per capita in 30 days: Assistance from social network",
  "mfhi_ef_30d_srccut_p_difany" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise 1/20 of GNI per capita in 30 days: Cutting back expenses",
  "mfhi_ef_30d_srcwrk_p_difany" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise 1/20 of GNI per capita in 30 days: Working more/additional jobs",
  "mfhi_ef_30d_srcinc_p_difany" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise 1/20 of GNI per capita in 30 days: From regular income",
  "mfhi_ef_30d_srcoth_p_difany" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise 1/20 of GNI per capita in 30 days: Other",
  
  "mfhi_ef_3d_srcany_dkr" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise KSH 3,500 (Rural) or KSH 6,000 (Urban) in 3 days: Don't know/refused", 
  "mfhi_ef_3d_srcany_np"  = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise KSH 3,500 (Rural) or KSH 6,000 (Urban) in 3 days: No", 
  "mfhi_ef_3d_srcany_p" = "Financial health outcomes/ Coping with risk: Access to emergency funds: Can raise KSH 3,500 (Rural) or KSH 6,000 (Urban) in 3 days: Yes", 
  
  "mfhi_i_pc_p12mos_lndhou" = "Financial health outcomes/ Capital investments: Physical assets: Purchased land, house or materials to improve house: Yes", 
  "mfhi_i_pc_p12mos_nonfarmliv" = "Financial health outcomes/ Capital investments: Physical assets: Purchased machinery, vehicles, equipment, tools for business or self-employment activity: Yes",
  "mfhi_i_pc_p12mos_farm" = "Financial health outcomes/ Capital investments: Physical assets: Purchased machinery, vehicles, equipment or tools for farming or raising livestock: Yes", 
  "mfhi_i_pc_p12mos_liv" = "Financial health outcomes/ Capital investments: Physical assets: Purchased livestock: Yes", 
  "mfhi_i_pc_p12mos_farmliv" = "Financial health outcomes/ Capital investments: Physical assets: Purchased machinery, vehicles, equipment, tools or livestock for farming or raising livestock: Yes", 
  "mfhi_i_pc_p12mos_any" = "Financial health outcomes/ Capital investments: Physical assets: Purchased any of the above physical assets: Yes",  
  
  "mfhi_i_fc_p12mos_pen" = "Financial health outcomes/ Capital investments: Financial assets: Contributed to a pension or individual retirement account: Yes", 
  "mfhi_i_fc_p12mos_lts" = "Financial health outcomes/ Capital investments: Financial assets: Contributed to long-term savings accounts: Yes", 
  "mfhi_i_fc_p12mos_sec" = "Financial health outcomes/ Capital investments: Financial assets: Purchased securities (stocks or bonds): Yes", 
  "mfhi_i_fc_p12mos_any" = "Financial health outcomes/ Capital investments: Financial assets: Contributed to- or purchased- any of the above financial assets: Yes", 
   
  "mfhi_i_pcfc_p12mos_any" = "Financial health outcomes/ Capital investments: Capital investments: Purchased- or contributed to- physical or financial assets that support production, productivity or long-term financial security", 
  
  "mfhi_i_hc_p12mos_any" = "Financial health outcomes/ Capital investments: Human capital: Paid for tuition for education or training for yourself or any family member: Yes",
  
  "mfhi_i_p12mos_any" = "Financial health outcomes/ Capital investments: Capital investments: Purchased- or contributed to- assets that support production, productivity or long-term financial security", 
  
  "mfhi_3_2" = "Financial health outcomes/ Capital investment: Investing for the future: Uses savings or credit to invest in long-term assets: Yes", 
  
  "mfhi_score_md2d" = "Financial health outcomes/ Summary: MFHI (discrete): Managing day to day: Score (mean)", 
  "mfhi_score_risk" = "Financial health outcomes/ Summary: MFHI (discrete): Coping with risk: Score (mean)", 
  "mfhi_score_inv" = "Financial health outcomes/ Summary: MFHI (discrete): Investing in capital: Score (mean)", 
  
  "mfhi_score_overall" = "Financial health outcomes/ Summary: MFHI (discrete): Overall: Score (mean)", 
  
  "mfhi_score_md2d_c" = "Financial health outcomes/ Summary: MFHI (gradated): Managing day to day: Score (mean)", 
  "mfhi_score_risk_c" = "Financial health outcomes/ Summary: MFHI (gradated): Coping with risk: Score (mean)", 
  "mfhi_score_inv_c" = "Financial health outcomes/ Summary: MFHI (gradated): Investing in capital: Score (mean)",
  
  "mfhi_score_overall_c" =  "Financial health outcomes/ Summary: MFHI (gradated): Overall: Score (mean)", 
  
  "mfhi_score_hi" = "Financial health outcomes/ Summary: MFHI (discrete): Score category: High (0.6-1)", 
  "mfhi_score_med" = "Financial health outcomes/ Summary: MFHI (discrete): Score category: Medium (0.3-0.6)", 
  "mfhi_score_low" = "Financial health outcomes/ Summary: MFHI (discrete): Score category: Low (0-0.3)", 
  
  "mfhi_score_hi_c" = "Financial health outcomes/ Summary: MFHI (gradated): Score category: High (0.6-1)", 
  "mfhi_score_med_c" = "Financial health outcomes/ Summary: MFHI (gradated): Score category: Medium (0.3-0.6)", 
  "mfhi_score_low_c" = "Financial health outcomes/ Summary: MFHI (gradated): Score category: Low (0-0.3)", 
  
  "mfhi_tc_0" = "Time consistent MFHI indicators: Total true indicators (of 5): None",                         
  "mfhi_tc_any1" = "Time consistent MFHI indicators: Total true indicators (of 5): Any one",                          
  "mfhi_tc_any2" = "Time consistent MFHI indicators: Total true indicators (of 5): Any two",                       
  "mfhi_tc_any3" = "Time consistent MFHI indicators: Total true indicators (of 5): Any three",                       
  "mfhi_tc_any4" = "Time consistent MFHI indicators: Total true indicators (of 5): Any four",                        
  "mfhi_tc_all5"= "Time consistent MFHI indicators: Total true indicators (of 5): All five", 
  
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
  "pfh_finstatus_dkr" = "Financial health perceptions: Change assessment: Change in financial status since last year: Don't know/refused",
  
  "fin_status_impr" = "Financial health perceptions: Change assessment: Change in financial status since last year: Improved", 
  "fin_status_worse" = "Financial health perceptions: Change assessment: Change in financial status since last year: Worsened", 
  
  "pfh_score_overall" = "Financial health perceptions: Overall score: Score [Min = 0, Max = 1]", 
  
  "hh_urbrur_Urban" = "Household characteristics: HH location: Urban", 
  "hh_size_all_c" = "Household characteristics: HH size: Household size (mean centered)", 
  "resp_gender_group_wmn" = "Respondent characteristics: Demographics: Women", 
  "resp_age_yrs_c_5" = "Respondent characteristics: Demographics: Respondent age (mean centered, +5 years)", 
  "resp_edu_group2_pri" = "Respondent characteristics: Educational attainment: Primary-level", 
  "resp_edu_group2_sec" = "Respondent characteristics: Educational attainment: Secondary-level", 
  "resp_edu_group2_trt" = "Respondent characteristics: Educational attainment: Tertiary-level", 

  "resp_live_group_empl" = "Respondent characteristics: Economic: Primary income source, Employment", 
  "resp_live_group_owbs" = "Respondent characteristics: Economic: Primary income source, Own business", 
  "resp_live_group_farm" = "Respondent characteristics: Economic: Primary income source, Farming",  
  "resp_live_group_cwrk" = "Respondent characteristics: Economic: Primary income source, Casual work", 
  "resp_live_group_trns" = "Respondent characteristics: Economic: Primary income source, Transfers", 
  "resp_live_group_othr" = "Respondent characteristics: Economic: Primary income source, Other",
  
  "resp_live_employment_str" = "Respondent characteristics: Economic: Primary income from employment or other", 
  
  "resp_income" = "Respondent characteristics: Economic: Personal monthly income (KSh)", 
  "resp_income_c_1000" = "Respondent characteristics: Economic: Personal monthly income [mean centered, +KSh 1,000]", 
  "resp_income_c_5000" = "Respondent characteristics: Economic: Personal monthly income [mean centered, +KSh 5,000]", 
  "resp_income_var" = "Respondent characteristics: Economic: Yearly variation in personal monthly income (%)",
  "resp_live_trnsf_in" = "Respondent characteristics: Economic: Income source, Government cash transfer program or pension", 
  "fb_financial_plan" = "Respondent characteristics: Financial behaviors: Has a spending plan", 
  "mfhi_1_2" = "Respondent characteristics: Financial behaviors: Has a spending plan", 
  "fb_gambling" = "Respondent characteristics: Financial behaviors: Has gambled in the past 12 months", 
  "fb_emergency_savings" = "Respondent characteristics: Financial behaviors: Currently keeps money aside for emergencies", 
  "fl_literacy_all" = "Respondent characteristics: Financial literacy: Understand interest, inflation and can understand SMS", 
  "sc_ff_finhelp" = "Respondent characteristics: Social capital: Have people who can provide financial support if needed", 
  
  "goals_group_educ" = "Respondent characteristics: Personal values: Most important goal: Educating yourself or your family", 
  "goals_group_food" = "Respondent characteristics: Personal values: Most important goal: Putting food on the table",   
  "goals_group_live" = "Respondent characteristics: Personal values: Most important goal: Getting or advancing job, career, livelihood", 
  "goals_group_other" = "Respondent characteristics: Personal values: Most important goal: Other", 
  
  "fin_reg_mobilemoney" = "Financial Access/Usage: Transaction devices: Registered account/member: Mobile money", 
  "fin_reg_mobilebanking" = "Financial Access/Usage: Transaction devices: Registered account/member: Mobile banking", 
  "fin_reg_bankmfb" = "Financial Access/Usage: Transaction devices: Registered account/member: Bank or microfinance bank", 
  "fin_reg_sacco" = "Financial Access/Usage: Transaction devices: Registered account/member: SACCO", 
  "fin_reg_chama" = "Financial Access/Usage: Transaction devices: Registered account/member: Chama", 
  
  "fin_sav_agg4_formal_trd" = "Financial Access/Usage: Savings: Any formal, traditional account: Currently uses", 
  "fin_sav_agg4_formal_dig" =  "Financial Access/Usage: Savings: Any formal, digital account: Currently uses",           
  "fin_sav_agg4_informal" = "Financial Access/Usage: Savings: Any informal strategy: Currently uses",             
  "fin_sav_agg4_cash" =  "Financial Access/Usage: Savings: Secret place: Currently uses", 
  
  "fin_loan_agg4_formal_trd" = "Financial Access/Usage: Credit: Any formal traditional loan: Currently uses",        
  "fin_loan_agg4_formal_dig"  = "Financial Access/Usage: Credit: Any formal digital loan: Currently uses",        
  "fin_loan_agg4_informal"  = "Financial Access/Usage: Credit: Any informal loan: Currently uses",          
  "fin_loan_agg4_other"  = "Financial Access/Usage: Credit: Other loan: Currently uses", 
  
  "fin_sav_bankmfb" = "Financial Access/Usage: Savings: Bank or microfinance bank: Currently uses",         
  "fin_sav_mfi"= "Financial Access/Usage: Savings: Microfinance institution (credit only): Currently uses",                 
  "fin_sav_mobilebanking" ="Financial Access/Usage: Savings: Mobile banking: Currently uses", 
  "fin_sav_mobilemoney"  ="Financial Access/Usage: Savings: Mobile money: Currently uses",     
  "fin_sav_sacco"  ="Financial Access/Usage: Savings: SACCO: Currently uses",              
  "fin_sav_chama"  ="Financial Access/Usage: Savings: Chama: Currently uses",                   
  "fin_sav_ff"  = "Financial Access/Usage: Savings: With friends/family: Currently uses",                   
  "fin_sav_secret" = "Financial Access/Usage: Savings: Secret hiding place: Currently uses",          
  "fin_sav_digapp" ="Financial Access/Usage: Savings: Digital app: Currently uses",           
  "fin_loan_bank"  ="Financial Access/Usage: Credit: Commercial bank: Currently uses",             
  "fin_loan_mfb"  ="Financial Access/Usage: Credit: Microfinance bank: Currently uses",                 
  "fin_loan_overdraft" = "Financial Access/Usage: Credit: Overdraft: Currently uses",          
  "fin_loan_creditcard" = "Financial Access/Usage: Credit: Credit card: Currently uses",      
  "fin_loan_mobilebanking" = "Financial Access/Usage: Credit: Mobile banking: Currently uses",    
  "fin_loan_mobilemoney"= "Financial Access/Usage: Credit: Mobile money: Currently uses",        
  "fin_loan_sacco" = "Financial Access/Usage: Credit: SACCO: Currently uses",                 
  "fin_loan_shylocks" = "Financial Access/Usage: Credit: Shylock: Currently uses",        
  "fin_loan_chama" = "Financial Access/Usage: Credit: Chama: Currently uses",           
  "fin_loan_gov" = "Financial Access/Usage: Credit: Government: Currently uses",               
  "fin_loan_hustler"= "Financial Access/Usage: Credit: Hustler fund: Currently uses",         
  "fin_loan_ff" = "Financial Access/Usage: Credit: Friends and family: Currently uses",                  
  "fin_loan_shopkeepercash" = "Financial Access/Usage: Credit: Shopkeeper cash loan: Currently uses",   
  "fin_loan_shopkeepergoods" = "Financial Access/Usage: Credit: Shopkeeper goods on credit: Currently uses",  
  "fin_loan_buyer"= "Financial Access/Usage: Credit: Buyer: Currently uses",             
  "fin_loan_hirepurchase"   = "Financial Access/Usage: Credit: Hire purchase: Currently uses",  
  "fin_loan_insurance" = "Financial Access/Usage: Credit: Insurance: Currently uses",         
  "fin_loan_mortgage" = "Financial Access/Usage: Credit: Mortgage: Currently uses"

)

GROUPS <- c("resp_all" = "National", 
            "resp_gender_fct" = "Gender", 
            "resp_age_group2_fct" = "Age (categories)", 
            "hh_county" = "County of residence",
            "hh_urbrur" = "Household location", 
            "resp_edu_group3_fct" = "Educational attainment",
            "resp_income_agg3_str" = "Income group", 
            "resp_live_group_fct" = "Livelihood", 
            "resp_age_group_fct" = "Age group")

GROUPCAT_LEVELS <- c("All adults", "Men", "Women", "< 25 yrs", "25-64 yrs", "65+ yrs", "Urban", "Rural", "Poorest 40%", "Middle 40%","Richest 20%", 
                     "Employed", "Own business", "Agriculture", "Casual", "Transfers", "Other", "Less then complete primary", "Some or complete secondary", "Some or complete tertiary", 
                     "[18-25)", "[25-45)", "[45-65)", "[25-65)", "[65+]")
