
########################
#
########################

# Reading in data -------------
pub_data <- get_data(year = 2024)

# Creating a codebook with the names and descriptions of variables
codebook <- gen_varbook_vec(pub_data)

# Preparing new multidimensional financial health indicators

int_data <- prep_2024(pub_data)



#View(int_data %>% select(n_md2d, w_md2d, mfhi_f_secure,  mfhi_nf_secure, mfhi_d_p3mo_stress_no, mfhi_score_md2d))
#View(int_data %>% select(n_risk, w_risk, mfhi_ef_30d_srcany_p_difsmeornot, mfhi_score_risk))
#View(int_data %>% select(n_inv, w_inv, mfhi_i_p12mos_any, mfhi_score_inv))
#View(int_data %>% select(n_md2d, w_md2d, mfhi_score_md2d, n_risk, w_risk, mfhi_score_risk, n_inv, w_inv, mfhi_score_inv, mfhi_score_overall))
#View(int_data %>% select(n_md2d, w_md2d, w_md2d_adj, n_risk, w_risk, w_risk_adj, n_inv, w_inv, w_inv_adj, w_total, w_total_adj, mfhi_score_overall))
