

# Examining differences between discrete and gradated mfhi

View(FA2024 %>% 
       select(mfhi_f_secure, mfhi_nf_secure, mfhi_d_secure, mfhi_ef_secure, mfhi_i_p12mos_any, mfhi_score_overall, mfhi_score_hi,
              mfhi_f_secure_c, mfhi_nf_secure_c, mfhi_d_secure, mfhi_ef_secure_c, mfhi_i_p12mos_any_c, mfhi_score_overall_c, mfhi_score_hi_c)
     )



View(FA2024 %>% 
       select(mfhi_score_md2d, mfhi_score_risk, mfhi_score_inv, mfhi_score_overall, mfhi_score_hi, mfhi_score_md2d_c, mfhi_score_risk_c, mfhi_score_inv_c, mfhi_score_overall_c, mfhi_score_hi_c)
)





FA2024_raw <- get_data(2024)
varbook24 <- varbook_to_df(gen_varbook_vec(FA2024))
valubook24 <- gen_valuebook(FA2024)

sav_uses <- c(3:7, 9)
crd_uses <- c(2:3, 5:9, 11:13)

View(FA2024 %>% select(matches("F1i[A-Z]$")))

View(FA2024 %>% select(matches("E1iii[A-Z]$")))


test <- FA2024 %>% 
# -- Uses savings or credit for mainly for productive purposes/ long-term assets

mutate_at(vars(matches("F1i[A-Z]$")), ~ ifelse(. %in% sav_uses, 1, 0)) %>%
  mutate(mfhi_3_2_sav = ifelse(rowSums(.[grepl("F1i[A-Z]$", names(.))]) > 0, 1, 0)) %>%
  
  mutate_at(vars(matches("E1iii[A-Z]$")), ~ ifelse(. %in% crd_uses, 1, 0)) %>%
  mutate(mfhi_3_2_crd = ifelse(rowSums(.[grepl("E1iii[A-Z]$", names(.))]) > 0, 1, 0)) %>%
  
  mutate(mfhi_3_2 = ifelse(mfhi_3_2_sav == 1 | mfhi_3_2_crd == 1, 1, 0))

View(test %>% mutate(x = F1iA + F1iB + F1iC + F1iD + F1iE + F1iF + F1iG + F1iH + F1iI + F1iJ + F1iK + F1iL) %>% 
       select(F1iA, F1iB, F1iC, F1iD, F1iE, F1iF, F1iG, F1iH, F1iI, F1iJ, F1iK, F1iL, x, mfhi_3_2_sav))

View(test %>% mutate(x = F1iA + F1iB + F1iC + F1iD + F1iE + F1iF + F1iG + F1iH + F1iI + F1iJ + F1iK + F1iL) %>% 
       select(F1iA, F1iB, F1iC, F1iD, F1iE, F1iF, F1iG, F1iH, F1iI, F1iJ, F1iK, F1iL, x, mfhi_3_2_sav))

varbook24 <- varbook_to_df(gen_varbook_vec(FA2024))
valubook24 <- gen_valuebook(FA2024)


FA2021 <- get_data(2021)
varbook21 <- varbook_to_df(gen_varbook_vec(FA2021))
valubook21 <- gen_valuebook(FA2021)

View(FA2021 %>% select(matches("E1iii[A-Z]$")))




FA2019 <- get_data(2019)
varbook21 <- varbook_to_df(gen_varbook_vec(FA2019))
valubook21 <- gen_valuebook(FA2019)

View(FA2019 %>% select(matches("e1[a-zA-Z0-9]3")))
View(FA2019 %>% select(starts_with("f2_")))


FA2016 <- get_data(2016)
varbook21 <- varbook_to_df(gen_varbook_vec(FA2016))
valubook21 <- gen_valuebook(FA2016)




test <- FA2024 %>% mutate(fin_debt_total = rowSums(.[grepl("E1xi[A-Z]$", names(.))], na.rm = T)) %>% select(fin_debt_total, matches("E1xi[A-Z]$"))

mutate_at(vars(matches("E1xi[A-Z]$")), ~ ifelse(. %in% sav_uses, 1, 0)) %>%
  
  
  
# Examining debt indicators for women: 
  
indicators <- c("fin_loan_agg4_informal_exchama", "fin_debt_any", "fin_debt_inc", "fin_debt_high", "fin_debtservice_inc", "fin_debtservice_high", "mfhi_d_secure")
groups <- GROUPS[c("resp_all", "hh_urbrur", "resp_gender_fct", "resp_age_group2_fct", "resp_income_agg3_str")]
ests <- compute_summary_mainlevel_1g(indicators, groups, data = FA2024 %>% filter(resp_gender_group_wmn == 1), weights = "sample_weights", psu = "sample_psu", keep = NULL)
  
data_women <-  FA2024 %>% filter(resp_gender_group_wmn == 1)

m <- lm(fin_debt_inc ~ fin_reg_mobilemoney + fin_reg_mobilebanking + fin_reg_bankmfb + fin_reg_sacco + fin_reg_chama + resp_age_yrs_c_5 + resp_age_yrs_c_5_sq + 
          resp_income_c_5000 + resp_income_c_5000_2 + resp_income_var_c + resp_live_group_empl + resp_edu_group2_pri + resp_edu_group2_trt, data = data_women, weights = data_women$sample_weights)
summary(m)

# Correspondence between income quintiles and wealth quintiles 

xx <- FA2024 %>% mutate(resp_income_agg3_str = factor(resp_income_agg3_str, levels = c("Poorest 40%", "Middle 40%", "Richest 20%"), ordered = TRUE), 
                            resp_wealth_agg3_str = factor(resp_wealth_agg3_str, levels = c("Poorest 40%", "Middle 40%", "Richest 20%"), ordered = TRUE))

table(xx$resp_income_agg3_str, xx$resp_wealth_agg3_str)

table(xx$resp_live_group_fct, xx$resp_wealth_agg3_str)

agreement <- sum(xx$resp_income_agg3_str == xx$resp_wealth_agg3_str, na.rm = TRUE)/ nrow(xx)

conf_matrix <- table(xx$resp_income_agg3_str, xx$resp_wealth_agg3_str, dnn = c("Income", "Wealth"))

library(reshape2)

conf_df <- as.data.frame(melt(conf_matrix)) %>% mutate(value = value/nrow(xx))

ggplot(conf_df, aes(x = Income, y = Wealth, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value*100))) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion Matrix", x = "Income", y = "Wealth")
  