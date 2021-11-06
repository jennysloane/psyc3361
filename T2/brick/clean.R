#R version 4.0.2 (2020-06-22) -- "Taking Off Again"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

list.of.packages <- c("here", "haven", "labelled", "assertr", "psych", "data.table", "dataCompareR", "lubridate", "naniar", "multicon", "plyr", "dplyr", "tidyverse")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE, repos = "http://cran.us.r-project.org")
invisible(lapply(list.of.packages, library, character.only = TRUE))

a_anon <- read_csv("brick/a_anon.csv")
b_anon <- read_csv("brick/b_anon.csv")

# remove one ostensible 11-year-old
a_anon <- a_anon %>%
  filter(age>15)

# in excel: we aligned different variable names between the replications to produce the below code, see "ab raw vars.xlsx"
# align column names
a <- a_anon %>%
  mutate(id = row_number()) %>%
  mutate(public_1_1_TEXT = as.numeric(public_1_1_TEXT)) %>%
  mutate(tax_3_5_TEXT = as.numeric(tax_3_5_TEXT)) %>%
  rename(StartDate = V8, EndDate = V9, duration = Q_TotalDuration, blood_paid_self = blood_paid_actual, 
         blood_unpaid_self = blood_nonpaid_actual, blood_paid_others = blood_paid_esti, blood_unpaid_others = blood_nonpaid_esti,
         smoking_status_ext = extention_smoke_stat, female = gender, serious_participation = serious, country_origin = origcount,
         socialclass = soc_class, english_lang = engunder) %>%
  rename(policy1_others_non = tax_3_8_TEXT, policy1_others_former = tax_3_6_TEXT, policy1_others_light = tax_3_1_TEXT,
         policy1_others_mod = tax_3_4_TEXT, policy1_others_heavy  = tax_3_5_TEXT) %>%
  rename(policy2_others_non = ad_3_8_TEXT, policy2_others_former = ad_3_6_TEXT, policy2_others_light = ad_3_1_TEXT,
         policy2_others_mod = ad_3_4_TEXT, policy2_others_heavy  = ad_3_5_TEXT) %>%
  rename(policy3_others_non = public_3_8_TEXT, policy3_others_former = public_3_6_TEXT, policy3_others_light = public_3_1_TEXT,
         policy3_others_mod = public_3_4_TEXT, policy3_others_heavy  = public_3_5_TEXT) %>%
  rename(policy4_others_non = restaurant_3_8_TEXT, policy4_others_former = restaurant_3_6_TEXT, policy4_others_light = restaurant_3_1_TEXT,
         policy4_others_mod = restaurant_3_4_TEXT, policy4_others_heavy  = restaurant_3_5_TEXT) %>%
  rename(policy5_others_non = workplace_3_8_TEXT, policy5_others_former = workplace_3_6_TEXT, policy5_others_light = workplace_3_1_TEXT,
         policy5_others_mod = workplace_3_4_TEXT, policy5_others_heavy  = workplace_3_5_TEXT) %>%
  rename(policy6_others_non = bus_train_3_8_TEXT, policy6_others_former = bus_train_3_6_TEXT, policy6_others_light = bus_train_3_1_TEXT,
         policy6_others_mod = bus_train_3_4_TEXT, policy6_others_heavy  = bus_train_3_5_TEXT) %>%
  rename(policy7_others_non = airplane_3_8_TEXT, policy7_others_former = airplane_3_6_TEXT, policy7_others_light = airplane_3_1_TEXT,
         policy7_others_mod = airplane_3_4_TEXT, policy7_others_heavy  = airplane_3_5_TEXT) %>%
  rename(policy8_others_non = restr_hotels_motels_8_TEXT, policy8_others_former = restr_hotels_motels_6_TEXT, policy8_others_light = restr_hotels_motels_1_TEXT,
         policy8_others_mod = restr_hotels_motels_4_TEXT, policy8_others_heavy  = restr_hotels_motels_5_TEXT) %>%
  rename(policy1_self_smoker = tax_0, policy2_self_smoker = ad_0, policy3_self_smoker = public_0, policy4_self_smoker = restaurants_0,
         policy5_self_smoker = workplaces_0, policy6_self_smoker = bus_train_0, policy7_self_smoker = airplane_0, policy8_self_smoker = hotel_motel_0) %>%
  rename(policy1_self_nonsmoker = tax_2, policy2_self_nonsmoker = ad_2, policy3_self_nonsmoker = public_2, policy4_self_nonsmoker = restaurant_2,
         policy5_self_nonsmoker = workplace_2, policy6_self_nonsmoker = bus_train_2, policy7_self_nonsmoker = airplane_2, policy8_self_nonsmoker = hotel_motel_2) %>%
  rename(policy1_others_smoker = tax_1_1_TEXT, policy2_others_smoker = ad_1_1_TEXT, policy3_others_smoker = public_1_1_TEXT, policy4_others_smoker = restaurant_1_1_TEXT,
         policy5_others_smoker = workplace_1_1_TEXT, policy6_others_smoker = bus_train_1_1_TEXT, policy7_others_smoker = airplane_1_1_TEXT, policy8_others_smoker = hotel_motel_1_1_TEXT) %>%
  rename(policy1_others_nonsmoker = increase_tax_2_TEXT, policy2_others_nonsmoker = ban_ad_2_TEXT, policy3_others_nonsmoker = ban_public_2_TEXT, 
         policy4_others_nonsmoker = restr_restaurant_2_TEXT, policy5_others_nonsmoker = restr_workplaces_2_TEXT, policy6_others_nonsmoker = restr__bus_train_2_TEXT, 
         policy7_others_nonsmoker = restr_airplane_2_TEXT, policy8_others_nonsmoker = restr_hotels_motels_2_TEXT) %>%
  mutate(StartDate = as_date(StartDate)) %>%
  mutate(EndDate = as_date(EndDate)) %>%
  mutate(duration = as.numeric(duration)) %>%
  mutate(policy1_others_smoker = as.numeric(policy1_others_smoker)) %>%
  mutate(policy2_others_smoker = as.numeric(policy2_others_smoker)) %>%
  mutate(policy3_others_smoker = as.numeric(policy3_others_smoker)) %>%
  mutate(policy4_others_smoker = as.numeric(policy4_others_smoker)) %>%
  mutate(policy5_others_smoker = as.numeric(policy5_others_smoker)) %>%
  mutate(policy6_others_smoker = as.numeric(policy6_others_smoker)) %>%
  mutate(policy7_others_smoker = as.numeric(policy7_others_smoker)) %>%
  mutate(policy8_others_smoker = as.numeric(policy8_others_smoker)) %>%
  mutate(policy1_others_nonsmoker = as.numeric(policy1_others_nonsmoker)) %>%
  mutate(policy2_others_nonsmoker = as.numeric(policy2_others_nonsmoker)) %>%
  mutate(policy3_others_nonsmoker = as.numeric(policy3_others_nonsmoker)) %>%
  mutate(policy4_others_nonsmoker = as.numeric(policy4_others_nonsmoker)) %>%
  mutate(policy5_others_nonsmoker = as.numeric(policy5_others_nonsmoker)) %>%
  mutate(policy6_others_nonsmoker = as.numeric(policy6_others_nonsmoker)) %>%
  mutate(policy7_others_nonsmoker = as.numeric(policy7_others_nonsmoker)) %>%
  mutate(policy8_others_nonsmoker = as.numeric(policy8_others_nonsmoker))

b <- b_anon %>%
  mutate(id = row_number() + 1000) %>%
  mutate(S1_Pay_Estimate = round(S1_Pay_Estimate, 0))  %>%
  mutate(S1_Pay_Estimate = round(S1_Pay_Estimate, 0))  %>%
  select(-Q_TotalDuration) %>%
  rename(duration = Duration__in_seconds_, female = gender, blood_paid_self = S1_Pay_Actual, blood_paid_others = S1_Pay_Estimate, 
         blood_unpaid_self = S1_NoPay_Actual, blood_unpaid_others = S1_NoPay_Estimate,
         smoking_status = S4_A_Status, country_origin = country, english_lang = english,
         S1cond = FL_12_DO_Study1BloodDonation_Version1_PaymentCondition, 
         funnel_improve = fun_improve, funnel_pay = fun_pay, funnel_purpose = fun_purpose, serious_participation = fun_serious) %>%
  rename(policy1_self_smoker = S4_A_S_ban_1,      policy2_self_smoker = S4_A_S_ban_2,      policy3_self_smoker = S4_A_S_ban_3, policy4_self_smoker = S4_A_S_restrict_1,
         policy5_self_smoker = S4_A_S_restrict_2, policy6_self_smoker = S4_A_S_restrict_3, policy7_self_smoker = S4_A_S_restrict_4, policy8_self_smoker = S4_A_S_restrict_5) %>%
  rename(policy1_self_nonsmoker = S4_A_NS_ban_1,      policy2_self_nonsmoker = S4_A_NS_ban_2,      policy3_self_nonsmoker = S4_A_NS_ban_3, policy4_self_nonsmoker = S4_A_NS_restrict_1,
         policy5_self_nonsmoker = S4_A_NS_restrict_2, policy6_self_nonsmoker = S4_A_NS_restrict_3, policy7_self_nonsmoker = S4_A_NS_restrict_4, policy8_self_nonsmoker = S4_A_NS_restrict_5) %>%
  rename(policy1_others_smoker = S4_B_S_Policy_1_1, policy2_others_smoker = S4_B_S_Policy_2_1, policy3_others_smoker = S4_B_S_Policy_3_1, policy4_others_smoker = S4_B_S_Policy_4_1,
         policy5_others_smoker = S4_B_S_Policy_5_1, policy6_others_smoker = S4_B_S_Policy_6_1, policy7_others_smoker = S4_B_S_Policy_7_1, policy8_others_smoker = S4_B_S_Policy_8_1) %>%
  rename(policy1_others_nonsmoker = S4_B_NS_Policy_1_1, policy2_others_nonsmoker = S4_B_NS_Policy_2_1, policy3_others_nonsmoker = S4_B_NS_Policy_3_1, policy4_others_nonsmoker = S4_B_NS_Policy_4_1,
         policy5_others_nonsmoker = S4_B_NS_Policy_5_1, policy6_others_nonsmoker = S4_B_NS_Policy_6_1, policy7_others_nonsmoker = S4_B_NS_Policy_7_1, policy8_others_nonsmoker = S4_B_NS_Policy_8_1) %>%
  mutate(StartDate = as_date(b_anon$StartDate)) %>% # had a typo here
  mutate(EndDate = as_date(EndDate))

val_labels(a$female) <-            val_labels(b$female)
val_labels(a$blood_paid_self) <-   val_labels(b$blood_paid_self)
val_labels(a$blood_unpaid_self) <- val_labels(b$blood_unpaid_self)
val_labels(a$smoking_status) <-    val_labels(b$smoking_status)

save(a, file = "a.rda") 
save(b, file = "b.rda") 
# rm(a_anon, b_anon, a_raw, b_raw)

# merge
merged <- a %>% 
  full_join(b) %>%
  select(-c(A.3,A.4,debrief,DO_BR_FL_79,DO_BR_FL_84,DO_Q_blood_nonpaid_actual:DO_Q_support_esti_smoker,
            EXT_COS_DO_1:EXT_COS_DO_9,FL_12_DO_Study1Blooddonation_Version2_NoPaymentCondition:FL_16_DO_FL_13,
            fun_time_Click_Count:fun_time_Page_Submit,funnel_time_1:funnel_time_4,S1_NoPay_Actual_DO_0:SC0,
            Study1BloodDonation_Version1_PaymentCondition_DO_S1_Pay_Actual:Study4Smokingstatusonrelatedpolicies_VersionB_EstimatedSupportRa,
            DO_Q_dedicated:DO_Q_support_esti_non,EXT_COS_DO_10:FL_13_DO_Study4Smokingstatusonrelatedpolicies_VersionB_Estimated,
            DO_Q_extension_actualA:DO_Q_support_actual_B, DO_BR_FL_98:DO_Q_smoking_status))


# communalism scale composite
merged <- merged %>%
  mutate(EXT_COS_3_r  = 8-EXT_COS_3)  %>%
  mutate(EXT_COS_4_r  = 8-EXT_COS_4)  %>%
  mutate(EXT_COS_6_r  = 8-EXT_COS_6)  %>%
  mutate(EXT_COS_9_r  = 8-EXT_COS_9)  %>%
  mutate(EXT_COS_10_r = 8-EXT_COS_10) %>%
  mutate(EXT_COS_12_r = 8-EXT_COS_12) %>%
  mutate(EXT_COS_13_r = 8-EXT_COS_13) %>%
  select(-c(EXT_COS_3,EXT_COS_4,EXT_COS_6,EXT_COS_9,EXT_COS_10,EXT_COS_12,EXT_COS_13)) %>%
  mutate(cos = rowMeans(select(., contains('COS')), na.rm = TRUE))

## reliability for COS scale
# cos <- merged %>%
#   group_by(id) %>%
#   select(., contains('COS')) %>%
#   select(., !contains('DO')) %>%
#   ungroup()
# 
# cos <- cos %>%
#   select(id, cos) %>%
#   filter(!is.na(cos)) %>%
#   distinct()
# 
# psych::alpha(cos, check.keys = TRUE)
# Call: psych::alpha(x = cos, check.keys = TRUE)
# raw_alpha std.alpha G6(smc) average_r S/N    ase mean   sd median_r
# 0.83      0.82    0.86      0.22 4.6 0.0082  4.9 0.74     0.18


# combine smoker/nonsmoker self ratings
merged <- merged %>%
  mutate(policy1_self = coalesce(policy1_self_smoker, policy1_self_nonsmoker)) %>%
  mutate(policy2_self = coalesce(policy2_self_smoker, policy2_self_nonsmoker)) %>%
  mutate(policy3_self = coalesce(policy3_self_smoker, policy3_self_nonsmoker)) %>%
  mutate(policy4_self = coalesce(policy4_self_smoker, policy4_self_nonsmoker)) %>%
  mutate(policy5_self = coalesce(policy5_self_smoker, policy5_self_nonsmoker)) %>%
  mutate(policy6_self = coalesce(policy6_self_smoker, policy6_self_nonsmoker)) %>%
  mutate(policy7_self = coalesce(policy7_self_smoker, policy7_self_nonsmoker)) %>%
  mutate(policy8_self = coalesce(policy8_self_smoker, policy8_self_nonsmoker)) %>%
  select(-c(policy1_self_smoker, policy1_self_nonsmoker, policy2_self_smoker, policy2_self_nonsmoker, 
            policy3_self_smoker, policy3_self_nonsmoker, policy4_self_smoker, policy4_self_nonsmoker, 
            policy5_self_smoker, policy5_self_nonsmoker, policy6_self_smoker, policy6_self_nonsmoker, 
            policy7_self_smoker, policy7_self_nonsmoker, policy8_self_smoker, policy8_self_nonsmoker)) %>%
  mutate(female = as.numeric(female)) %>%
  select(sort(names(.)))

merged <- merged %>%
  mutate(self_interest_self_S1   = blood_paid_self-blood_unpaid_self) %>%
  mutate(self_interest_others_S1 = blood_paid_others-blood_unpaid_others)

# gather self policy support variables
policy_self_df  <- gather(merged, "policy_support_self", "policy_support", policy1_self, policy2_self, policy3_self, policy4_self, 
         policy5_self, policy6_self, policy7_self, policy8_self)
policy_self_df <- policy_self_df %>%
  select(id, policy_support_self, policy_support) %>%
  distinct()

merged <- merge(policy_self_df, merged, by = "id")

# create numbers 1-8 for policy_self
merged <- merged %>%
 mutate(policy_self = case_when(policy_support_self == "policy1_self" & !is.na(policy_support) ~ 1,
                                policy_support_self == "policy2_self" & !is.na(policy_support) ~ 2,
                                policy_support_self == "policy3_self" & !is.na(policy_support) ~ 3,
                                policy_support_self == "policy4_self" & !is.na(policy_support) ~ 4,
                                policy_support_self == "policy5_self" & !is.na(policy_support) ~ 5,
                                policy_support_self == "policy6_self" & !is.na(policy_support) ~ 6,
                                policy_support_self == "policy7_self" & !is.na(policy_support) ~ 7,
                                policy_support_self == "policy8_self" & !is.na(policy_support) ~ 8)) %>%
  select(-c(policy1_self, policy2_self, policy3_self, policy4_self, policy5_self, policy6_self, policy7_self, policy8_self, policy_support_self))

# reconcile the factor levels
merged$policy_support[merged$study == "MTurk"]    <- recode(merged$policy_support[merged$study == "MTurk"],    "0" = "oppose",  "1" = "support", "2" = "no opinion")
merged$policy_support[merged$study == "Prolific"] <- recode(merged$policy_support[merged$study == "Prolific"], "1" = "support", "2" = "oppose",  "3" = "no opinion")

# reorder levels
merged$policy_support   <- ordered(merged$policy_support, levels = c("oppose", "no opinion", "support"))
merged$policy_support_n <-  recode(merged$policy_support, "oppose" = 0,  "no opinion" = .5, "support" = 1)

## make "no opinion" policy support = NA for self-rated policies in S4
# merged$policy_support_n <- na_if(merged$policy_support_n, .5)

# calculate policy support by id
policy_self_df <- merged %>%
  group_by(id) %>%
  select(id, policy_self, policy_support_n) %>%
  dplyr::summarise(policy_support_by_id  = mean(policy_support_n, na.rm = TRUE)) %>%
  filter(!is.na(policy_support_by_id)) %>%
  ungroup()

merged <- full_join(policy_self_df, merged, by = "id")

## tidying for S4 vars
# gather policy estimates (others) --  drops NA rows. Need to exclude _self variables to avoid gathering them

policy_others_df <- gather(merged, "smoking_status_others", "estimate_S4", policy1_others_former:policy8_others_smoker) %>%
  select(id, smoking_status_others, estimate_S4) %>%
  mutate(estimate_S4 = as.numeric(estimate_S4))

policy_others_df <- policy_others_df %>%  
  filter(!is.na(estimate_S4)) %>%
  distinct()
  
merged <- full_join(policy_others_df, merged, by = "id") 

# get policy_other numbers from smoking_status_others

merged$policy_others[merged$smoking_status_others == "policy1_others_former"] <- 1
merged$policy_others[merged$smoking_status_others == "policy2_others_former"] <- 2
merged$policy_others[merged$smoking_status_others == "policy3_others_former"] <- 3
merged$policy_others[merged$smoking_status_others == "policy4_others_former"] <- 4
merged$policy_others[merged$smoking_status_others == "policy5_others_former"] <- 5
merged$policy_others[merged$smoking_status_others == "policy6_others_former"] <- 6
merged$policy_others[merged$smoking_status_others == "policy7_others_former"] <- 7
merged$policy_others[merged$smoking_status_others == "policy8_others_former"] <- 8

merged$policy_others[merged$smoking_status_others == "policy1_others_non"] <- 1
merged$policy_others[merged$smoking_status_others == "policy2_others_non"] <- 2
merged$policy_others[merged$smoking_status_others == "policy3_others_non"] <- 3
merged$policy_others[merged$smoking_status_others == "policy4_others_non"] <- 4
merged$policy_others[merged$smoking_status_others == "policy5_others_non"] <- 5
merged$policy_others[merged$smoking_status_others == "policy6_others_non"] <- 6
merged$policy_others[merged$smoking_status_others == "policy7_others_non"] <- 7
merged$policy_others[merged$smoking_status_others == "policy8_others_non"] <- 8

merged$policy_others[merged$smoking_status_others == "policy1_others_light"] <- 1
merged$policy_others[merged$smoking_status_others == "policy2_others_light"] <- 2
merged$policy_others[merged$smoking_status_others == "policy3_others_light"] <- 3
merged$policy_others[merged$smoking_status_others == "policy4_others_light"] <- 4
merged$policy_others[merged$smoking_status_others == "policy5_others_light"] <- 5
merged$policy_others[merged$smoking_status_others == "policy6_others_light"] <- 6
merged$policy_others[merged$smoking_status_others == "policy7_others_light"] <- 7
merged$policy_others[merged$smoking_status_others == "policy8_others_light"] <- 8

merged$policy_others[merged$smoking_status_others == "policy1_others_mod"] <- 1
merged$policy_others[merged$smoking_status_others == "policy2_others_mod"] <- 2
merged$policy_others[merged$smoking_status_others == "policy3_others_mod"] <- 3
merged$policy_others[merged$smoking_status_others == "policy4_others_mod"] <- 4
merged$policy_others[merged$smoking_status_others == "policy5_others_mod"] <- 5
merged$policy_others[merged$smoking_status_others == "policy6_others_mod"] <- 6
merged$policy_others[merged$smoking_status_others == "policy7_others_mod"] <- 7
merged$policy_others[merged$smoking_status_others == "policy8_others_mod"] <- 8

merged$policy_others[merged$smoking_status_others == "policy1_others_heavy"] <- 1
merged$policy_others[merged$smoking_status_others == "policy2_others_heavy"] <- 2
merged$policy_others[merged$smoking_status_others == "policy3_others_heavy"] <- 3
merged$policy_others[merged$smoking_status_others == "policy4_others_heavy"] <- 4
merged$policy_others[merged$smoking_status_others == "policy5_others_heavy"] <- 5
merged$policy_others[merged$smoking_status_others == "policy6_others_heavy"] <- 6
merged$policy_others[merged$smoking_status_others == "policy7_others_heavy"] <- 7
merged$policy_others[merged$smoking_status_others == "policy8_others_heavy"] <- 8

merged$policy_others[merged$smoking_status_others == "policy1_others_smoker"] <- 1
merged$policy_others[merged$smoking_status_others == "policy2_others_smoker"] <- 2
merged$policy_others[merged$smoking_status_others == "policy3_others_smoker"] <- 3
merged$policy_others[merged$smoking_status_others == "policy4_others_smoker"] <- 4
merged$policy_others[merged$smoking_status_others == "policy5_others_smoker"] <- 5
merged$policy_others[merged$smoking_status_others == "policy6_others_smoker"] <- 6
merged$policy_others[merged$smoking_status_others == "policy7_others_smoker"] <- 7
merged$policy_others[merged$smoking_status_others == "policy8_others_smoker"] <- 8

merged$policy_others[merged$smoking_status_others == "policy1_others_nonsmoker"] <- 1
merged$policy_others[merged$smoking_status_others == "policy2_others_nonsmoker"] <- 2
merged$policy_others[merged$smoking_status_others == "policy3_others_nonsmoker"] <- 3
merged$policy_others[merged$smoking_status_others == "policy4_others_nonsmoker"] <- 4
merged$policy_others[merged$smoking_status_others == "policy5_others_nonsmoker"] <- 5
merged$policy_others[merged$smoking_status_others == "policy6_others_nonsmoker"] <- 6
merged$policy_others[merged$smoking_status_others == "policy7_others_nonsmoker"] <- 7
merged$policy_others[merged$smoking_status_others == "policy8_others_nonsmoker"] <- 8

merged$policy_others[!is.na(merged$policy_support)] <- NA

# renaming smoking_status_others and reducing complexity
 merged$smoking_status_others <- recode(merged$smoking_status_others, policy1_others_non = "ext_non",
                                        policy1_others_former = "ext_former", policy1_others_light = "ext_light",
                                        policy1_others_mod = "ext_mod", policy1_others_heavy = "ext_heavy",
                                        policy1_others_nonsmoker = "nonsmoker", policy1_others_smoker = "smoker")
 merged$smoking_status_others <- recode(merged$smoking_status_others, policy2_others_non = "ext_non",
                                        policy2_others_former = "ext_former", policy2_others_light = "ext_light",
                                        policy2_others_mod = "ext_mod", policy2_others_heavy = "ext_heavy",
                                        policy2_others_nonsmoker = "nonsmoker", policy2_others_smoker = "smoker")
 merged$smoking_status_others <- recode(merged$smoking_status_others, policy3_others_non = "ext_non",
                                        policy3_others_former = "ext_former", policy3_others_light = "ext_light",
                                        policy3_others_mod = "ext_mod", policy3_others_heavy = "ext_heavy",
                                        policy3_others_nonsmoker = "nonsmoker", policy3_others_smoker = "smoker")
 merged$smoking_status_others <- recode(merged$smoking_status_others, policy4_others_non = "ext_non",
                                        policy4_others_former = "ext_former", policy4_others_light = "ext_light",
                                        policy4_others_mod = "ext_mod", policy4_others_heavy = "ext_heavy",
                                        policy4_others_nonsmoker = "nonsmoker", policy4_others_smoker = "smoker")
 merged$smoking_status_others <- recode(merged$smoking_status_others, policy5_others_non = "ext_non",
                                        policy5_others_former = "ext_former", policy5_others_light = "ext_light",
                                        policy5_others_mod = "ext_mod", policy5_others_heavy = "ext_heavy",
                                        policy5_others_nonsmoker = "nonsmoker", policy5_others_smoker = "smoker")
 merged$smoking_status_others <- recode(merged$smoking_status_others, policy6_others_non = "ext_non",
                                        policy6_others_former = "ext_former", policy6_others_light = "ext_light",
                                        policy6_others_mod = "ext_mod", policy6_others_heavy = "ext_heavy",
                                        policy6_others_nonsmoker = "nonsmoker", policy6_others_smoker = "smoker")
 merged$smoking_status_others <- recode(merged$smoking_status_others, policy7_others_non = "ext_non",
                                        policy7_others_former = "ext_former", policy7_others_light = "ext_light",
                                        policy7_others_mod = "ext_mod", policy7_others_heavy = "ext_heavy",
                                        policy7_others_nonsmoker = "nonsmoker", policy7_others_smoker = "smoker")
 merged$smoking_status_others <- recode(merged$smoking_status_others, policy8_others_non = "ext_non",
                                        policy8_others_former = "ext_former", policy8_others_light = "ext_light",
                                        policy8_others_mod = "ext_mod", policy8_others_heavy = "ext_heavy",
                                        policy8_others_nonsmoker = "nonsmoker", policy8_others_smoker = "smoker")

merged$smoking_status_others <- ordered(merged$smoking_status_others, levels = c("ext_non", "ext_former", "ext_light", "ext_mod", "ext_heavy", "nonsmoker", "smoker"))
 

merged$smoking_status     <- recode (merged$smoking_status, "0" = "nonsmoker", "1" = "smoker")
merged$smoking_status_ext <- recode (merged$smoking_status_ext, "0" = "never", "1" = "former", "2" = "light", "3" = "moderate", "4" = "heavy")
merged$smoking_status_ext <- ordered(merged$smoking_status_ext, levels = c("never", "former", "light", "moderate", "heavy"))

## tidy format for S1 vars

# sensitivity to payment self S1
merged$sensitivity <- NA
merged$sensitivity[!is.na(merged$blood_paid_self)] <- 0
merged$sensitivity[merged$blood_paid_self == 1 & merged$blood_unpaid_self == 0] <- 1
merged$self_interest_self_S1 <- (merged$blood_paid_self - merged$blood_unpaid_self)

## gather S1 paid vs. unpaid self
donate_self <- gather(merged, "donation_payment_self", "donate", blood_paid_self, blood_unpaid_self)

donate_self <- donate_self %>%
  select(id, donation_payment_self, donate) %>%
  distinct()

merged <- full_join(donate_self, merged, by = "id")

merged$donation_payment_self <- recode(merged$donation_payment_self, blood_paid_self = "paid", blood_unpaid_self = "unpaid")

## gather S1 paid vs. unpaid other (estimates)
donate_others <- gather(merged, "donation_payment_others", "estimate_S1", blood_paid_others, blood_unpaid_others)

donate_others <- donate_others %>%
  select(id, donation_payment_others, estimate_S1) %>%
  distinct()

merged <- full_join(donate_others, merged, by = "id")

merged$donation_payment_others <- recode(merged$donation_payment_others, blood_paid_others = "paid", blood_unpaid_others = "unpaid")
merged$donation_payment_others <- recode(merged$donation_payment_others, blood_paid_self = "paid", blood_unpaid_self = "unpaid")

merged <- merged %>%
  select(id, study, donation_payment_self, donate, donation_payment_others, estimate_S1, policy_self, policy_support, 
          smoking_status_others, estimate_S4, smoking_status, smoking_status_ext, StartDate, EndDate, duration, 
          english_lang, age, female, funnel_improve, funnel_pay, funnel_purpose, serious_participation, socialclass, everything())

merged$female = as.character(merged$female)

merged <- merged %>%
  select(-c(policy1_others_former:V7, ExternalReference:outline, consent_1:consentform, DistributionChannel, assignmentId, A:A.2))

S1 <- merged %>%
  select(id, study, cos, donation_payment_others, donation_payment_self, donate, estimate_S1, blood_paid_others, blood_paid_self, blood_unpaid_others, blood_unpaid_self) %>%
  distinct()

merged <- merged %>%
  select(-c(donation_payment_others, donation_payment_self, donate, estimate_S1, blood_paid_others, blood_paid_self, blood_unpaid_others, blood_unpaid_self)) %>%
  distinct()

   ## rbind when columns don't match
   # https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/
     rbind.all.columns <- function(x, y) {
     x.diff <- setdiff(colnames(x), colnames(y))
     y.diff <- setdiff(colnames(y), colnames(x))
     x[, c(as.character(y.diff))] <- NA
     y[, c(as.character(x.diff))] <- NA
     return(rbind(x, y))
   }
   
   merged <- rbind.all.columns(merged, S1)
   
merged <- merged %>%
  select(id, study, cos, donation_payment_self, donate, donation_payment_others, estimate_S1, policy_self, smoking_status, policy_support_n, policy_others, smoking_status_others,
          estimate_S4, self_interest_self_S1, self_interest_others_S1, everything()) %>%
  arrange(id, policy_self, policy_others)


save(merged, file = "brick/merged.rda")
