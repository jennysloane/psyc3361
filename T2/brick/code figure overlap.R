# requiered packages and dataset

list.of.packages <- c("tidyverse", "plyr", "dplyr", "effsize", "ggstatsplot", "effectsize", "MOTE", "psych")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
invisible(lapply(list.of.packages, library, character.only = TRUE))

load("merged.rda")

merged <- merged %>%
  mutate(overestimation_S1 = self_interest_others_S1 - 5.6)

merged$overestimation_S1[is.na(merged$self_interest_others_S1)] <- NA

mergedplot <- merged %>% # n = 413 for MTurk: too many?
  group_by(study) %>%
  filter(donation_payment_self == "paid" & donation_payment_others == "paid" & policy_self == 1) %>%  
  distinct()

mergedplot$Sample <- mergedplot$study

# colored overlaid histograms
png("comp by study.png", width = 500, height = 300)
ggplot(mergedplot) +
  geom_histogram(aes(x=overestimation_S1, fill=Sample), 
                 colour="grey50", alpha=0.5, position="identity", binwidth = 5) + 
                 labs(x="Overestimation (%)", y="Number of participants") + 
                 theme(text = element_text(size = 15))
dev.off()

# grayscale overlaid histograms
p1 <- ggplot() +
  geom_histogram(data = mergedplot, aes(x = overestimation_S1, fill = Sample),
                 colour = "transparent", position = 'identity', alpha = .65, binwidth = 5) +
  scale_fill_grey() + labs(x="Overestimation (%)", y="Number of participants")+
  theme(text = element_text(size = 22))
df <- ggplot_build(p1)$data[[1]][ , c("xmin", "y", "group")]
df$Sample <- factor(df$group, labels = c("MTurk", "Prolific"))
p1 +
  geom_step(data = df, aes(x = xmin, y = y, linetype = Sample)) + 
  theme_bw() +
  theme(text = element_text(size = 15))
p1




#S4

H4_wide_S4 <- spread(H4_wide_S4, smoking_status_others, estimate_S4) %>%  # new estimate columns are "smoker" and "nonsmoker" 
  select(id, cos, smoker, nonsmoker, policy_others, study) %>%
  distinct() %>%
  mutate(self_interest_others_S4 = nonsmoker - smoker) # one row per policy

# M and SD of estimated self-interest across policies
H4_est_self_interest_by_ID <- H4_wide_S4 %>%
  group_by(id) %>%
  dplyr::summarise(self_interest_others_S4_m  = mean(self_interest_others_S4, na.rm = FALSE),
                   self_interest_others_S4_sd =   sd(self_interest_others_S4, na.rm = FALSE)) %>%
  ungroup()

H4_est_self_interest_by_ID <- H4_est_self_interest_by_ID %>%
  mutate(overestimation_S4 = (self_interest_others_S4_m - 27.1)) %>%
  distinct()

mergedplot2 <- left_join(merged, H4_est_self_interest_by_ID, copy = FALSE) %>%
  group_by(study) %>%
  select(id, study, overestimation_S4) %>%  
  distinct()

mergedplot2$Sample <- mergedplot2$study

# grayscale overlaid histograms S4
p2 <- ggplot() +
  geom_histogram(data = mergedplot2, aes(x = overestimation_S4, fill = Sample, xmin=-100, xmax=100),
                 colour = "transparent", position = 'identity', alpha = .65, binwidth = 5) +
  scale_fill_grey() + labs(x="Overestimation (%)", y="Number of participants")+
  theme(text = element_text(size = 22))
df <- ggplot_build(p1)$data[[1]][ , c("xmin", "y", "group")]
df$Sample <- factor(df$group, labels = c("MTurk", "Prolific"))
p2 +
  geom_step(data = df, aes(x = xmin, y = y, linetype = Sample)) + 
  theme_bw() +
  theme(text = element_text(size = 15))
p2

