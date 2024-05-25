#setwd

setwd("G:/IIT-D/Sem II/HSD700/Research paper reviews/Mental health literacy/Data & Analysis")


#Libraries

#install.packages("psych")

library(psych)
library(apaTables)
library(dplyr)
library(knitr)
library(psych)
library(knitr)
library(car)
library(stargazer)
library(corrplot)

#-----------------------------

#Data Summary- Data for Analysis (fdata)

fdata <- read.csv("Data for Analysis.csv")
#View(fdata)
colnames(fdata)
dim(fdata)
head(fdata)
summary(fdata)

#-----------------------------

###Difference Between Pre and Post Tests (Column Creation)

fdata$diff.MHL_KMHP <- fdata$post.MHL_KMHP - fdata$pre.MHL_KMHP 
fdata$diff.MHL_EBS <- fdata$post.MHL_EBS - fdata$pre.MHL_EBS
fdata$diff.MHL_FASHSB <- fdata$post.MHL_FASHSB - fdata$pre.MHL_FASHSB
fdata$diff.MHL_SHS <- fdata$post.MHL_SHS - fdata$pre.MHL_SHS
fdata$diff.MHL_Total <- fdata$post.MHL_Total - fdata$pre.MHL_Total
fdata$diff.MHSAS <- fdata$post.MHSAS - fdata$pre.MHSAS
fdata$diff.STIG <- fdata$post.STIG - fdata$pre.STIG
fdata$diff.MHSES <- fdata$post.MHSES - fdata$pre.MHSES
fdata$diff.SPANE <- fdata$post.SPANE_B - fdata$pre.SPANE_B
fdata$diff.FLOUSC <- fdata$post.FLOUSC - fdata$pre.FLOUSC

#View(fdata)
dim(fdata)

#-----------------------------


#Codebook 1- Final Data Sheet 

fdata.col_order <- seq_along(fdata)
fdata.col_names <- names(fdata)
fdata.col_info <-data.frame(fdata.col_order, fdata.col_names)
#View(fdata.col_info)
write.csv(fdata.col_info, "Codebook1_fdata.csv")

#-----------------------------

###Demographics

##Age 

mean_age <- mean(fdata$Age)
age_range <- range(fdata$Age)

##Gender

gender_table <- table(fdata$Gender)

n_male <- gender_table["Male"]
n_female <- gender_table["Female"]
n_not_say <- gender_table["Prefer not to say"]
percent_male <- round((gender_table["Male"] / sum(gender_table)) * 100, 1)
percent_female <- round((gender_table["Female"] / sum(gender_table)) * 100, 1)
percent_not_say <- round((gender_table["Prefer not to say"] / sum(gender_table)) * 100, 1)

##Education

education_table <- table(fdata$Educational.Qualification)

n_p_bachelor <- education_table["Pursuing a Bachelor's degree (BA/ BSc/ BTech)"]
n_c_bachelor <- education_table["Completed a Bachelor's course (BA/ BSc/ BTech)"]
n_p_master <- education_table["Pursuing a Master's course (MA/ MSc/ MTech)"] 
n_c_master <- education_table["Completed a Master's course (MA/ MSc/ MTech)"] 
percent_p_bachelor <- round((education_table["Pursuing a Bachelor's degree (BA/ BSc/ BTech)"] / sum(education_table)) * 100, 1)
percent_c_bachelor <- round((education_table["Completed a Bachelor's course (BA/ BSc/ BTech)"] / sum(education_table)) * 100, 1)
percent_p_master <- round((education_table["Pursuing a Master's course (MA/ MSc/ MTech)"] / sum(education_table)) * 100, 1)
percent_c_master <- round((education_table["Completed a Master's course (MA/ MSc/ MTech)"] / sum(education_table)) * 100, 1)


#Demographic Table Creation

dtable_data <- data.frame(
  "Statistic" = c("Mean age", "Age range", "Male", "Female", "Prefer not to say", "Pursuing Bachelor's", "Completed Bachelor's", "Pursuing Master's", "Completed Master's"),
  "n" = c("", "", n_male, n_female, n_not_say, n_p_bachelor, n_c_bachelor, n_p_master, n_c_master),
  "Value" = c(mean_age, paste(age_range, collapse = "-"), percent_male, percent_female, percent_not_say, percent_p_bachelor, percent_c_bachelor, percent_p_master, percent_c_master),
  "Unit" = c("years", "years", "%", "%", "%", "%", "%", "%", "%")
)

dtable_data


dtable <- kable(dtable_data, format = "markdown", col.names = c("Statistic", "n", "Value", "Unit"))

write.table(dtable_data, "Demographic table.csv", sep = ",", row.names = FALSE)


#-----------------------------

###Feedback


##GPT_Rating  

gpt_rating_table <- table(fdata$GPT_Rating)

n_gpt_rating_1 <- gpt_rating_table["Excellent"]
n_gpt_rating_2 <- gpt_rating_table["Good"]
n_gpt_rating_3 <- gpt_rating_table["Fair"]
n_gpt_rating_4 <- gpt_rating_table["Poor"]
percent_gpt_rating_1 <- round((gpt_rating_table["Excellent"] / sum(gpt_rating_table)) * 100, 1)
percent_gpt_rating_2 <- round((gpt_rating_table["Good"] / sum(gpt_rating_table)) * 100, 1)
percent_gpt_rating_3 <- round((gpt_rating_table["Fair"] / sum(gpt_rating_table)) * 100, 1)
percent_gpt_rating_4 <- round((gpt_rating_table["Poor"] / sum(gpt_rating_table)) * 100, 1)


##Prompts_Rating


prompts_rating_table <- table(fdata$Prompts_Rating)

n_prompts_rating_1 <- prompts_rating_table["Excellent"]
n_prompts_rating_2 <- prompts_rating_table["Good"]
n_prompts_rating_3 <- prompts_rating_table["Fair"]
n_prompts_rating_4 <- prompts_rating_table["Poor"]
percent_prompts_rating_1 <- round((prompts_rating_table["Excellent"] / sum(prompts_rating_table)) * 100, 1)
percent_prompts_rating_2 <- round((prompts_rating_table["Good"] / sum(prompts_rating_table)) * 100, 1)
percent_prompts_rating_3 <- round((prompts_rating_table["Fair"] / sum(prompts_rating_table)) * 100, 1)
percent_prompts_rating_4 <- round((prompts_rating_table["Poor"] / sum(prompts_rating_table)) * 100, 1)


##Intervention_Rating


intervention_rating_table <- table(fdata$Intervention_Rating)

n_intervention_rating_1 <- intervention_rating_table["Excellent"]
n_intervention_rating_2 <- intervention_rating_table["Good"]
n_intervention_rating_3 <- intervention_rating_table["Fair"]
n_intervention_rating_4 <- intervention_rating_table["Poor"]
percent_intervention_rating_1 <- round((intervention_rating_table["Excellent"] / sum(intervention_rating_table)) * 100, 1)
percent_intervention_rating_2 <- round((intervention_rating_table["Good"] / sum(intervention_rating_table)) * 100, 1)
percent_intervention_rating_3 <- round((intervention_rating_table["Fair"] / sum(intervention_rating_table)) * 100, 1)
percent_intervention_rating_4 <- round((intervention_rating_table["Poor"] / sum(intervention_rating_table)) * 100, 1)


##MHL_Rating


mhl_rating_table <- table(fdata$MHL_Rating)

n_mhl_rating_1 <- mhl_rating_table["Yes"] 
n_mhl_rating_2 <- mhl_rating_table["No"] 
n_mhl_rating_3 <- mhl_rating_table["Maybe"]
percent_mhl_rating_1 <- round((mhl_rating_table["Yes"] / sum(mhl_rating_table)) * 100, 1)
percent_mhl_rating_2 <- round((mhl_rating_table["No"] / sum(mhl_rating_table)) * 100, 1)
percent_mhl_rating_3 <- round((mhl_rating_table["Maybe"] / sum(mhl_rating_table)) * 100, 1)


##GPT_MHL


gpt_mhl_table <- table(fdata$GPT_MHL)

n_gpt_mhl_1 <- gpt_mhl_table["Yes"]
n_gpt_mhl_2 <- gpt_mhl_table["No"] 
n_gpt_mhl_3 <- gpt_mhl_table["Maybe"]
percent_gpt_mhl_1 <- round((gpt_mhl_table["Yes"] / sum(gpt_mhl_table)) * 100, 1)
percent_gpt_mhl_2 <- round((gpt_mhl_table["No"] / sum(gpt_mhl_table)) * 100, 1)
percent_gpt_mhl_3 <- round((gpt_mhl_table["Maybe"] / sum(gpt_mhl_table)) * 100, 1)


##GPT_Flaw


gpt_flaw_table <- table(fdata$GPT_Flaw)

n_gpt_flaw_1 <- gpt_flaw_table["Yes"]
n_gpt_flaw_2 <- gpt_flaw_table["No"]
n_gpt_flaw_3 <- gpt_flaw_table["Maybe"]
percent_gpt_flaw_1 <- round((gpt_flaw_table["Yes"] / sum(gpt_flaw_table)) * 100, 1)
percent_gpt_flaw_2 <- round((gpt_flaw_table["No"] / sum(gpt_flaw_table)) * 100, 1)
percent_gpt_flaw_3 <- round((gpt_flaw_table["Maybe"] / sum(gpt_flaw_table)) * 100, 1)


#Feedback Table Creation

ftable_data <- data.frame(
  "Statistic" = c("GPT_Rating", "Excellent", "Good", "Fair", "Poor", "Prompts_Rating", "Excellent", "Good", "Fair", "Poor", "Interventions Rating", "Excellent", "Good", "Fair", "Poor", "MHL_Rating", "Yes", "No", "Maybe", "GPT_MHL", "Yes", "No", "Maybe", "GPT_Flaw", "Yes", "No", "Maybe"),
  "n" = c("", n_gpt_rating_1, n_gpt_rating_2, n_gpt_rating_3, n_gpt_rating_4, "", n_prompts_rating_1, n_prompts_rating_2, n_prompts_rating_3, n_prompts_rating_4, "", n_intervention_rating_1, n_intervention_rating_2, n_intervention_rating_3, n_intervention_rating_4, "", n_mhl_rating_1, n_mhl_rating_2, n_mhl_rating_3, "", n_gpt_mhl_1, n_gpt_mhl_2, n_gpt_mhl_3, "", n_gpt_flaw_1, n_gpt_flaw_2, n_gpt_flaw_3),
  "Value" = c("", percent_gpt_rating_1, percent_gpt_rating_2, percent_gpt_rating_3, percent_gpt_rating_4, "", percent_prompts_rating_1, percent_prompts_rating_2, percent_prompts_rating_3, percent_prompts_rating_4, "", percent_intervention_rating_1, percent_intervention_rating_2, percent_intervention_rating_3, percent_intervention_rating_4, "", percent_mhl_rating_1, percent_mhl_rating_2, percent_mhl_rating_3,"", percent_gpt_mhl_1, percent_gpt_mhl_2, percent_gpt_mhl_3, "", percent_gpt_flaw_1, percent_gpt_flaw_2, percent_gpt_flaw_3),
  "Unit" = c("", "%", "%", "%", "%", "", "%", "%", "%", "%", "", "%", "%", "%", "%", "", "%", "%", "%", "", "%", "%", "%", "", "%", "%", "%")
)

#View(ftable_data)

ftable <- kable(ftable_data, format = "markdown", col.names = c("", "Value", "Unit", "n"))

write.table(ftable_data, "Feedback table.csv", sep = ",", row.names = FALSE)


#-----------------------------

###Descriptive Statistics

dstats <- function(x, na.omit = FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  min <- min(x)
  max <- max(x)
  m <- mean(x)
  median <- median(x)
  n <- length(x)
  s <- sd(x)*sqrt((length(x)-1)/length(x))
  skew <- (sum((x-m)^3/s^3)/n)*((sqrt(n*(n-1)))/(n-2))
  akurt <- ((sum((x-m)^4)/n)/((sum((x-m)^2)/n)^2))-3
  kurt <- (n/((n-1)*(n-3)))*((n+1)*(akurt)+6)
  stds <- sd(x)
  return(c(n=n, min=min, max=max, mean=m, median=median, stdp=s, skew=skew, akurt=akurt, kurt=kurt, stds=stds))
}


dstats_vars <- c("pre.MHL_KMHP", "post.MHL_KMHP", "pre.MHL_EBS", "post.MHL_EBS", "pre.MHL_FASHSB", "post.MHL_FASHSB", "pre.MHL_SHS", "post.MHL_SHS", "pre.MHL_Total", "post.MHL_Total", "pre.MHSAS", "post.MHSAS", "pre.STIG", "post.STIG", "pre.MHSES", "post.MHSES", "pre.SPANE_B", "post.SPANE_B", "pre.FLOUSC", "post.FLOUSC")
dstats_table <- sapply(fdata[dstats_vars], dstats)

write.csv(dstats_table, "Descriptive_Statistics_Table.csv", row.names = TRUE)

#-----------------------------

###Box Plots 


png(filename = "Box Plots.png", width = 1200, height = 600)

par(mfrow = c(2, 3))

boxplot(fdata$pre.MHL_Total, fdata$post.MHL_Total, names = c("pre.MHL", "post.MHL"))
boxplot(fdata$pre.MHSAS, fdata$post.MHSAS, names = c("pre.MHSAS", "post.MHSAS"))
boxplot(fdata$pre.STIG, fdata$post.STIG, names = c("pre.STIG", "post.STIG"))
boxplot(fdata$pre.MHSES, fdata$post.MHSES, names = c("pre.MHSES", "post.MHSES"))
boxplot(fdata$pre.SPANE_B, fdata$post.SPANE_B, names = c("pre.SPANE", "post.SPANE"))
boxplot(fdata$pre.FLOUSC, fdata$post.FLOUSC, names = c("pre.FLOUSC", "post.FLOUSC"))

dev.off()


#-----------------------------

###Correlation Test 


cor_MHL_KMHP <- cor.test(fdata$pre.MHL_KMHP, fdata$post.MHL_KMHP, method = c("pearson"), conf.level = 0.95)
cor_MHL_EBS <- cor.test(fdata$pre.MHL_EBS, fdata$post.MHL_EBS, method = c("pearson"), conf.level = 0.95)
cor_MHL_FASHSB <- cor.test(fdata$pre.MHL_FASHSB, fdata$post.MHL_FASHSB, method = c("pearson"), conf.level = 0.95)
cor_MHL_SHS <- cor.test(fdata$pre.MHL_SHS, fdata$post.MHL_SHS, method = c("pearson"), conf.level = 0.95)
cor_MHL_Total <- cor.test(fdata$pre.MHL_Total, fdata$post.MHL_Total, method = c("pearson"), conf.level = 0.95)
cor_MHSAS <- cor.test(fdata$pre.MHSAS, fdata$post.MHSAS, method = c("pearson"), conf.level = 0.95)
cor_STIG <- cor.test(fdata$pre.STIG, fdata$post.STIG, method = c("pearson"), conf.level = 0.95)
cor_MHSES <- cor.test(fdata$pre.MHSES, fdata$post.MHSES, method = c("pearson"), conf.level = 0.95)
cor_SPANE <- cor.test(fdata$pre.SPANE_B, fdata$post.SPANE_B, method = c("pearson"), conf.level = 0.95)
cor_FLOUSC <- cor.test(fdata$pre.FLOUSC, fdata$post.FLOUSC, method = c("pearson"), conf.level = 0.95)


correlation_results <- matrix(c("MHL_KMHP", cor_MHL_KMHP$statistic, cor_MHL_KMHP$p.value,
                                 "MHL_EBS", cor_MHL_EBS$statistic, cor_MHL_EBS$p.value,
                                 "MHL_FASHSB", cor_MHL_FASHSB$statistic, cor_MHL_FASHSB$p.value,
                                 "MHL_SHS", cor_MHL_SHS$statistic, cor_MHL_SHS$p.value,
                                 "MHL_Total", cor_MHL_Total$statistic, cor_MHL_Total$p.value,
                                 "MHSAS", cor_MHSAS$statistic, cor_MHSAS$p.value,
                                 "STIG", cor_STIG$statistic, cor_STIG$p.value,
                                 "MHSES", cor_MHSES$statistic, cor_MHSES$p.value,
                                 "SPANE", cor_SPANE$statistic, cor_SPANE$p.value,
                                 "FLOUSC", cor_FLOUSC$statistic, cor_FLOUSC$p.value),
                               nrow = 10, ncol= 3, byrow = TRUE,
                               dimnames = list(c("MHL_KMHP", "MHL_EBS", "MHL_FASHSB", "MHL_SHS", "MHL_Total", "MHL_MHSAS", "MHL_STIG", "MHL_MHSES", "MHL_SPANE", "MHL_FLOUSC"),
                                               c("Variable", "t", "p-value")))



#View(correlation_results)

write.csv(correlation_results, "Correlation_Results.csv", row.names = FALSE)


# Correlation Plot


png(filename = "Correlation Plots.png", width = 1200, height = 600)


par(mfrow = c(1, 1))

cor_vars <- c("pre.MHL_Total", "post.MHL_Total", "pre.MHSAS", "post.MHSAS", "pre.STIG", "post.STIG", "pre.MHSES", "post.MHSES", "pre.SPANE_B", "post.SPANE_B", "pre.FLOUSC", "post.FLOUSC")

cor_subset <- fdata[, cor_vars]

cor_plot <- cor(cor_subset)

corrplot(cor_plot, type="upper", method="color")


dev.off()


#-----------------------------


##Normality Test (Normal Probability Plots)


#par(mfrow = c(2, 5))


qqnorm(fdata$diff.MHL_KMHP, pch = 2, col = "red", main = "MHL_KMHP")
qqline(fdata$diff.MHL_KMHP)

qqnorm(fdata$diff.MHL_EBS, pch = 2, col = "red", main = "MHL_EBS")
qqline(fdata$diff.MHL_EBS)

qqnorm(fdata$diff.MHL_FASHSB, pch = 2, col = "red", main = "MHL_FASHSB")
qqline(fdata$diff.MHL_FASHSB)

qqnorm(fdata$diff.MHL_SHS, pch = 2, col = "red", main = "MHL_SHS")
qqline(fdata$diff.MHL_SHS)


png(filename = "qqnorm Plots.png", width = 1200, height = 600)

par(mfrow = c(2, 3))

qqnorm(fdata$diff.MHL_Total, pch = 2, col = "red", main = "MHL")
qqline(fdata$diff.MHL_Total)

qqnorm(fdata$diff.MHSAS, pch = 2, col = "red", main = "MHSAS")
qqline(fdata$diff.MHSAS)

qqnorm(fdata$diff.STIG, pch = 2, col = "red", main = "STIG")
qqline(fdata$diff.STIG)

qqnorm(fdata$diff.MHSES, pch = 2, col = "red", main = "MHSES")
qqline(fdata$diff.MHSES)

qqnorm(fdata$diff.SPANE, pch = 2, col = "red", main = "SPANE")
qqline(fdata$diff.SPANE)

qqnorm(fdata$diff.FLOUSC, pch = 2, col = "red", main = "FLOUSC")
qqline(fdata$diff.FLOUSC)

dev.off()


##Normality Test (Shapiro-Wilk Test)


sw_MHL_KMHP <- shapiro.test(fdata$diff.MHL_KMHP)
sw_MHL_EBS <- shapiro.test(fdata$diff.MHL_EBS)
sw_MHL_FASHSB <- shapiro.test(fdata$diff.MHL_FASHSB)
sw_MHL_SHS <- shapiro.test(fdata$diff.MHL_SHS)
sw_MHL_Total <- shapiro.test(fdata$diff.MHL_Total)
sw_MHSAS <- shapiro.test(fdata$diff.MHSAS)
sw_STIG <- shapiro.test(fdata$diff.STIG)
sw_MHSES <- shapiro.test(fdata$diff.MHSES)
sw_SPANE <- shapiro.test(fdata$diff.SPANE)
sw_FLOUSC <- shapiro.test(fdata$diff.FLOUSC)


shapiro.wilk_results <- matrix(c("MHL_Total", sw_MHL_Total$statistic, sw_MHL_Total$p.value,
                                 "MHSAS", sw_MHSAS$statistic, sw_MHSAS$p.value,
                                 "STIG", sw_STIG$statistic, sw_STIG$p.value,
                                 "MHSES", sw_MHSES$statistic, sw_MHSES$p.value,
                                 "SPANE", sw_SPANE$statistic, sw_SPANE$p.value,
                                 "FLOUSC", sw_FLOUSC$statistic, sw_FLOUSC$p.value),
                               nrow = 6, ncol= 3, byrow = TRUE,
                               dimnames = list(c("MHL_Total", "MHL_MHSAS", "MHL_STIG", "MHL_MHSES", "MHL_SPANE", "MHL_FLOUSC"),
                                               c("Variable", "W", "p-value")))



#View(shapiro.wilk_results)

write.csv(shapiro.wilk_results, "Shapiro.Wilk_Results.csv", row.names = FALSE)



#-----------------------------


###Paired t test  

#Null Hypothesis: Mean difference between pairs of observations is zero 
#Alternative Hypotheis: Mean difference between pairs of observations is zero
#Alpha: 0.05


ptt_MHL_KMHP <- t.test(fdata$pre.MHL_KMHP, fdata$post.MHL_KMHP, paired = TRUE)
ptt_MHL_EBS <- t.test(fdata$pre.MHL_EBS, fdata$post.MHL_EBS, paired = TRUE)
ptt_MHL_FASHSB <- t.test(fdata$pre.MHL_FASHSB, fdata$post.MHL_FASHSB, paired = TRUE)
ptt_MHL_SHS <- t.test(fdata$pre.MHL_SHS, fdata$post.MHL_SHS, paired = TRUE)
ptt_MHL_Total <- t.test(fdata$pre.MHL_Total, fdata$post.MHL_Total, paired = TRUE)
ptt_MHSAS <- t.test(fdata$pre.MHSAS, fdata$post.MHSAS, paired = TRUE)
ptt_STIG <- t.test(fdata$pre.STIG, fdata$post.STIG, paired = TRUE)
ptt_MHSES <- t.test(fdata$pre.MHSES, fdata$post.MHSES, paired = TRUE)
ptt_SPANE_B <- t.test(fdata$pre.SPANE_B, fdata$post.SPANE_B, paired = TRUE)
ptt_FLOUSC <- t.test(fdata$pre.FLOUSC, fdata$post.FLOUSC, paired = TRUE)
ptt_FLOUSC

paired.ttest_results <- matrix(c("MHL_KMHP", ptt_MHL_KMHP$statistic, ptt_MHL_KMHP$p.value,
                                         "MHL_EBS", ptt_MHL_EBS$statistic, ptt_MHL_EBS$p.value,
                                         "MHL_FASHSB", ptt_MHL_FASHSB$statistic, ptt_MHL_FASHSB$p.value,
                                         "MHL_SHS", ptt_MHL_SHS$statistic, ptt_MHL_SHS$p.value,
                                         "MHL_Total", ptt_MHL_Total$statistic, ptt_MHL_Total$p.value,
                                         "MHSAS", ptt_MHSAS$statistic, ptt_MHSAS$p.value,
                                         "STIG", ptt_STIG$statistic, ptt_STIG$p.value,
                                         "MHSES", ptt_MHSES$statistic, ptt_MHSES$p.value,
                                         "SPANE", ptt_SPANE_B$statistic, ptt_SPANE_B$p.value,
                                         "FLOUSC", ptt_FLOUSC$statistic, ptt_FLOUSC$p.value),
                                       nrow = 10, ncol= 3, byrow = TRUE,
                                       dimnames = list(c("MHL_KMHP", "MHL_EBS", "MHL_FASHSB", "MHL_SHS", "MHL_Total", "MHSAS", "STIG", "MHSES", "SPANE", "FLOUSC"),
                                                       c("Variable", "t", "p-value")))


paired.ttest_results

write.csv(paired.ttest_results, "Paired.ttest_Results.csv", row.names = FALSE)


ptt_SPANE_B <- t.test(fdata$pre.SPANE_B, fdata$post.SPANE_B, paired = TRUE)
ptt_SPANE_B

ptt_SPANE_P <- t.test(fdata$pre.SPANE_P, fdata$post.SPANE_P, paired = TRUE)
ptt_SPANE_P

ptt_SPANE_N <- t.test(fdata$pre.SPANE_N, fdata$post.SPANE_N, paired = TRUE)
ptt_SPANE_N


#-----------------------------


###Rough 


#-----------------------------

###Cohen's d



cohens_d <- function(group1, group2) {
  mean_diff <- mean(group1) - mean(group2)
  pooled_sd <- sqrt((var(group1) + var(group2)) / 2)
  d <- mean_diff / pooled_sd
  return(d)
}

d_MHL_KMHP <- cohens_d(fdata$pre.MHL_KMHP, fdata$post.MHL_KMHP)
d_MHL_EBS <- cohens_d(fdata$pre.MHL_EBS, fdata$post.MHL_EBS)
d_MHL_FASHSB <- cohens_d(fdata$pre.MHL_FASHSB, fdata$post.MHL_FASHSB)
d_MHL_SHS <- cohens_d(fdata$pre.MHL_SHS, fdata$post.MHL_SHS)
d_MHL_Total <- cohens_d(fdata$pre.MHL_Total, fdata$post.MHL_Total)
d_MHSAS <- cohens_d(fdata$pre.MHSAS, fdata$post.MHSAS)
d_STIG <- cohens_d(fdata$pre.STIG, fdata$post.STIG)
d_MHSES <- cohens_d(fdata$pre.MHSES, fdata$post.MHSES)
d_SPANE_B <- cohens_d(fdata$pre.SPANE_B, fdata$post.SPANE_B)
d_FLOUSC <- cohens_d(fdata$pre.FLOUSC, fdata$post.FLOUSC)



cohens.d_results <- matrix(c("MHL_KMHP", d_MHL_KMHP,
                            "MHL_EBS", d_MHL_EBS,
                            "MHL_FASHSB", d_MHL_FASHSB, 
                            "MHL_SHS", d_MHL_SHS, 
                            "MHL_Total", d_MHL_Total, 
                            "MHSAS", d_MHSAS, 
                            "STIG", d_STIG, 
                            "MHSES", d_MHSES, 
                            "SPANE", d_SPANE, 
                            "FLOUSC", d_FLOUSC,
                            nrow = 10, ncol= 2, byrow = TRUE,
                            dimnames = list(c("MHL_KMHP", "MHL_EBS", "MHL_FASHSB", "MHL_SHS", "MHL_Total", "MHSAS", "STIG", "MHSES", "SPANE", "FLOUSC"),
                                               c("Variable", "Cohen's d"))))


#View(cohens.d_results)

write.csv(cohens.d_results, "cohens.d_results.csv", row.names = FALSE)


d_SPANE_B <- cohens_d(fdata$pre.SPANE_B, fdata$post.SPANE_B)
d_SPANE_B

d_SPANE_P <- cohens_d(fdata$pre.SPANE_P, fdata$post.SPANE_P)
d_SPANE_P

d_SPANE_N <- cohens_d(fdata$pre.SPANE_N, fdata$post.SPANE_N)
d_SPANE_N



#---------------------



