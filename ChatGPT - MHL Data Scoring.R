#setwd

setwd("G:/IIT-D/Sem II/HSD700/Research paper reviews/Mental health literacy/Data & Analysis")


#Libraries

#install.packages("psych")

library(dplyr)
library(psych)

#-----------------------------

#Data Summary- Consent Form

con.data <- read.csv("ChatGPT - MHL (Consent form).csv")
#View(con.data)
colnames(con.data)
dim(con.data)
head(con.data)
summary(con.data)

#Codebook 1- Consent form Data

con.col_order <- seq_along(con.data)
con.col_names <- names(con.data)
con.col_info <- data.frame(con.col_order, con.col_names)
#View(con.col_info)
write.csv(con.col_info, "Codebook_Consentform.csv")

#-----------------------------

#Data Summary- Pre-test Data

pre.data <- read.csv("ChatGPT - MHL (Pre-test).CSV")
#View(pre.data)
colnames(pre.data)
dim(pre.data)
head(pre.data)
summary(pre.data)

#Codebook 1- Pre-test Data

pre.col_order <- seq_along(pre.data)
pre.col_names <- names(pre.data)
pre.col_info <-data.frame(pre.col_order, pre.col_names)
#View(pre.col_info)
write.csv(pre.col_info, "Codebook1_Pretest.csv")

#-----------------------------

#Data Summary- Post-test Data

post.data <- read.csv("ChatGPT - MHL (Post-test).CSV")
#View(post.data)
colnames(post.data)
dim(post.data)
head(post.data)
summary(post.data)

#Codebook 1- Post-test Data

post.col_order <- seq_along(post.data)
post.col_names <- names(post.data)
post.col_info <-data.frame(post.col_order, post.col_names)
#View(post.col_info)
write.csv(post.col_info, "Codebook1_Posttest.csv")


#-----------------------------
#-----------------------------


###Scoring1 


##MHLq-SVa

#MHLq-SVa(Normal scoring)

mhl_scoring1 <- function(x) {
  ifelse(x == "Strongly disagree", 1, 
         ifelse(x == "Disagree", 2, 
                ifelse(x == "Neither agree nor disagree", 3, 
                       ifelse(x == "Agree", 4, 
                              ifelse(x == "Strongly agree", 5, x)))))
}

pre.data[,c(7, 9, 10, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)] <- apply(pre.data[,c(7, 9, 10, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)], 2, mhl_scoring1)
#View(pre.data)

post.data[,c(6, 8, 9, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)] <- apply(post.data[,c(6, 8, 9, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)], 2, mhl_scoring1)
#View(post.data)

#MHLq-SVa(Reverse scoring)

mhl_scoring2 <- function(x) {
  ifelse(x == "Strongly disagree", 5, 
         ifelse(x == "Disagree", 4, 
                ifelse(x == "Neither agree nor disagree", 3, 
                       ifelse(x == "Agree", 2, 
                              ifelse(x == "Strongly agree", 1, x)))))
}


pre.data[,c(8, 11, 12)] <- apply(pre.data[,c(8, 11, 12)], 2, mhl_scoring2)
#View(pre.data)

post.data[,c(7, 10, 11)] <- apply(post.data[,c(7, 10, 11)], 2, mhl_scoring2)
#View(post.data)


##MHSAS

#MHSAS (Reverse scoring)


mhsas_scoring2 <- function(x) {
  ifelse(x == 1, 7, 
         ifelse(x == 2, 6, 
                ifelse(x == 3, 5, 
                       ifelse(x == 4, 4, 
                              ifelse(x == 5, 3,
                                     ifelse(x == 6, 2, 
                                            ifelse(x == 7, 1, x)))))))
}


pre.data[,c(24, 27, 28, 30, 31)] <- apply(pre.data[,c(24, 27, 28, 30, 31)], 2, mhsas_scoring2)
#View(pre.data)

post.data[,c(23, 26, 27, 29, 30)] <- apply(post.data[,c(23, 26, 27, 29, 30)], 2, mhsas_scoring2)
#View(post.data)


##STIG-9

#STIG-9 (Normal scoring)

stig_scoring1 <- function(x) {
  ifelse(x == "Disagree", 0, 
         ifelse(x == "Somewhat disagree", 1, 
                ifelse(x == "Somewhat agree", 2, 
                       ifelse(x == "Agree", 3, x))))
}

pre.data[,32:40] <- apply(pre.data[,32:40], 2, stig_scoring1)
#View(pre.data)

post.data[,31:39] <- apply(post.data[,31:39], 2, stig_scoring1)
#View(post.data)

##MHSES

##SPANE

#SPANE (Normal scoring)

spane_scoring1 <- function(x) {
  ifelse(x == "Very rarely or never", 1, 
         ifelse(x == "Rarely", 2, 
                ifelse(x == "Sometimes", 3, 
                       ifelse(x == "Often", 4, 
                              ifelse(x == "Very often or always", 5, x)))))
}

pre.data[,47:58] <- apply(pre.data[,47:58], 2, spane_scoring1)
#View(pre.data)

post.data[,46:57] <- apply(post.data[,46:57], 2, spane_scoring1)
#View(post.data)

#FLOUSC (Normal scoring)

flousc_scoring1 <- function(x) {
  ifelse(x == "Strongly disagree", 1, 
         ifelse(x == "Disagree", 2, 
                ifelse(x == "Slightly disagree", 3, 
                       ifelse(x == "Mixed or neither agree nor disagree", 4, 
                              ifelse(x == "Slightly agree", 5,
                                     ifelse(x == "Agree", 6,
                                            ifelse(x == "Strongly agree", 7, x)))))))
}

pre.data[,59:66] <- apply(pre.data[,59:66], 2, flousc_scoring1)
#View(pre.data)

post.data[,58:65] <- apply(post.data[,58:65], 2, flousc_scoring1)
#View(post.data)

#-----------------------------
#-----------------------------


###Scoring2


##MHLq-SVa

#Columns to be Summed- pre-test

mhl1_sum_pre.col <- c(13, 16, 17, 18, 20, 21)
mhl2_sum_pre.col <- c(8, 11, 12)
mhl3_sum_pre.col <- c(10, 14, 22)
mhl4_sum_pre.col <- c(7, 9, 15, 19)
mhl_sum_pre.col <- c(7:22)

#Columns to be Summed- post-test

mhl1_sum_post.col <- c(12, 15, 16, 17, 19, 20)
mhl2_sum_post.col <- c(7, 10, 11)
mhl3_sum_post.col <- c(9, 13, 21)
mhl4_sum_post.col <- c(6, 8, 14, 18)
mhl_sum_post.col <- c(6:21)


#Columns Creation- pre--test

pre.data[, mhl1_sum_pre.col] <- lapply(pre.data[, mhl1_sum_pre.col], as.numeric)
pre.data$pre.MHL_KMHP <- rowSums(pre.data[, mhl1_sum_pre.col])

pre.data[, mhl2_sum_pre.col] <- lapply(pre.data[, mhl2_sum_pre.col], as.numeric)
pre.data$pre.MHL_EBS <- rowSums(pre.data[, mhl2_sum_pre.col])

pre.data[, mhl3_sum_pre.col] <- lapply(pre.data[, mhl3_sum_pre.col], as.numeric)
pre.data$pre.MHL_FASHSB <- rowSums(pre.data[, mhl3_sum_pre.col])

pre.data[, mhl4_sum_pre.col] <- lapply(pre.data[, mhl4_sum_pre.col], as.numeric)
pre.data$pre.MHL_SHS <- rowSums(pre.data[, mhl4_sum_pre.col])

pre.data[, mhl_sum_pre.col] <- lapply(pre.data[, mhl_sum_pre.col], as.numeric)
pre.data$pre.MHL_Total <- rowSums(pre.data[, mhl_sum_pre.col])

#View(pre.data)


#Columns Creation- post-test

post.data[, mhl1_sum_post.col] <- lapply(post.data[, mhl1_sum_post.col], as.numeric)
post.data$post.MHL_KMHP <- rowSums(post.data[, mhl1_sum_post.col])

post.data[, mhl2_sum_post.col] <- lapply(post.data[, mhl2_sum_post.col], as.numeric)
post.data$post.MHL_EBS <- rowSums(post.data[, mhl2_sum_post.col])

post.data[, mhl3_sum_post.col] <- lapply(post.data[, mhl3_sum_post.col], as.numeric)
post.data$post.MHL_FASHSB <- rowSums(post.data[, mhl3_sum_post.col])

post.data[, mhl4_sum_post.col] <- lapply(post.data[, mhl4_sum_post.col], as.numeric)
post.data$post.MHL_SHS <- rowSums(post.data[, mhl4_sum_post.col])

post.data[, mhl_sum_post.col] <- lapply(post.data[, mhl_sum_post.col], as.numeric)
post.data$post.MHL_Total <- rowSums(post.data[, mhl_sum_post.col])

#View(post.data)


##MHSAS


#Columns to be Summed- pre-test

mhsas_sum_pre.col <- c(23:31)


#Columns to be Summed- post-test

mhsas_sum_post.col <- c(22:30)


#Columns Creation- pre-test

pre.data[, mhsas_sum_pre.col] <- lapply(pre.data[, mhsas_sum_pre.col], as.numeric)
pre.data$pre.MHSAS <- rowSums(pre.data[, mhsas_sum_pre.col]) / 9

#View(pre.data)


#Columns Creation- post-test

post.data[, mhsas_sum_post.col] <- lapply(post.data[, mhsas_sum_post.col], as.numeric)
post.data$post.MHSAS <- rowSums(post.data[, mhsas_sum_post.col]) / 9

#View(post.data)


##STIG-9

#Columns to be Summed- pre-test

stig_sum_pre.col <- c(32:40)


#Columns to be Summed- post-test

stig_sum_post.col <- c(31:39)


#Columns Creation- pre-test

pre.data[, stig_sum_pre.col] <- lapply(pre.data[, stig_sum_pre.col], as.numeric)
pre.data$pre.STIG <- rowSums(pre.data[, stig_sum_pre.col])

#View(pre.data)


#Columns Creation- post-test

post.data[, stig_sum_post.col] <- lapply(post.data[, stig_sum_post.col], as.numeric)
post.data$post.STIG <- rowSums(post.data[, stig_sum_post.col])

#View(post.data)


##MHSES

#Columns to be Summed- pre-test

mhses_sum_pre.col <- c(41:46)


#Columns to be Summed- post-test

mhses_sum_post.col <- c(40:45)


#Columns Creation- pre-test

pre.data[, mhses_sum_pre.col] <- lapply(pre.data[, mhses_sum_pre.col], as.numeric)
pre.data$pre.MHSES <- rowSums(pre.data[, mhses_sum_pre.col])

#View(pre.data)


#Columns Creation- post-test

post.data[, mhses_sum_post.col] <- lapply(post.data[, mhses_sum_post.col], as.numeric)
post.data$post.MHSES <- rowSums(post.data[, mhses_sum_post.col])

#View(post.data)


##SPANE


#Columns to be Summed- pre-test

spane.P_sum_pre.col <- c(47,49,51,53,56,58)
spane.N_sum_pre.col <- c(48,50,52,54,55,57)

#View(pre.data)

#Columns to be Summed- post-test

spane.P_sum_post.col <- c(47,49,51,53,56,58)
spane.N_sum_post.col <- c(48,50,52,54,55,57)


#Columns Creation- pre-test

pre.data[, spane.P_sum_pre.col] <- lapply(pre.data[, spane.P_sum_pre.col], as.numeric)
pre.data$pre.SPANE_P <- rowSums(pre.data[, spane.P_sum_pre.col])

pre.data[, spane.N_sum_pre.col] <- lapply(pre.data[, spane.N_sum_pre.col], as.numeric)
pre.data$pre.SPANE_N <- rowSums(pre.data[, spane.N_sum_pre.col])

pre.data$pre.SPANE_B <- pre.data$pre.SPANE_P - pre.data$pre.SPANE_N

#View(pre.data)


#Columns Creation- post-test

post.data[, spane.P_sum_pre.col] <- lapply(post.data[, spane.P_sum_post.col], as.numeric)
post.data$post.SPANE_P <- rowSums(post.data[, spane.P_sum_post.col])

post.data[, spane.N_sum_post.col] <- lapply(post.data[, spane.N_sum_post.col], as.numeric)
post.data$post.SPANE_N <- rowSums(post.data[, spane.N_sum_post.col])

post.data$post.SPANE_B <- post.data$post.SPANE_P - post.data$post.SPANE_N

#View(post.data)


##FLOUSC


#Columns to be Summed- pre-test

flousc_sum_pre.col <- c(59:66)


#Columns to be Summed- post-test

flousc_sum_post.col <- c(58:65)


#Columns Creation- pre-test

pre.data[, flousc_sum_pre.col] <- lapply(pre.data[, flousc_sum_pre.col], as.numeric)
pre.data$pre.FLOUSC <- rowSums(pre.data[, flousc_sum_pre.col])

#View(pre.data)


#Columns Creation- post-test

post.data[, flousc_sum_post.col] <- lapply(post.data[, flousc_sum_post.col], as.numeric)
post.data$post.FLOUSC <- rowSums(post.data[, flousc_sum_post.col])

#View(post.data)

#-----------------------------

##Feedback


#Columns Creation- pre-test

pre.data$GPT_Use <- pre.data$How.often.do.you.use.ChatGPT
#View(pre.data)

#Columns Creation- post-test

post.data$GPT_Rating <- post.data$How.would.you.rate.your.experience.with.ChatGPT
post.data$Prompts_Rating <- post.data$How.would.you.rate.the.responses.generated.by.ChatGPT.to.the.prompts..questions.provided.
post.data$Intervention_Rating <- post.data$How.would.you.rate.the.intervention.as.a.whole.
post.data$MHL_Rating <- post.data$Do.you.feel.you.are.now.more.knowledgeable.about.mental.health.as.a.result.of.the.intervention.
post.data$GPT_MHL <- post.data$Would.you.consider.using.ChatGPT.to.get.information.regarding.mental.health.in.the.future.
post.data$GPT_Flaw <- post.data$Did.you..at.any.point..feel.that.the.information.provided.by.chatGPT.was.flawed.
  
#View(post.data) 


#-----------------------------
#-----------------------------

##Refining Codebooks


#Codebook 2- Pre-test Data

pre.col_order <- seq_along(pre.data)
pre.col_names <- names(pre.data)
pre.col_info <-data.frame(pre.col_order, pre.col_names)
#View(pre.col_info)
write.csv(pre.col_info, "Codebook2_pretest.csv")

#Codebook 2- Post-test Data

post.col_order <- seq_along(post.data)
post.col_names <- names(post.data)
post.col_info <-data.frame(post.col_order, post.col_names)
#View(post.col_info)
write.csv(post.col_info, "Codebook2_posttest.csv")

  
#-----------------------------
#-----------------------------

###Creating Final Data Sheet for Analysis


##Deleting Duplicates

con.data <- con.data[!duplicated(con.data$Username), ]
pre.data <- pre.data[!duplicated(pre.data$Username), ]
post.data <- post.data[!duplicated(post.data$Username), ]

#View(con.data)
#View(pre.data)
#View(post.data)


## Selecting Columns to be included into the Final Data Sheet


con.data_fdata <- con.data[, c(2, 5:7)]
pre.data_fdata <- pre.data[, c(2, 79, 67:78)]
post.data_fdata <- post.data[, c(2, 75:92)]


# Merge the three files based on the common column


merged_data1 <- merge(con.data_fdata, pre.data_fdata, by = "Username")
merged_data2 <- merge(merged_data1, post.data_fdata, by = "Username")
fdata <- merged_data2


dim(fdata)
missing_rows_pre <- anti_join(pre.data, fdata, by = "Username")
missing_rows_post <- anti_join(post.data, fdata, by = "Username")
missing_rows_con <- anti_join(con.data, fdata, by = "Username")

#View(missing_rows_con)
#View(missing_rows_pre)
#View(missing_rows_post)

#View(fdata)

write.csv(fdata, "Data for Analysis.csv", row.names = FALSE)


#-----------------------------
#-----------------------------


###Cronbach's Alpha

#install.packages("psych")
library(psych)


##MHLq-SVa

alpha_MHL <- alpha(pre.data[, 7:22])

##MHSAS

alpha_MHSAS <- alpha(pre.data[, 23:31])

##STIG-9

alpha_STIG <- alpha(pre.data[, 32:40])

##MHSES

alpha_MHSES <- alpha(pre.data[, 41:46])

##SPANE

alpha_SPANE_P <- alpha(pre.data[, c(47,49,51,53,56,58)])
alpha_SPANE_N <- alpha(pre.data[, c(48,50,52,54,55,57)])

#FLOUSC 

alpha_FLOUSC <- alpha(pre.data[, 59:66])


var.alpha <- c("MHL", "MHSAS", "STIG", "MHSES", "SPANE-P", "SPANE-N", "FLOUSC")
score.alpha <- c(alpha_MHL$total$raw_alpha, alpha_MHSAS$total$raw_alpha, alpha_STIG$total$raw_alpha, alpha_MHSES$total$raw_alpha, alpha_SPANE_P$total$raw_alpha, alpha_SPANE_N$total$raw_alpha, alpha_FLOUSC$total$raw_alpha)

cronbachs_alpha <- data.frame(var.alpha, score.alpha)

cronbachs_alpha

write.csv(cronbachs_alpha, "Cronbach's_Alpha.csv", row.names = FALSE)


#-----------------------------

###Line Graphs


mean_pre.MHL <- mean(fdata$pre.MHL_Total)
mean_post.MHL <- mean(fdata$post.MHL_Total)
mean_pre.MHSAS <- mean(fdata$pre.MHSAS)
mean_post.MHSAS <- mean(fdata$post.MHSAS)
mean_pre.STIG <- mean(fdata$pre.STIG)
mean_post.STIG <- mean(fdata$post.STIG)
mean_pre.MHSES <- mean(fdata$pre.MHSES)
mean_post.MHSES <- mean(fdata$post.MHSES)
mean_pre.SPANE <- mean(fdata$pre.SPANE_B)
mean_post.SPANE <- mean(fdata$post.SPANE_B)
mean_pre.FLOUSC <- mean(fdata$pre.FLOUSC)
mean_post.FLOUSC <- mean(fdata$post.FLOUSC)


line_graph_data <- matrix(c("MHL", mean_pre.MHL, mean_post.MHL,
                         "MHSAS", mean_pre.MHSAS, mean_post.MHSAS,
                         "STIG", mean_pre.STIG, mean_post.STIG,
                         "MHSES", mean_pre.MHSES, mean_post.MHSES,
                         "SPANE", mean_pre.SPANE, mean_post.SPANE,
                         "FLOUSC", mean_pre.FLOUSC, mean_post.FLOUSC),
                       nrow = 6, ncol= 3, byrow = TRUE,
                       dimnames = list(c(),
                                       c("Variable", "pre_mean", "post_mean")))



line_graph <- t(line_graph_data)

write.csv(line_graph, "Line.Graph_Data.csv", row.names = FALSE)





