setwd("/Users/lizhang/Desktop/100FINAL")
library(ggplot2)
df <- read.csv("bs100_final_data.csv")
str(df)

#partI
figure1 <- ggplot(df, aes(x = alc)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Alcohol Use", x = "Days of Alcohol Use", y ="Number of People" ) +
  theme_minimal()
figure1
figure2 <- ggplot(df,aes(x="",y=alc))+
  geom_boxplot(aes(y = alc), fill = "lightgreen", color = "black")+
  labs(title = "Boxplot of Alcohol Use",x="Alcohol", y = "Days of Alcohol Use")+
  theme_minimal()
figure2
ggsave("outliers.png",figure2)
ggsave("Ib.png", figure1)

figure3 <- ggplot(df, aes(x = health_lit, y = alc)) +
  geom_point() +
  geom_smooth(color = "skyblue") +
  labs(title = "Bivariate Plot of Health Literacy and Health Outcome",
       x = "Health Literacy",
       y = "Alcohol Use") +
  theme_minimal()
figure3
ggsave("figure3.png", figure3, width = 8, height = 6, dpi = 300)

cor(df$health_lit, df$alc)

#Create a binary variable of high health literacy
df$high_health_literacy <- ifelse(df$health_lit >= 45, 1, 0)

####partII 

#health_lit
lit_dis<- ggplot(df, aes(x = health_lit)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Health Literacy", x = "Health Literacy", y = "Frequency") +
  theme_minimal()
ggsave("distribution_lit.png",lit_dis)

lit_dis_2 <- ggplot(df,aes(x="",y=health_lit))+
  geom_boxplot(aes(y = health_lit), fill = "skyblue", color = "black")+
  labs(title = "Boxplot of Health Literacy",x="", y = "Health Literacy")
lit_dis_2
ggsave("distribution_lit_2.png",lit_dis_2)

#sex
sex_dis <- ggplot(df, aes(x = factor(sex))) +
  geom_bar(aes(fill = factor(sex)), color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Sex", x = "Sex", y = "Count")+
  scale_fill_manual(values = c("0" = "skyblue", "1" = "pink"),name = "Sex",labels = c("0" = "Male", "1" = "Female"))+
  scale_x_discrete(labels = c("0" = "Male", "1" = "Female"))
sex_dis
ggsave("distribution_sex.png",sex_dis)

sex_alc <- ggplot(df, aes(x = factor(sex), y = alc, fill = factor(sex))) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white", color = "black") +
  theme_minimal() +
  labs(title = "Alcohol Consumption by Sex", x = "Sex", y = "Days of Alcohol Consumption")+
  scale_fill_manual(values = c("0" = "skyblue", "1" = "pink"), name = "Sex", labels = c("0" = "Male", "1" = "Female"))+
  scale_x_discrete(labels = c("0" = "Male", "1" = "Female"))
theme(legend.position = "none") 
ggsave("sex_alc.png",sex_alc)

#pol
pol_dis <- ggplot(df, aes(x = factor(pol))) +
  geom_bar(aes(fill = factor(pol)), color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Living above the poverty line", x = "Living above the poverty line", y = "Count")+
  scale_fill_manual(values = c("0" = "yellow", "1" = "lightgreen"),name = "Living above the poverty line",labels = c("0" = "No", "1" = "Yes"))+
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))
pol_dis
ggsave("distribution_pol.png",pol_dis)

pol_alc <- ggplot(df, aes(x = factor(pol), y = alc, fill = factor(pol))) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white", color = "black") +
  theme_minimal() +
  labs(title = "Alcohol Consumption by Poverty Status", x = "Above Poverty Line", y = "Days of Alcohol Consumption")+
  scale_fill_manual(values = c("0" = "yellow", "1" = "lightgreen"),name = "Living above the poverty line",labels = c("0" = "No", "1" = "Yes"))+
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))
pol_alc
ggsave("pol_alc.png",pol_alc)

#daily_fol
fol_dis<- ggplot(df, aes(x = daily_fol)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  geom_vline(xintercept = 400, linetype = "dashed", color = "red", size = 1.5) +
  geom_text(aes(x = 310, y = -2, label = "Recommended amount"), color = "red", hjust = -0.5) +
  labs(title = "Distribution of Daily Total Folate Intake", x = "Daily Total Folate Intake", y = "Frequency") +
  theme_minimal()
fol_dis
ggsave("distribution_fol.png",fol_dis)

fol_dis_2 <- ggplot(df,aes(x="",y=daily_fol))+
  geom_boxplot(aes(y = daily_fol), fill = "skyblue", color = "black")+
  geom_hline(yintercept = 400, linetype = "dashed", color = "red") +
  geom_text(aes(x = 0.6, y = 400, label = "Recommended amount"), color = "red", vjust = -0.5) +
  labs(title = "Boxplot of Daily Total Folate Intake ",x="", y = "Daily Total Folate Intake(micrograms)")
fol_dis_2
ggsave("distribution_fol_2.png",fol_dis_2)

fol_alc <- ggplot(df, aes(x = daily_fol, y = alc)) +
  geom_point() +
  geom_smooth(color = "skyblue") +
  geom_vline(xintercept = 400, linetype = "dashed", color = "red") +
  geom_text(aes(x = 400, y = 0, label = "Recommended amount"), color = "red", vjust = -0.5) +
  labs(title = "Bivariate Plot of Daily Folate and Alcohol Use",
       x = "Daily Folate",
       y = "Alcohol Use") +
  theme_minimal()
fol_alc
ggsave("fol_alc.png",fol_alc)


#ins
ins_dis <- ggplot(df, aes(x = factor(ins))) +
  geom_bar(aes(fill = factor(ins)), color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Insurance status", x = "Insurance status", y = "Count")+
  scale_fill_manual(values = c("0" = "#ADD8E6", "1" = "#4682B4", "2" = "yellow"),
                    name = "Insurance status",
                    labels = c("0" = "Public Insurance", "1" = "Private Insurance", "2" = "Uninsured")) +
ins_dis
ggsave("distribution_ins.png",ins_dis)

ins_alc <- ggplot(df, aes(x = factor(ins), y = alc, fill = factor(ins))) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white", color = "black") +
  theme_minimal() +
  labs(title = "Alcohol Consumption by Insurance status", x = "Insurance status", y = "Days of Alcohol Consumption")+
  scale_fill_manual(values = c("0" = "#ADD8E6", "1" = "#4682B4", "2" = "yellow"),
                    name = "Insurance status",
                    labels = c("0" = "Public Insurance", "1" = "Private Insurance", "2" = "Uninsured")) +
  scale_x_discrete(labels = c("0" = "Public Insurance", "1" = "Private Insurance","2"="Uninsured"))
ins_alc
ggsave("ins_alc.png",ins_alc)


#educ
educ_dis <- ggplot(df, aes(x = factor(educ))) +
  geom_bar(aes(fill = factor(educ)), color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Highest Level of Education", 
       x = "Highest Level of Education", 
       y = "Count") +
  scale_fill_manual(values = c("#E0FFDB", "#B6E3A8", "#8CC084", "#5E8C61", "#2D5E36"),
                    name = "Highest Level of Education",
                    labels = c("0" = "Elementary school education",
                               "1" = "High school graduate",
                               "2" = "Some college",
                               "3" = "College degree",
                               "4" = "Graduate degree")) 
educ_dis
ggsave("educ_dis.png", educ_dis)

educ_alc <- ggplot(df, aes(x = factor(educ), y = alc, fill = factor(educ))) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white", color = "black") +
  theme_minimal() +
  labs(title = "Alcohol Consumption by Highest Level of Education", 
       x = "Highest Level of Education", 
       y = "Days of Alcohol Consumption") +
  scale_fill_manual(values = c("#E0FFDB", "#B6E3A8", "#8CC084", "#5E8C61", "#2D5E36"),
                    name = "Highest Level of Education",
                    labels = c("0" = "Elementary school education",
                               "1" = "High school graduate",
                               "2" = "Some college",
                               "3" = "College degree",
                               "4" = "Graduate degree")) +
  scale_x_discrete(labels = c("0" = "Elementary", "1" = "High School", "2" = "Some College", "3" = "College", "4" = "Graduate"))
educ_alc
ggsave("educ_alc.png", educ_alc)







#partIII
#Create a binary variable of high health literacy
df$high_health_literacy <- ifelse(df$health_lit >= 45, 1, 0)


library(dplyr)
library(tidyr)


#for categorical variables
summary_table <- function(data, grouping_var) {
  result <- data %>%
    group_by({{ grouping_var }}) %>%
    summarise(
      Total = n(),
      `h_lit_N` = sum(high_health_literacy == 1),
      `l_lit_N` = sum(high_health_literacy == 0)
    ) %>%
    mutate(
      "{{grouping_var}}_pct" = sprintf("%.2f%%", (Total / sum(Total)) * 100),
      `h_lit_pct` = sprintf("%.2f%%", (`h_lit_N` / sum(`h_lit_N`)) * 100),
      `l_lit_pct` = sprintf("%.2f%%", (`l_lit_N` / sum(`l_lit_N`)) * 100)
    )
  
  return(result)
}

# apply
summary_table_by_sex <- summary_table(df, sex)
summary_table_by_pol <- summary_table(df, pol)
summary_table_by_sex
summary_table_by_pol
summary_table_by_daily_ins <- summary_table(df, ins)
summary_table_by_daily_ins
summary_table_by_daily_educ <- summary_table(df, educ)
summary_table_by_daily_educ




#for continuous variables
continuous_summary_table <- function(data, grouping_var, variable) {
  result <- data %>%
    group_by({{ grouping_var }}) %>%
    summarise(
      mean = sprintf("%.2f", mean({{ variable }})),
      sd = sprintf("%.2f", sd({{ variable }}))
    ) 
  
  return(result)
}

# apply
summary_table_by_fol <- continuous_summary_table(df, high_health_literacy, daily_fol)
summary_table_by_fol
mean(df$daily_fol)
sd(df$daily_fol)
summary_table_by_alc <- continuous_summary_table(df, high_health_literacy, alc)
summary_table_by_alc
mean(df$alc)
sd(df$alc)
summary_table_by_fol


#p_value
chisq.test(df$high_health_literacy, df$sex)
chisq.test(df$high_health_literacy, df$pol)
chisq.test(df$high_health_literacy, df$pol)
chisq.test(df$high_health_literacy, df$ins)
fisher.test(df$high_health_literacy, df$educ)

t.test(df$daily_fol[df$high_health_literacy==1],
       df$daily_fol[df$high_health_literacy==0])
t.test(df$alc[df$high_health_literacy==1],
       df$alc[df$high_health_literacy==0])


table2 <- lm(formula = alc ~ health_lit, data = df)
summary(table2)


