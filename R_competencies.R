#General library set up


#Installing the package with the database
install.packages('medicaldata')

install.packages('tinytext')

#importing the library 
library(medicaldata)
data(strep_tb)

?strep_tb

library(tidyverse)
glimpse(strep_tb)

#It has 107 rows
#13 columns

head(strep_tb)
summary(strep_tb)

#simple table to count varaibles
table(strep_tb$gender)

#Deteriming the rows
which(strep_tb$gender == "F" & strep_tb$baseline_condition == "3_Poor")


#Analyis using dplyr library for efficiency
f_improvemnet_6m <- strep_tb %>%
  filter(gender == "F", baseline_condition == "3_Poor") %>%
  summarise(
    Total = n(),
    Improved = sum(improved),
    Percentage = paste((sum(improved) / n()) * 100, "%", sep = "")
  )


improvement_rates <- strep_tb %>%
  filter(arm == "Streptomycin") %>%
  group_by(gender) %>%
  summarise(
    Total = n(),
    Improved = sum(improved),
    Percentage = (sum(improved) / n()) * 100
  )


dose_0 <- strep_tb %>% 
  filter(gender == "F", dose_strep_g == 0, dose_PAS_g == 0) %>% 
  summarise(
    Group = "Dose 0",
    Median = median(rad_num),
    IQR = IQR(rad_num)
  )

dose_2 <- strep_tb %>% 
  filter(gender == "F", dose_strep_g == 2, dose_PAS_g == 0) %>% 
  summarise(
    Group = "Dose 0",
    Median = median(rad_num),
    IQR = IQR(rad_num)
  )

#Histogram 
rad_f_0 <-  strep_tb %>%
  filter(gender == "F", dose_strep_g == 0, dose_PAS_g == 0) %>% 
  group_by(rad_num)

ggplot(rad_f_0, aes(x = rad_num)) +
  geom_histogram(binwidth = 0.5, color = "purple", fill = "purple") +
  geom_vline(xintercept = dose_0$Median, color = "black", linewidth = 1) +
  geom_vline(xintercept = dose_0$Median - dose_0$IQR/2, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = dose_0$Median + dose_0$IQR/2, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Rad_Num for dose 0", x = "rad_num", y = "frequency") +
  theme_minimal()

#Histogram
rad_f_2 <-  strep_tb %>%
  filter(gender == "F", dose_strep_g == 2, dose_PAS_g == 0) %>% 
  group_by(rad_num)

ggplot(rad_f_2, aes(x = rad_num)) +
  geom_histogram(binwidth = 0.5, color = "purple", fill = "pink") +
  geom_vline(xintercept = dose_2$Median, color = "black", linewidth = 1) +
  geom_vline(xintercept = dose_2$Median - dose_0$IQR/2, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = dose_2$Median + dose_0$IQR/2, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Rad_Num for Dose 2", x = "Rad_Num", y = "Frequency") +
  theme_minimal()

combined_table <- strep_tb %>%
  filter(dose_PAS_g == 0, dose_strep_g %in% c(0, 2)) %>%
  group_by(dose_strep_g) %>%
  summarize(
    Median = median(rad_num, na.rm = TRUE),
    Q1 = quantile(rad_num, 0.25, na.rm = TRUE),
    Q3 = quantile(rad_num, 0.75, na.rm = TRUE)
  )

#Box
ggplot(strep_tb, aes(x = factor(dose_strep_g), y = rad_num)) +
  geom_boxplot() +
  labs(y = 'Rad_Num',
       x = 'Dose of Streptomycin (g)')


#Point line 
ggplot(strep_summary, aes(x = dose_strep_g, color = dose_strep_g)) +
  geom_pointrange(aes(y = Median,
                      ymin = Q1,
                      ymax = Q3),
                  show.legend = FALSE) +
  labs(y = 'Rad_Num',
       x = 'Dose of Streptomycin (g)')

