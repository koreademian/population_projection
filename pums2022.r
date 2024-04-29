
library(car)
library(haven)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(glue)
library(tibble)
library(ggpubr)
library(cowplot)
library(writexl)
library(tidyr)
pums2022_p<-read_sas("Y:/Data/Census/programs-surveys/acs/data/pums/2022/1-Year/sas/psam_p48.sas7bdat")
pums2022_h<-read_sas("Y:/Data/Census/programs-surveys/acs/data/pums/2022/1-Year/sas/psam_h48.sas7bdat")

#multi-generational family
h22 <- pums2022_h[, c("WGTP","SERIALNO","MULTG" )]
p22 <- pums2022_p[, c("PWGTP","SERIALNO", "NATIVITY", "HISP","RAC1P")]
# Merge the two data frames based on SERIALNO
mh22 <- merge(h22, p22, by = "SERIALNO")
Total<-mh22
# filter each group of interest
mh22$Nativity_Status <- ifelse(mh22$NATIVITY == '1', "US-Born", "Foreign-Born")
mh22$Multgen_Status <- ifelse(mh22$MULTG == '2', "Multi-generational", "Not-Multigenerational")
Asian <- mh22[mh22$HISP == "01" & mh22$RAC1P == "6", ]
NH_White <- mh22[mh22$HISP == "01" & mh22$RAC1P == "1", ]
Hisp <- mh22[mh22$HISP != "01", ]
Hispanic_US_born <- mh22[mh22$HISP != "01" & mh22$NATIVITY == "1", ]
Hispanic_Foreign_born <- mh22[mh22$HISP != "01" & mh22$NATIVITY == "2", ]
Black <- mh22[mh22$HISP == "01" & mh22$RAC1P == "2", ]
# Calculate the sum of PWGTP by mutigenerational status combining with serialno then create hispanic_US_born and hispanic_foreign_born multigenerational status using pwgpt weight
asian_table <- aggregate(PWGTP ~ Multgen_Status , data = Asian, FUN = sum)
# Rename the PWGTP variable to Asian2022
names(asian_table)[names(asian_table) == "PWGTP"] <- "Asian2022"
#do the same for NH-White
nh_white_table <- aggregate(PWGTP ~ Multgen_Status, data = NH_White, FUN = sum)
names(nh_white_table)[names(nh_white_table) == "PWGTP"] <- "NH-White2022"
#do the same for Hispanic
Hisp_table <- aggregate(PWGTP ~ Multgen_Status, data = Hisp, FUN = sum)
names(Hisp_table)[names(Hisp_table) == "PWGTP"] <- "Hisp2022"
#do the same for us_born_Hispanic
hispanic_us_born_table <- aggregate(PWGTP ~ Multgen_Status, data = Hispanic_US_born, FUN = sum)
names(hispanic_us_born_table)[names(hispanic_us_born_table) == "PWGTP"] <- "US_born_Hisp2022"
#do the same for foreign_born_Hispanic
hispanic_foreign_born_table <- aggregate(PWGTP ~ Multgen_Status , data = Hispanic_Foreign_born, FUN = sum)
names(hispanic_foreign_born_table)[names(hispanic_foreign_born_table) == "PWGTP"] <- "Foreign_born_Hisp2022"
#do the same for Black
black_table <- aggregate(PWGTP ~ Multgen_Status , data = Black, FUN = sum)
names(black_table)[names(black_table) == "PWGTP"] <- "Black2022"
#do the same for total
total_table <- aggregate(PWGTP ~ Multgen_Status , data = mh22, FUN = sum)
names(total_table)[names(total_table) == "PWGTP"] <- "All2022"
#combine
# Merge Asian and NH-White tables
combined_table <- merge(asian_table, nh_white_table, by = "Multgen_Status", all = TRUE)
combined_table <- merge(combined_table, Hisp_table, by = "Multgen_Status", all = TRUE)
combined_table <- merge(combined_table, hispanic_us_born_table, by = "Multgen_Status", all = TRUE)
combined_table <- merge(combined_table, hispanic_foreign_born_table, by = "Multgen_Status", all = TRUE)
combined_table <- merge(combined_table, black_table, by = "Multgen_Status", all = TRUE)
combined_table <- merge(combined_table, total_table, by = "Multgen_Status", all = TRUE)
multigen2022<-combined_table
View(multigem2022)
#2011
pums2011_p<-read_sas("Y:/Data/Census/programs-surveys/acs/data/pums/2011/1-Year/sas/psam_p48.sas7bdat")
pums2011_h<-read_sas("Y:/Data/Census/programs-surveys/acs/data/pums/2011/1-Year/sas/psam_h48.sas7bdat")

#multi-generational family
h11 <- pums2011_h[, c("WGTP","SERIALNO","MULTG" )]
p11 <- pums2011_p[, c("PWGTP","SERIALNO", "NATIVITY", "HISP","RAC1P")]
# Merge the two data frames based on SERIALNO
mh11 <- merge(h11, p11, by = "SERIALNO")
Total<-mh11
# filter each group of interest
mh11$Nativity_Status <- ifelse(mh11$NATIVITY == '1', "US-Born", "Foreign-Born")
mh11$Multgen_Status <- ifelse(mh11$MULTG == '2', "Multi-generational", "Not-Multigenerational")
Asian <- mh11[mh11$HISP == "01" & mh11$RAC1P == "6", ]
NH_White <- mh11[mh11$HISP == "01" & mh11$RAC1P == "1", ]
Hisp <- mh11[mh11$HISP != "01", ]
Hispanic_US_born <- mh11[mh11$HISP != "01" & mh11$NATIVITY == "1", ]
Hispanic_Foreign_born <- mh11[mh11$HISP != "01" & mh11$NATIVITY == "2", ]
Black <- mh11[mh11$HISP == "01" & mh11$RAC1P == "2", ]
# Calculate the sum of PWGTP by mutigenerational status combining with serialno then create hispanic_US_born and hispanic_foreign_born multigenerational status using pwgpt weight
asian_table <- aggregate(PWGTP ~ Multgen_Status , data = Asian, FUN = sum)
# Rename the PWGTP variable to Asian2011
names(asian_table)[names(asian_table) == "PWGTP"] <- "Asian2011"
#do the same for NH-White
nh_white_table <- aggregate(PWGTP ~ Multgen_Status, data = NH_White, FUN = sum)
names(nh_white_table)[names(nh_white_table) == "PWGTP"] <- "NH-White2011"
#do the same for Hispanic
Hisp_table <- aggregate(PWGTP ~ Multgen_Status, data = Hisp, FUN = sum)
names(Hisp_table)[names(Hisp_table) == "PWGTP"] <- "Hisp2011"
#do the same for us_born_Hispanic
hispanic_us_born_table <- aggregate(PWGTP ~ Multgen_Status, data = Hispanic_US_born, FUN = sum)
names(hispanic_us_born_table)[names(hispanic_us_born_table) == "PWGTP"] <- "US_born_Hisp2011"
#do the same for foreign_born_Hispanic
hispanic_foreign_born_table <- aggregate(PWGTP ~ Multgen_Status , data = Hispanic_Foreign_born, FUN = sum)
names(hispanic_foreign_born_table)[names(hispanic_foreign_born_table) == "PWGTP"] <- "Foreign_born_Hisp2011"
#do the same for Black
black_table <- aggregate(PWGTP ~ Multgen_Status , data = Black, FUN = sum)
names(black_table)[names(black_table) == "PWGTP"] <- "Black2011"
#do the same for total
total_table <- aggregate(PWGTP ~ Multgen_Status , data = mh11, FUN = sum)
names(total_table)[names(total_table) == "PWGTP"] <- "All2011"
#combine
# Merge Asian and NH-White tables
combined_table <- merge(asian_table, nh_white_table, by = "Multgen_Status", all = TRUE)
combined_table <- merge(combined_table, Hisp_table, by = "Multgen_Status", all = TRUE)
combined_table <- merge(combined_table, hispanic_us_born_table, by = "Multgen_Status", all = TRUE)
combined_table <- merge(combined_table, hispanic_foreign_born_table, by = "Multgen_Status", all = TRUE)
combined_table <- merge(combined_table, black_table, by = "Multgen_Status", all = TRUE)
combined_table <- merge(combined_table, total_table, by = "Multgen_Status", all = TRUE)
multigen2011<-combined_table
View(multigen2011)

#2016
pums2016_p<-read_sas("Y:/Data/Census/programs-surveys/acs/data/pums/2016/1-Year/sas/psam_p48.sas7bdat")
pums2016_h<-read_sas("Y:/Data/Census/programs-surveys/acs/data/pums/2016/1-Year/sas/psam_h48.sas7bdat")

#multi-generational family
h16 <- pums2016_h[, c("WGTP","SERIALNO","MULTG" )]
p16 <- pums2016_p[, c("PWGTP","SERIALNO", "NATIVITY", "HISP","RAC1P")]
# Merge the two data frames based on SERIALNO
mh16 <- merge(h16, p16, by = "SERIALNO")
Total<-mh16
# filter each group of interest
mh16$Nativity_Status <- ifelse(mh16$NATIVITY == '1', "US-Born", "Foreign-Born")
mh16$Multgen_Status <- ifelse(mh16$MULTG == '2', "Multi-generational", "Not-Multigenerational")
Asian <- mh16[mh16$HISP == "01" & mh16$RAC1P == "6", ]
NH_White <- mh16[mh16$HISP == "01" & mh16$RAC1P == "1", ]
Hisp <- mh16[mh16$HISP != "01", ]
Hispanic_US_born <- mh16[mh16$HISP != "01" & mh16$NATIVITY == "1", ]
Hispanic_Foreign_born <- mh16[mh16$HISP != "01" & mh16$NATIVITY == "2", ]
Black <- mh16[mh16$HISP == "01" & mh16$RAC1P == "2", ]
# Calculate the sum of PWGTP by mutigenerational status combining with serialno then create hispanic_US_born and hispanic_foreign_born multigenerational status using pwgpt weight
asian_table <- aggregate(PWGTP ~ Multgen_Status , data = Asian, FUN = sum)
# Rename the PWGTP variable to Asian2016
names(asian_table)[names(asian_table) == "PWGTP"] <- "Asian2016"
#do the same for NH-White
nh_white_table <- aggregate(PWGTP ~ Multgen_Status, data = NH_White, FUN = sum)
names(nh_white_table)[names(nh_white_table) == "PWGTP"] <- "NH-White2016"
#do the same for Hispanic
Hisp_table <- aggregate(PWGTP ~ Multgen_Status, data = Hisp, FUN = sum)
names(Hisp_table)[names(Hisp_table) == "PWGTP"] <- "Hisp2016"
#do the same for us_born_Hispanic
hispanic_us_born_table <- aggregate(PWGTP ~ Multgen_Status, data = Hispanic_US_born, FUN = sum)
names(hispanic_us_born_table)[names(hispanic_us_born_table) == "PWGTP"] <- "US_born_Hisp2016"
#do the same for foreign_born_Hispanic
hispanic_foreign_born_table <- aggregate(PWGTP ~ Multgen_Status , data = Hispanic_Foreign_born, FUN = sum)
names(hispanic_foreign_born_table)[names(hispanic_foreign_born_table) == "PWGTP"] <- "Foreign_born_Hisp2016"
#do the same for Black
black_table <- aggregate(PWGTP ~ Multgen_Status , data = Black, FUN = sum)
names(black_table)[names(black_table) == "PWGTP"] <- "Black2016"
#do the same for total
total_table <- aggregate(PWGTP ~ Multgen_Status , data = mh16, FUN = sum)
names(total_table)[names(total_table) == "PWGTP"] <- "All2016"
#combine
# Merge Asian and NH-White tables
combined_table <- merge(asian_table, nh_white_table, by = "Multgen_Status", all = TRUE)
combined_table <- merge(combined_table, Hisp_table, by = "Multgen_Status", all = TRUE)
combined_table <- merge(combined_table, hispanic_us_born_table, by = "Multgen_Status", all = TRUE)
combined_table <- merge(combined_table, hispanic_foreign_born_table, by = "Multgen_Status", all = TRUE)
combined_table <- merge(combined_table, black_table, by = "Multgen_Status", all = TRUE)
combined_table <- merge(combined_table, total_table, by = "Multgen_Status", all = TRUE)
multigen2016<-combined_table
View(multigen2016)

# Load the openxlsx library if not already loaded
library(openxlsx)

# Create a new Excel workbook
wb <- createWorkbook()

# Add each data frame to a separate sheet
addWorksheet(wb, sheetName = "Multigen2011")
writeData(wb, sheet = "Multigen2011", x = multigen2011)

addWorksheet(wb, sheetName = "Multigen2016")
writeData(wb, sheet = "Multigen2016", x = multigen2016)

addWorksheet(wb, sheetName = "Multigen2022")
writeData(wb, sheet = "Multigen2022", x = multigen2022)

# Save the workbook to an Excel file
saveWorkbook(wb, file = "C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/multigen_data.xlsx")


write_xlsx(combined_table, "C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/disability_6group_2011.xlsx")

#disability by age and race and nativity
# Create the ch1 data frame
p22 <- pums2022_p[, c("WGTP","SERIALNO","MULTG")]

# Create a new variable for disability status and age groups
ch1$Disability_Status <- ifelse(ch1$DIS == '1', "With a disability", "Without a disability")
ch1$Age_Group <- cut(ch1$AGEP, breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, 94, 99, Inf), labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+"), right = FALSE)

# Create subsets for each race and ethnicity group
Asian <- ch1[ch1$HISP == "01" & ch1$RAC1P == "6", ]
NH_White <- ch1[ch1$HISP == "01" & ch1$RAC1P == "1", ]
Hispanic_US_born <- ch1[ch1$HISP != "01" & ch1$NATIVITY == "1", ]
Hispanic_Foreign_born <- ch1[ch1$HISP != "01" & ch1$NATIVITY == "2", ]
Black <- ch1[ch1$HISP == "01" & ch1$RAC1P == "2", ]
Total <- ch1

# Calculate the sum of PWGTP by disability status and age group for each race and ethnicity group
asian_table <- aggregate(PWGTP ~ Disability_Status + Age_Group, data = Asian, FUN = sum)
nh_white_table <- aggregate(PWGTP ~ Disability_Status + Age_Group, data = NH_White, FUN = sum)
hispanic_us_born_table <- aggregate(PWGTP ~ Disability_Status + Age_Group, data = Hispanic_US_born, FUN = sum)
hispanic_foreign_born_table <- aggregate(PWGTP ~ Disability_Status + Age_Group, data = Hispanic_Foreign_born, FUN = sum)
black_table <- aggregate(PWGTP ~ Disability_Status + Age_Group, data = Black, FUN = sum)
total_table <- aggregate(PWGTP ~ Disability_Status + Age_Group, data = Total, FUN = sum)

# Rename the PWGTP columns for each race and ethnicity group
colnames(asian_table)[3] <- "Asian"
colnames(nh_white_table)[3] <- "NH-White"
colnames(hispanic_us_born_table)[3] <- "Hispanic (US-born)"
colnames(hispanic_foreign_born_table)[3] <- "Hispanic (Foreign-born)"
colnames(black_table)[3] <- "Black"
colnames(total_table)[3] <- "Total"

# Merge the tables into a single data frame with suffixes
combined_table <- merge(asian_table, nh_white_table, by = c("Disability_Status", "Age_Group"), all = TRUE, suffixes = c("_Asian", "_NH-White"))
combined_table <- merge(combined_table, hispanic_us_born_table, by = c("Disability_Status", "Age_Group"), all = TRUE, suffixes = c("", "_Hispanic (US-born)"))
combined_table <- merge(combined_table, hispanic_foreign_born_table, by = c("Disability_Status", "Age_Group"), all = TRUE, suffixes = c("", "_Hispanic (Foreign-born)"))
combined_table <- merge(combined_table, black_table, by = c("Disability_Status", "Age_Group"), all = TRUE, suffixes = c("",  "_Black"))
combined_table <- merge(combined_table, total_table, by = c("Disability_Status", "Age_Group"), all = TRUE, suffixes = c("",  "_Total"))

# Reorder the rows in the combined table by age group
combined_table <- combined_table %>%
  arrange(Age_Group)

# Print the reordered combined table
print(combined_table)

write_xlsx(combined_table, "C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/disability_6group_2011.xlsx")
















# Create a new variable for disability status, nativity, Hispanic, and age groups
ch1$Disability_Status <- ifelse(ch1$DIS == '1', "With a disability", "Without a disability")
ch1$Nativity_Status <- ifelse(ch1$NATIVITY == '1', "US-Born", "Foreign-Born")
ch1$Hispanic_Status <- ifelse(ch1$HISP == '01', "Non-Hispanic","Hispanic",)
ch1$Age_Group <- cut(ch1$AGEP, breaks = c(0, 18, 65, Inf), labels = c("Under 18", "19-65", "Over 65"), right = FALSE)

# Subset the data for US-born Hispanics and calculate the sum of PWGTP by age group
us_born_hispanic_data <- ch1[ch1$Nativity_Status == "US-Born" & ch1$Hispanic_Status == "Hispanic", ]
us_born_hispanic_table <- aggregate(PWGTP ~ Disability_Status + Age_Group, data = us_born_hispanic_data, FUN = sum)

# Subset the data for non-US-born Hispanics and calculate the sum of PWGTP by age group
non_us_born_hispanic_data <- ch1[ch1$Nativity_Status != "US-Born" & ch1$Hispanic_Status == "Hispanic", ]
non_us_born_hispanic_table <- aggregate(PWGTP ~ Disability_Status + Age_Group, data = non_us_born_hispanic_data, FUN = sum)

# Combine the tables into a single data frame
combined_table <- merge(us_born_hispanic_table, non_us_born_hispanic_table, by = c("Disability_Status", "Age_Group"), all = TRUE)
combined_table <- combined_table[order(combined_table$Age_Group), ]

# Rename the column "PWGTP.x" to "US-Born Hispanic" and "PWGTP.y" to "Foreign-Born Hispanic"
names(combined_table)[names(combined_table) == "PWGTP.x"] <- "US-Born Hispanic"
names(combined_table)[names(combined_table) == "PWGTP.y"] <- "Foreign-Born Hispanic"

# Add row names
rownames(combined_table) <- 1:nrow(combined_table)

# Print the combined table
print(combined_table)

# Subset the data for US-born Non-Hispanics and calculate the sum of PWGTP by age group
us_born_non_hispanic_data <- ch1[ch1$Nativity_Status == "US-Born" & ch1$Hispanic_Status == "Non-Hispanic", ]
us_born_non_hispanic_table <- aggregate(PWGTP ~ Disability_Status + Age_Group, data = us_born_non_hispanic_data, FUN = sum)

# Subset the data for Foreign-born Non-Hispanics and calculate the sum of PWGTP by age group
foreign_born_non_hispanic_data <- ch1[ch1$Nativity_Status == "Foreign-Born" & ch1$Hispanic_Status == "Non-Hispanic", ]
foreign_born_non_hispanic_table <- aggregate(PWGTP ~ Disability_Status + Age_Group, data = foreign_born_non_hispanic_data, FUN = sum)

# Combine the tables into a single data frame
combined_table <- merge(combined_table, us_born_non_hispanic_table, by = c("Disability_Status", "Age_Group"), all = TRUE)
combined_table <- merge(combined_table, foreign_born_non_hispanic_table, by = c("Disability_Status", "Age_Group"), all = TRUE)

# Rename the column "PWGTP.x" to "US-Born Non-Hispanic" and "PWGTP.y" to "Foreign-Born Non-Hispanic"
names(combined_table)[names(combined_table) == "PWGTP.x"] <- "US-Born Non-Hispanic"
names(combined_table)[names(combined_table) == "PWGTP.y"] <- "Foreign-Born Non-Hispanic"

# Add row names
rownames(combined_table) <- 1:nrow(combined_table)

# Print the updated combined table
print(combined_table)
print(combined_table)

write_xlsx(combined_table, "C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/disability_age_nativity_hispanic_2011.xlsx")
write_xlsx(combined_table, "C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/disability_age_nativity_hispanic_2022.xlsx")







# Create the ch1 data frame
ch1 <- pums2011[, c("PWGTP", "POBP", "ANC1P", "ANC2P")]

# Create a named vector of ancestry codes and descriptions
ancestry_codes <- c(
  "271" = "Cuba",
  "275" = "Dominica",
  "300" = "Bahamas",
  "301" = "Barbados",
  "308" = "Jamaica",
  "310" = "West Indies",
  "314" = "Trinidad and Tobago",
  "322" = "West Indies",
  "325" = "Antigua and Barbuda",
  "329" = "Grenada",
  "330" = "St. Vincent and the Grenadines",
  "331" = "St. Lucia",
  "335" = "West Indies",
  "336" = "Haiti",
  "359" = "Caribbean, Not Specified"
)

# Create a named vector of country codes
country_codes <- c(
  "321" = "Antigua and Barbuda",
  "323" = "Bahamas",
  "324" = "Barbados",
  "327" = "Cuba",
  "328" = "Dominica",
  "329" = "Dominican Republic",
  "330" = "Grenada",
  "332" = "Haiti",
  "333" = "Jamaica",
  "338" = "St. Kitts-Nevis",
  "339" = "St. Lucia",
  "340" = "St. Vincent and the Grenadines",
  "341" = "Trinidad and Tobago",
  "343" = "West Indies",
  "344" = "Caribbean, Not Specified"
)


# Combine the ancestry codes and country codes
combined_codes <- c(ancestry_codes, country_codes)

# Filter only the rows with ancestry codes (ANC1P, ANC2P) and country codes (POBP)
filtered_ch1 <- ch1[ch1$ANC1P %in% names(ancestry_codes) | ch1$ANC2P %in% names(ancestry_codes) | ch1$POBP %in% names(country_codes), ]

# Combine the unique values of ANC1P, ANC2P (ancestry names) and POBP (country names) after filtering
combined_names <- unique(c(
  combined_codes[filtered_ch1$ANC1P],
  combined_codes[filtered_ch1$ANC2P],
  combined_codes[filtered_ch1$POBP]
))

# Remove NA values from combined_names
combined_names <- combined_names[!is.na(combined_names)]

# Calculate the sum of PWGTP for each combined name
sum_pwgtp_by_name <- sapply(combined_names, function(name) {
  sum(filtered_ch1$PWGTP[combined_codes[filtered_ch1$ANC1P] == name | combined_codes[filtered_ch1$ANC2P] == name | combined_codes[filtered_ch1$POBP] == name], na.rm = TRUE)
})

# Create a data frame with the combined names and their corresponding sums
result_df <- data.frame(Name = combined_names, Sum_Population = sum_pwgtp_by_name)

# Remove rows with NA in the Name column
result_df <- result_df[complete.cases(result_df$Name), ]

# Remove rows with zero population
result_df <- result_df[result_df$Sum_Population != 0, ]

# Collapse the result by unique country names
collapsed_result <- aggregate(Sum_Population ~ Name, data = result_df, FUN = sum)

# Rank the collapsed result by sum of population in descending order
ranked_result <- collapsed_result[order(collapsed_result$Sum_Population, decreasing = TRUE), ]

# Add a rank variable
ranked_result$Rank <- seq_len(nrow(ranked_result))

# Print the ranked result
print(ranked_result)


write_xlsx(ranked_result, "C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/Texan_sum_by_Caribbean2.xlsx")












#Caribeans by states
pums2022_a<-read_sas("Y:/Data/Census/programs-surveys/acs/data/pums/2022/1-Year/sas/psam_pusa.sas7bdat")
pums2022_b<-read_sas("Y:/Data/Census/programs-surveys/acs/data/pums/2022/1-Year/sas/psam_pusb.sas7bdat")


#use ACC1P
# Create the ch1 data frame
ch1a <- pums2022_a[, c("PWGTP", "POBP", "WAOB", "ANC1P", "ANC2P","ST")]
ch1b <- pums2022_b[, c("PWGTP", "POBP", "WAOB", "ANC1P", "ANC2P","ST")]
ch1c <- rbind(ch1a, ch1b)

# Create a named vector of ancestry codes and descriptions
ancestry_codes <- c(
  "271" = "Cuban",
  "275" = "Dominican",
#  "290" = "Hispanic",
#  "291" = "Spanish",
#  "295" = "Spanish American",
  "300" = "Bahamian",
  "301" = "Barbadian",
#  "302" = "Belizean",
  "308" = "Jamaican",
  "310" = "Dutch West Indian",
  "314" = "Trinidadian Tobagonian",
  "322" = "British West Indian",
  "325" = "Antigua and Barbuda",
  "329" = "Grenadian",
  "330" = "Vincent-Grenadine Islander",
  "331" = "St Lucia Islander",
  "335" = "West Indian",
  "336" = "Haitian",
  "359" = "Other West Indian"
)

# Create a named vector of country codes
country_codes <- c(
  "321" = "Antigua and Barbuda",
  "323" = "Bahamas",
  "324" = "Barbados",
  "327" = "Cuba",
  "328" = "Dominica",
  "329" = "Dominican Republic",
  "330" = "Grenada",
  "332" = "Haiti",
  "333" = "Jamaica",
  "338" = "St. Kitts-Nevis",
  "339" = "St. Lucia",
  "340" = "St. Vincent and the Grenadines",
  "341" = "Trinidad and Tobago",
  "343" = "West Indies",
  "344" = "Caribbean, Not Specified"
)

# Print the ancestry codes with their descriptions
print(ancestry_codes)


# Calculate the sum of PWGTP by country code and state
carib_sum_by_state <- ch1c %>%
  filter(ANC1P %in% names(ancestry_codes) | ANC2P %in% names(ancestry_codes) | POBP %in% names(country_codes)) %>%
  group_by(ST, POBP) %>%
  summarize(Sum_Population = sum(PWGTP, na.rm = TRUE))

# Group the data by state and calculate the sum of population within each country code
carib_sum_by_state <- carib_sum_by_state %>%
  group_by(ST) %>%
  summarize(Sum_Population = sum(Sum_Population))

# Add a rank column based on the descending order of Sum_Population within each state
carib_sum_by_state <- carib_sum_by_state %>%
  arrange(ST, desc(Sum_Population)) %>%
  mutate(Rank = rank(desc(Sum_Population)))

# Sort the data frame by state
carib_sum_by_state <- carib_sum_by_state %>%
  arrange(ST)

# Create a named vector of state codes and names
state_codes <- c(
  "01" = "Alabama/AL",
  "02" = "Alaska/AK",
  "04" = "Arizona/AZ",
  "05" = "Arkansas/AR",
  "06" = "California/CA",
  "08" = "Colorado/CO",
  "09" = "Connecticut/CT",
  "10" = "Delaware/DE",
  "11" = "District of Columbia/DC",
  "12" = "Florida/FL",
  "13" = "Georgia/GA",
  "15" = "Hawaii/HI",
  "16" = "Idaho/ID",
  "17" = "Illinois/IL",
  "18" = "Indiana/IN",
  "19" = "Iowa/IA",
  "20" = "Kansas/KS",
  "21" = "Kentucky/KY",
  "22" = "Louisiana/LA",
  "23" = "Maine/ME",
  "24" = "Maryland/MD",
  "25" = "Massachusetts/MA",
  "26" = "Michigan/MI",
  "27" = "Minnesota/MN",
  "28" = "Mississippi/MS",
  "29" = "Missouri/MO",
  "30" = "Montana/MT",
  "31" = "Nebraska/NE",
  "32" = "Nevada/NV",
  "33" = "New Hampshire/NH",
  "34" = "New Jersey/NJ",
  "35" = "New Mexico/NM",
  "36" = "New York/NY",
  "37" = "North Carolina/NC",
  "38" = "North Dakota/ND",
  "39" = "Ohio/OH",
  "40" = "Oklahoma/OK",
  "41" = "Oregon/OR",
  "42" = "Pennsylvania/PA",
  "44" = "Rhode Island/RI",
  "45" = "South Carolina/SC",
  "46" = "South Dakota/SD",
  "47" = "Tennessee/TN",
  "48" = "Texas/TX",
  "49" = "Utah/UT",
  "50" = "Vermont/VT",
  "51" = "Virginia/VA",
  "53" = "Washington/WA",
  "54" = "West Virginia/WV",
  "55" = "Wisconsin/WI",
  "56" = "Wyoming/WY",
  "72" = "Puerto Rico/PR"
)

# Add the state names to the carib_sum_by_state data frame
carib_sum_by_state <- carib_sum_by_state %>%
  mutate(Name = state_codes[ST])

# Print the updated data frame
print(carib_sum_by_state)


write_xlsx(carib_sum_by_state, "C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/carib_ancestry_sum_by_state.xlsx")
# Create the ch1 data frame
ch1a <- pums2022_a[, c("PWGTP", "POBP", "WAOB", "YOEP", "ST")]
ch1b <- pums2022_b[, c("PWGTP", "POBP", "WAOB", "YOEP", "ST")]
ch1c <- rbind(ch1a, ch1b)

# Create a named vector of country codes
country_codes <- c(
  "322" = "Antigua and Barbuda",
  "323" = "Bahamas",
  "324" = "Barbados",
  "327" = "Cuba",
  "328" = "Dominica",
  "329" = "Dominican Republic",
  "330" = "Grenada",
  "332" = "Haiti",
  "333" = "Jamaica",
  "338" = "St. Kitts-Nevis",
  "339" = "St. Lucia",
  "340" = "St. Vincent and the Grenadines",
  "341" = "Trinidad and Tobago",
  "343" = "West Indies",
  "344" = "Caribbean, Not Specified"
)


# Calculate the sum of PWGTP by country code and state
carib_sum_by_state <- ch1c %>%
  filter(POBP %in% names(country_codes)) %>%
  group_by(ST, POBP) %>%
  summarize(Sum_Population = sum(PWGTP, na.rm = TRUE))

# Group the data by state and calculate the sum of population within each country code
carib_sum_by_state <- carib_sum_by_state %>%
  group_by(ST) %>%
  summarize(Sum_Population = sum(Sum_Population))

# Add a rank column based on the descending order of Sum_Population within each state
carib_sum_by_state <- carib_sum_by_state %>%
  arrange(ST, desc(Sum_Population)) %>%
  mutate(Rank = rank(desc(Sum_Population)))

# Sort the data frame by state
carib_sum_by_state <- carib_sum_by_state %>%
  arrange(ST)

# Create a named vector of state codes and names
state_codes <- c(
  "01" = "Alabama/AL",
  "02" = "Alaska/AK",
  "04" = "Arizona/AZ",
  "05" = "Arkansas/AR",
  "06" = "California/CA",
  "08" = "Colorado/CO",
  "09" = "Connecticut/CT",
  "10" = "Delaware/DE",
  "11" = "District of Columbia/DC",
  "12" = "Florida/FL",
  "13" = "Georgia/GA",
  "15" = "Hawaii/HI",
  "16" = "Idaho/ID",
  "17" = "Illinois/IL",
  "18" = "Indiana/IN",
  "19" = "Iowa/IA",
  "20" = "Kansas/KS",
  "21" = "Kentucky/KY",
  "22" = "Louisiana/LA",
  "23" = "Maine/ME",
  "24" = "Maryland/MD",
  "25" = "Massachusetts/MA",
  "26" = "Michigan/MI",
  "27" = "Minnesota/MN",
  "28" = "Mississippi/MS",
  "29" = "Missouri/MO",
  "30" = "Montana/MT",
  "31" = "Nebraska/NE",
  "32" = "Nevada/NV",
  "33" = "New Hampshire/NH",
  "34" = "New Jersey/NJ",
  "35" = "New Mexico/NM",
  "36" = "New York/NY",
  "37" = "North Carolina/NC",
  "38" = "North Dakota/ND",
  "39" = "Ohio/OH",
  "40" = "Oklahoma/OK",
  "41" = "Oregon/OR",
  "42" = "Pennsylvania/PA",
  "44" = "Rhode Island/RI",
  "45" = "South Carolina/SC",
  "46" = "South Dakota/SD",
  "47" = "Tennessee/TN",
  "48" = "Texas/TX",
  "49" = "Utah/UT",
  "50" = "Vermont/VT",
  "51" = "Virginia/VA",
  "53" = "Washington/WA",
  "54" = "West Virginia/WV",
  "55" = "Wisconsin/WI",
  "56" = "Wyoming/WY",
  "72" = "Puerto Rico/PR"
)

# Add the state names to the carib_sum_by_state data frame
carib_sum_by_state <- carib_sum_by_state %>%
  mutate(Name = state_codes[ST])

# Print the updated data frame
print(carib_sum_by_state)


write_xlsx(carib_sum_by_state, "C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/carib_sum_by_state.xlsx")

pums2022<-read_sas("Y:/Data/Census/programs-surveys/acs/data/pums/2022/1-Year/sas/psam_p48.sas7bdat")

# Create the ch1 data frame
ch1 <- pums2022[, c("PWGTP", "POBP", "WAOB", "ANC1P", "ANC2P","ST")]

# Create a named vector of country codes
country_codes <- c(
  "321" = "Antigua and Barbuda",
  "323" = "Bahamas",
  "324" = "Barbados",
  "327" = "Cuba",
  "328" = "Dominica",
  "329" = "Dominican Republic",
  "330" = "Grenada",
  "332" = "Haiti",
  "333" = "Jamaica",
  "338" = "St. Kitts-Nevis",
  "339" = "St. Lucia",
  "340" = "St. Vincent and the Grenadines",
  "341" = "Trinidad and Tobago",
  "343" = "West Indies",
  "344" = "Caribbean, Not Specified"
)

# Calculate the sum by country code
tb2 <- aggregate(ch1$PWGTP, by = list(countrycode = ch1$POBP), FUN = sum, na.rm = TRUE)

# Filter the data frame to include only countries in country_codes
tb2 <- tb2[tb2$countrycode %in% names(country_codes), ]

# Rename the country code column
tb2$countrycode <- country_codes[tb2$countrycode]

# Rename the aggregated PWGTP column
colnames(tb2)[2] <- "Sum_PWGTP"

# Sort the data frame in descending order of Sum_PWGTP
tb2 <- tb2[order(-tb2$Sum_PWGTP), ]

# Add a new column for the ranking
tb2$Rank <- seq_len(nrow(tb2))

# Reorder the columns to have the Rank column first
tb2 <- tb2[, c("Rank", "countrycode", "Sum_PWGTP")]

# Create a data frame
df <- as.data.frame(tb2)

# Calculate the sum of PWGTP for country codes
sum_country_codes <- sum(df$Sum_PWGTP)

# Calculate the sum of PWGTP for all countries
sum_all_countries <- sum(ch1$PWGTP, na.rm = TRUE)

# Create a new row with the sums
sum_row <- c("-", "Total Country Codes", sum_country_codes)
df <- rbind(data.frame(Rank = NA, countrycode = "-", Sum_PWGTP = sum_all_countries), sum_row, df)

# Update the ranking column
df$Rank <- seq_len(nrow(df))

# Print the data frame
print(df)

# Calculate the sum by country code
tb2 <- aggregate(ch1$PWGTP, by = list(countrycode = ch1$POBP), FUN = sum, na.rm = TRUE)

# Filter the data frame to include only countries in country_codes
tb2 <- tb2[tb2$countrycode %in% names(country_codes), ]

# Rename the country code column
tb2$countrycode <- country_codes[tb2$countrycode]

# Rename the aggregated PWGTP column
colnames(tb2)[2] <- "Sum_PWGTP"

# Sort the data frame in descending order of Sum_PWGTP
tb2 <- tb2[order(-tb2$Sum_PWGTP), ]

# Add a new column for the ranking
tb2$Rank <- seq_len(nrow(tb2))

# Reorder the columns to have the Rank column first
tb2 <- tb2[, c("Rank", "countrycode", "Sum_PWGTP")]

# Create a data frame
df <- as.data.frame(tb2)

# Calculate the sum of PWGTP for country codes
sum_country_codes <- sum(df$Sum_PWGTP)

# Calculate the sum of PWGTP for all countries
sum_all_countries <- sum(ch1$PWGTP, na.rm = TRUE)

# Create a new row with the sums
sum_row <- c("-", "Total Country Codes", sum_country_codes)
df <- rbind(data.frame(Rank = NA, countrycode = "-", Sum_PWGTP = sum_all_countries), sum_row, df)

# Print the data frame
print(df)

write_xlsx(df, "C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/Caribbean2011.xlsx")


#father's day base data for comparison between TX and US
pums2022_5y<-read_sas("Y:/Data/Census/programs-surveys/acs/data/pums/2022/5-Year/sas/psam_p48.sas7bdat")
pums2022_5y_US<-read_sas("Y:/Data/Census/programs-surveys/acs/data/pums/2022/5-Year/sas/psam_pusa.sas7bdat")

tb1<-pums2022_5y[, c("ESP", "PWGTP")]
tb11<-pums2022_5y_US[, c("ESP", "PWGTP")]
# Recode empty strings as "not available"
tb1$ESP <- ifelse(tb1$ESP == "", "not own child of householder", tb1$ESP)
tb11$ESP <- ifelse(tb11$ESP == "", "not own child of householder", tb11$ESP)
# Print the updated data frame
print(tb11)

# Define the recoding rules
recode_rules <- c("1" = "Living with two parents: Both parents in labor force",
                  "2" = "Living with two parents: Father only in labor force",
                  "3" = "Living with two parents: Mother only in labor force",
                  "4" = "Living with two parents: Neither parent in labor force",
                  "5" = "Living with father: In labor force",
                  "6" = "Living with father: Not in labor force",
                  "7" = "Living with mother: In labor force",
                  "8" = "Living with mother: Not in labor force")

# Recode the values
tb1$ESP <- recode(tb1$ESP, !!!recode_rules)
tb11$ESP <- recode(tb11$ESP, !!!recode_rules)
# Print the updated data frame
print(tb1)

# Calculate the sum by ESP from tb1
tb2 <- aggregate(PWGTP ~ ESP, data = tb1, sum)
tb21 <- aggregate(PWGTP ~ ESP, data = tb11, sum)
# Print the resulting table
print(tb21)

write_xlsx(tb2, "C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/table_tx_father.xlsx")
write_xlsx(tb21, "C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/table_us_father.xlsx")



#read sasdat from our local repository

pums2022_1y<-read_sas("Y:/Data/Census/programs-surveys/acs/data/pums/2022/1-Year/sas/psam_p48.sas7bdat")

ch1<-pums2022_1y[, c("AGEP", "PWGTP", "NATIVITY", "POBP", "WAOB","RAC1P", "RACBLK")]
table(ch1$RACBLK)
table(ch1$NATIVITY, ch1$WAOB)

#Blanck /African American in Texas
# Create a named vector of race categories
race_categories <- c(
  "1" = "White alone",
  "2" = "Black or African American alone",
  "3" = "American Indian alone",
  "4" = "Alaska Native alone",
  "5" = "American Indian and Alaska Native tribes specified; or American Indian or Alaska Native, not specified and no other races",
  "6" = "Asian alone",
  "7" = "Native Hawaiian and Other Pacific Islander alone",
  "8" = "Some Other Race alone",
  "9" = "Two or More Races")

table_race <- ch1 %>%
  group_by(RAC1P) %>%
  summarize(Sum_PWGTP = sum(PWGTP)) %>%
  mutate(Race_Category  = race_categories[as.character(RAC1P)])

write_xlsx(table_race, "C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/race_comp_tx.xlsx")

print(table_race)
# Filter ch1 by RACBLK
ch11 <- ch1 %>%
  filter(RACBLK == 1)

# Filter ch11 by NATIVITY and WAOB
ch12 <- ch11 %>%
  filter(NATIVITY == 1 & WAOB %in% c(1, 2))

state_names <- c(
  "001" = "Alabama/AL",
  "002" = "Alaska/AK",
  "004" = "Arizona/AZ",
  "005" = "Arkansas/AR",
  "006" = "California/CA",
  "008" = "Colorado/CO",
  "009" = "Connecticut/CT",
  "010" = "Delaware/DE",
  "011" = "District of Columbia/DC",
  "012" = "Florida/FL",
  "013" = "Georgia/GA",
  "015" = "Hawaii/HI",
  "016" = "Idaho/ID",
  "017" = "Illinois/IL",
  "018" = "Indiana/IN",
  "019" = "Iowa/IA",
  "020" = "Kansas/KS",
  "022" = "Kentucky/KY",
  "022" = "Louisiana/LA",
  "023" = "Maine/ME",
  "024" = "Maryland/MD",
  "025" = "Massachusetts/MA",
  "026" = "Michigan/MI",
  "027" = "Minnesota/MN",
  "028" = "Mississippi/MS",
  "029" = "Missouri/MO",
  "030" = "Montana/MT",
  "031" = "Nebraska/NE",
  "032" = "Nevada/NV",
  "033" = "New Hampshire/NH",
  "034" = "New Jersey/NJ",
  "035" = "New Mexico/NM",
  "036" = "New York/NY",
  "037" = "North Carolina/NC",
  "038" = "North Dakota/ND",
  "039" = "Ohio/OH",
  "040" = "Oklahoma/OK",
  "041" = "Oregon/OR",
  "042" = "Pennsylvania/PA",
  "044" = "Rhode Island/RI",
  "045" = "South Carolina/SC",
  "046" = "South Dakota/SD",
  "047" = "Tennessee/TN",
  "048" = "Texas/TX",
  "049" = "Utah/UT",
  "050" = "Vermont/VT",
  "051" = "Virginia/VA",
  "053" = "Washington/WA",
  "054" = "West Virginia/WV",
  "055" = "Wisconsin/WI",
  "056" = "Wyoming/WY",
  "060" = "American Samoa",
  "066" = "Guam",
  "069" = "Commonwealth of the Northern Mariana Islands",
  "072" = "Puerto Rico",
  "078" = "US Virgin Islands"
)

# Create a summary table with sum of PWGTP by POBP
table_african <- ch12 %>%
  group_by(POBP) %>%
  summarize(Sum_PWGTP = sum(PWGTP)) %>%
  mutate(State_Name = state_names[as.character(POBP)])

# Print the resulting table
print(table_african)


# Save table_african as an Excel file
write_xlsx(table_african, "C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/USborn_african.xlsx")

# foreign born composition
# Filter ch1 by RACBLK
ch11 <- ch1 %>%
  filter(RACBLK == 1)

# Filter ch11 by NATIVITY and WAOB
ch13 <- ch11 %>%
  filter(NATIVITY == 2 & WAOB %in% c(6))
# Create a named vector of African country codes
african_country_codes <- c(
  "400" = "Algeria",
  "407" = "Cameroon",
  "408" = "Cabo Verde",
  "412" = "Congo",
  "414" = "Egypt",
  "416" = "Ethiopia",
  "417" = "Eritrea",
  "420" = "Gambia",
  "421" = "Ghana",
  "423" = "Guinea",
  "425" = "Ivory Coast",
  "427" = "Kenya",
  "429" = "Liberia",
  "430" = "Libya",
  "436" = "Morocco",
  "440" = "Nigeria",
  "442" = "Rwanda",
  "444" = "Senegal",
  "447" = "Sierra Leone",
  "448" = "Somalia",
  "449" = "South Africa",
  "451" = "Sudan",
  "453" = "Tanzania",
  "454" = "Togo",
  "456" = "Tunisia",
  "457" = "Uganda",
  "459" = "Democratic Republic of Congo (Zaire)",
  "460" = "Zambia",
  "461" = "Zimbabwe",
  "462" = "Africa",
  "463" = "South Sudan",
  "464" = "Northern Africa, Not Specified",
  "467" = "Western Africa, Not Specified",
  "468" = "Other Africa, Not Specified",
  "469" = "Eastern Africa, Not Specified"
)

foreignborn_african <- ch13 %>%
  group_by(POBP) %>%
  summarize(Sum_PWGTP = sum(PWGTP)) %>%
  mutate(African_Country = african_country_codes[as.character(POBP)])

print(foreignborn_african)
write_xlsx(foreignborn_african, "C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/foreignborn_african.xlsx")

#Age structure by POBP
ch2<-  ch1 %>%
  group_by(AGEP) %>%
  summarise(pop = sum(PWGTP)) %>%
  arrange(AGEP, .locale = "en")

ggplot(ch2, aes(x=AGEP, y=pop)) +
  geom_line()

#nested by nativity and place of birth
ch1<-pums2022_1y[, c("AGEP", "PWGTP", "NATIVITY", "POBP", "WAOB")]
ch1$POB_ch<-as.factor(str_sub(ch1$POBP, 1,1))
ch1$AGEP<-as.factor(ch1$AGEP)
ch3<- ch1 %>%
  nest_by(NATIVITY,POB_ch)

ch5<- ch1 %>%
  nest_by(WAOB)


#ch3$data<-as.data.frame(ch3$data)

ch41<-ch3 %>%
filter(NATIVITY==1)
ch41$NATIVITY<-NULL
ch42<-ch3 %>%
filter(NATIVITY==2)
ch42$NATIVITY<-NULL

#plots for frequency by 
ch41_plots <- 
  ch41 %>% 
  drop_na()%>%
  mutate(plot = map2(
    data, POB_ch,  
    ~ ggplot(data = .x, aes(x = AGEP, y = PWGTP)) +
      ggtitle(glue("population: {.y}")) +
      geom_line()))

#convert it sum of PWGTP
#Native
ch41_plots <- ch41 %>% 
  drop_na() %>% 
  mutate(plot = map2(
    data, POB_ch,  
    ~ ggplot(data = .x, aes(x = AGEP, y = PWGTP)) +
      ggtitle(glue("population: {.y}")) +
      stat_summary(geom = "line", fun = sum, aes(group = 1, linetype = "Sum")))) 

(ch41_plots$plot[[1]] + ch41_plots$plot[[2]] + ch41_plots$plot[[3]] + 
   ch41_plots$plot[[4]] + ch41_plots$plot[[5]] + ch41_plots$plot[[6]])  + plot_layout(ncol = 3)

#Foreign born
ch42_plots <- ch42 %>% 
  drop_na() %>% 
  mutate(plot = map2(
    data, POB_ch,  
    ~ ggplot(data = .x, aes(x = AGEP, y = PWGTP)) +
      ggtitle(glue("population: {.y}")) +
      stat_summary(geom = "line", fun = sum, aes(group = 1, linetype = "Sum")))) 

(ch42_plots$plot[[1]] + ch42_plots$plot[[2]] + ch42_plots$plot[[3]] + 
   ch42_plots$plot[[4]] + ch42_plots$plot[[5]] + ch42_plots$plot[[6]])  + plot_layout(ncol = 3)

#Place of birth 0 means native 1
ch5_plots <- ch5 %>% 
  drop_na() %>% 
  mutate(plot = map2(
    data, WAOB,  
    ~ ggplot(data = .x, aes(x = AGEP, y = PWGTP)) +
      ggtitle(glue("population: {.y}")) +
      stat_summary(geom = "line", fun = sum, aes(group = 1, linetype = "Sum")) +
      theme(legend.position = c(0.85, 0.85))  # move legend to upper right corner
  ))

plot_grid(
  (ch5_plots$plot[[1]] + ggtitle("Texans born in the US") + xlab("AGE") + ylab("Population")) + ylim(0, NA), 
  (ch5_plots$plot[[4]] + ggtitle("Texans born in Asia") + xlab("AGE") + ylab("Population")) + ylim(0, NA), 
  (ch5_plots$plot[[8]] + ggtitle("Texans born in Pacific Islands") + xlab("AGE") + ylab("Population")) + ylim(0, NA), 
  align = "v", axis = "tb", nrow = 3
)

#intergrate all six plots by differtent colors
library(patchwork)

#comparison born in US vs Asia
library(cowplot)

plot_grid((ch5_plots$plot[[1]] +  ggtitle("Native") + xlab("AGE") + ylab("Population")), 
          (ch5_plots$plot[[4]] +  ggtitle("Asia")  +xlab("AGE") + ylab("Population")), ncol = 2)

#read 2010 pums data
pums2010_1y<-read_sas("Y:/Data/Census/programs-surveys/acs/data/pums/2010/1-Year/sas/psam_p48.sas7bdat")
#
ch6_2010<-pums2010_1y[, c("AGEP", "PWGTP", "NATIVITY", "POBP", "WAOB", "YOEP")]
ch6_2022<-pums2022_1y[, c("AGEP", "PWGTP", "NATIVITY", "POBP", "WAOB", "YOEP")]


ch6_2010n<- ch6_2010 %>%
  nest_by(WAOB)

ch6_2010_plots <- ch6_2010n %>% 
  drop_na() %>% 
  mutate(plot = map2(
    data, WAOB,  
    ~ ggplot(data = .x, ae
    
    s(x = YOEP, y = PWGTP)) +
      ggtitle(glue("population: {.y}")) +
      stat_summary(geom = "line", fun = sum, aes(group = 1, linetype = "Sum")) +
      theme(legend.position = c(0.15, 0.85))  # move legend to upper right corner
  ))

plot_grid(
  (ch6_2010_plots$plot[[4]] + ggtitle("Year of entry born in Asia") + xlab("Year of entry") + ylab("Population")) + ylim(0, NA), 
  (ch6_2010_plots$plot[[8]] + ggtitle("Year of entry born in Pacific Islands") + xlab("Year of entry") + ylab("Population")) + ylim(0, NA), 
  align = "v", axis = "tb", nrow = 2
)

ch6_2022n<- ch6_2022 %>%
  nest_by(WAOB)

ch6_2022_plots <- ch6_2022n %>% 
  drop_na() %>% 
  mutate(plot = map2(
    data, WAOB,  
    ~ ggplot(data = .x, aes(x = YOEP, y = PWGTP)) +
      ggtitle(glue("population: {.y}")) +
      stat_summary(geom = "line", fun = sum, aes(group = 1, linetype = "Sum")) +
      theme(legend.position = c(0.15, 0.85))  # move legend to upper right corner
  ))

plot_grid(
  (ch6_2022_plots$plot[[4]] + ggtitle("Year of entry born in Asia") + xlab("Year of entry") + ylab("Population")) + ylim(0, NA), 
  (ch6_2022_plots$plot[[8]] + ggtitle("Year of entry born in Pacific Islands") + xlab("Year of entry") + ylab("Population")) + ylim(0, NA), 
  align = "v", axis = "tb", nrow = 2
)

#pums 2010 vs 2022 for asian born
plot_grid(
  (ch6_2010_plots$plot[[4]] + ggtitle("Year of entry born in Asia in 2010") + xlab("Year of entry") + ylab("Population")) + ylim(0, NA), 
  (ch6_2022_plots$plot[[4]] + ggtitle("Year of entry born in Asia in 2022") + xlab("Year of entry") + ylab("Population")) + ylim(0, NA), 
  align = "v", axis = "tb", nrow = 2
)
#pums 2010 vs 2022 for born in pacific islanders
plot_grid(
  (ch6_2010_plots$plot[[8]] + ggtitle("Year of entry born in Pacific Islands in 2010") + xlab("Year of entry") + ylab("Population")) + ylim(0, NA), 
  (ch6_2022_plots$plot[[8]] + ggtitle("Year of entry born in Pacific Islands in 2022") + xlab("Year of entry") + ylab("Population")) + ylim(0, NA), 
  align = "v", axis = "tb", nrow = 2
)


#create excel table from acs 5 year
library(writexl)
library(dplyr)
pums2022_5y<-read_sas("Y:/Data/Census/programs-surveys/acs/data/pums/2022/5-Year/sas/psam_p48.sas7bdat")


ch7<-pums2022_5y[, c("SCHL","PWGTP", "NATIVITY", "WAOB","ADJINC")]
ch7$SCHL<-as.numeric(ch7$SCHL)
ch7 <- ch7 %>% 
  mutate(edu = recode(SCHL, recodes="21:24 = 'Bachelor or higher'; else = 'HighSchool or lower'"))
table(ch7$edu)


ch7_nest<- ch7 %>%
  nest_by(WAOB)

ch7_prop_plots <- ch7_nest %>%
  drop_na() %>% 
  mutate(plot = map2(
    data, WAOB,  
    ~ ggplot(data = .x, aes(x = YOEP, y = PWGTP)) +
      ggtitle(glue("population: {.y}")) +
      stat_summary(geom = "line", fun = sum, aes(group = 1, linetype = "Sum")) +
      theme(legend.position = c(0.15, 0.85))  # move legend to upper right corner
  ))





ch7_prop_plots <- ch7_nest %>%
  drop_na() %>% 
  mutate(plot = map2(
    data, WAOB,  
    ~ ggplot(data = .x, aes(x = edu, y = prop.table(PWGTP), fill = edu)) +
      ggtitle(glue("population: {.y}")) +
      geom_bar(stat = "identity") +
      ylab("Proportion") +
      scale_fill_manual(values = c("HighSchool or lower" = "orange", "Bachelor or higher" = "blue")) +
      theme(legend.position = "bottom")
  ))


  plot_grid(
    (ch7_prop_plots$plot[[4]] + ggtitle("Asian educational attainment") + xlab("Aian") + ylab("Proportion")) + ylim(0, NA), 
    (ch7_prop_plots$plot[[8]] + ggtitle("Pacific Islander educational attainment") + xlab("Pacific Islander") + ylab("Proportion")) + ylim(0, NA), 
    align = "v", axis = "tb", nrow = 2
  )

ch7_prop_plots <- ch7_nest %>%
  drop_na() %>% 
  mutate(plot = map2(
    data, WAOB,  
    ~ ggplot(data = .x, aes(x = "", y = prop.table(PWGTP), fill = edu)) +
      ggtitle(glue("population: {.y}")) +
      geom_col(width = 1) +
      ylab("Proportion") +
      scale_fill_manual(values = c("HighSchool or lower" = "orange", "Bachelor or higher" = "blue")) +
      coord_polar("y", start = 0) +
      theme_void() +
      theme(legend.position = "bottom")
  ))

library(ggplot2)
library(gridExtra)

ch7_prop_plots <- ch7_nest %>%
  drop_na() %>% 
  mutate(plot = map2(
    data, WAOB,  
    ~ ggplot(data = .x, aes(x = edu, y = prop.table(PWGTP), fill = edu)) +
      ggtitle(glue("population: {.y}")) +
      geom_bar(stat = "identity") +
      ylab("Proportion") +
      scale_fill_manual(values = c("HighSchool or lower" = "orange", "Bachelor or higher" = "blue")) +
      theme(legend.position = "bottom")
  ))

grid.arrange(grobs = ch7_prop_plots$plot, ncol = 2)

ch7_prop_plots <- ch7_nest %>%
  drop_na() %>% 
  mutate(plot = map2(
    data, WAOB,  
    ~ ggplot(data = .x, aes(x = "", y = prop.table(PWGTP), fill = edu)) +
      ggtitle(glue("population: {.y}")) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      ylab("") +
      scale_fill_manual(values = c("HighSchool or lower" = "orange", "Bachelor or higher" = "blue")) +
      theme_void() +
      theme(legend.position = "bottom")
  ))

# show the plots
print(ch7_prop_plots$plot[[1]])
print(ch7_prop_plots$plot[[2]])
# add more print statements for additional plots as needed



ch7_prop_plots <- ch7_nest %>%
  drop_na() %>% 
  mutate(plot = map2(
    data, WAOB,  
    ~ ggplot(data = .x, aes(x = edu, y = prop.table(PWGTP), fill = edu)) +
      ggtitle(glue("population: {.y}")) +
      geom_bar(stat = "identity") +
      ylab("Proportion") +
      scale_fill_manual(values = c("HighSchool or lower" = "orange", "Bachelor or higher" = "blue")) +
      theme(legend.position = "bottom")
  ))

ch7 <- pums2022_5y[, c("SCHL", "PWGTP", "NATIVITY", "WAOB","POBP","ADJINC")]
ch7$SCHL <- as.numeric(ch7$SCHL)

ch7 <- ch7 %>%
  mutate(edu = recode(SCHL, recodes = "21:24 = 'Bachelor or higher'; else = 'HighSchool or lower'"))

ch7_prop <- ch7 %>%
  group_by(WAOB) %>%
  summarize(prop_bachelor_higher = sum(PWGTP[edu == "Bachelor or higher"]) / sum(PWGTP))

ggplot(ch7_prop, aes(x = WAOB, y = prop_bachelor_higher, fill = "Bachelor or Higher")) +
  geom_bar(stat = "identity") +
  ggtitle("Proportion of Bachelor or higher by World ared of birth in 2022 Texas") +
  ylab("Proportion") +
  xlab("World ared of birth") +
  scale_x_discrete(labels = c("US", "Puerto Rico", "Latin America", 
                              "Asia", "Europe","Africa", 
                              "North America", "Pacific Islander")) +
  scale_fill_manual(values = c("gray", "gray", "gray", "red", "gray", "gray", "gray", "blue")) +
  theme(legend.position = "bottom", 
        axis.title.x = element_text(size = 16, face = "bold"), 
        axis.title.y = element_text(size = 16, face = "bold"), 
        axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 14), 
        plot.title = element_text(size = 20, face = "bold"))


#by WAOB
ch7_prop <- ch7 %>%
  group_by(WAOB) %>%
  summarize(prop_bachelor_higher = sum(PWGTP[edu == "Bachelor or higher"]) / sum(PWGTP))

ggplot(ch7_prop, aes(x = WAOB, y = prop_bachelor_higher, fill = "Bachelor or Higher")) +
  geom_bar(stat = "identity") +
  ggtitle("Proportion of Bachelor or higher by World area of birth in 2022 Texas") +
  ylab("Proportion") +
  xlab("World area of birth") +
  scale_x_discrete(labels = c("US", "Puerto Rico", "Latin America", 
                              "Asia", "Europe","Africa", 
                              "North America", "Pacific Islander")) +
  scale_fill_manual(values = c("gray", "gray", "gray", "red", "gray", "gray", "gray", "blue")) +
theme(legend.position = c(0.2, 0.9), 
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.key.width = unit(2, "cm"),
        axis.title.x = element_text(size = 18, face = "bold"), 
        axis.title.y = element_text(size = 18, face = "bold"), 
        axis.text.x = element_text(size = 12.5), 
        axis.text.y = element_text(size = 15), 
        plot.title = element_text(size = 20, face = "bold")) +
  guides(fill = guide_legend(override.aes = list(fill = "gray")))


ggsave("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/edu_world1.jpg", plot = last_plot(), width = 10, height = 10, dpi = 300)

# Load required packages
library(dplyr)
library(rlang)
library(purrr)

# Subset the data frame
ch7 <- pums2022_5y[, c("SCHL", "PWGTP", "NATIVITY", "WAOB", "POBP", "ADJINC", "RAC1P", "PERNP", "AGEP", "PINCP","FESRP", "RAC1P", "FHISP")]
ch7$SCHL <- as.numeric(ch7$SCHL)
ch7$factor <- as.numeric(ch7$ADJINC)/1000000
ch7$adj_ern <- ch7$factor*ch7$PERNP
ch7$adj_inc <- ch7$factor*ch7$PINCP
ch7 <- ch7 %>%
  subset(AGEP >= 16)
# Recode the education level
# ch7 <- ch7 %>%
#   mutate(edu = Recode(SCHL, recodes = "21:24 = 'Bachelor or higher'; else = 'HighSchool or lower'"))
# Recode the education level
ch7$edu <- Recode(ch7$SCHL, recodes = "21:24 = 'Bachelor or higher'; else = 'HighSchool or lower'")
# Subset the data frames for each group
ch7_aapi <- ch7[ch7$NATIVITY == '2' & ch7$RAC1P %in% c('6', '7'),]  # AAPI foreign born
ch7_others <- ch7[ch7$NATIVITY == '2' & !ch7$RAC1P %in% c('6', '7'),]  # Other race foreign born
ch7_usborn <- ch7[ch7$NATIVITY == '1',]  # US born

# Calculate educational attainment proportions for each group
edu_prop_aapi <- sum(ch7_aapi$PWGTP[ch7_aapi$edu == "Bachelor or higher"]) / sum(ch7_aapi$PWGTP)
edu_prop_others <- sum(ch7_others$PWGTP[ch7_others$edu == "Bachelor or higher"]) / sum(ch7_others$PWGTP)
edu_prop_usborn <- sum(ch7_usborn$PWGTP[ch7_usborn$edu == "Bachelor or higher"]) / sum(ch7_usborn$PWGTP)

# Calculate average earning for each group
avg_earning_aapi <- weighted.mean(ch7_aapi$adj_ern, ch7_aapi$PWGTP)
avg_earning_others <- weighted.mean(ch7_others$adj_ern, ch7_others$PWGTP)
avg_earning_usborn <- weighted.mean(ch7_usborn$adj_ern, ch7_usborn$PWGTP)

# Calculate average income for each group amonng employed 
ch71 <- ch7 %>%
  subset(FESRP == 1)
# Subset the data frames for each group
ch71_aapi <- ch71[ch71$NATIVITY == '2' & ch71$RAC1P %in% c('6', '7'),]  # AAPI foreign born
ch71_others <- ch71[ch71$NATIVITY == '2' & !ch71$RAC1P %in% c('6', '7'),]  # Other race foreign born
ch71_usborn <- ch71[ch71$NATIVITY == '1',]  # US born

edu_prop_aapi2 <- sum(ch71_aapi$PWGTP[ch71_aapi$edu == "Bachelor or higher"]) / sum(ch71_aapi$PWGTP)
edu_prop_others2 <- sum(ch71_others$PWGTP[ch71_others$edu == "Bachelor or higher"]) / sum(ch71_others$PWGTP)
edu_prop_usborn2 <- sum(ch71_usborn$PWGTP[ch71_usborn$edu == "Bachelor or higher"]) / sum(ch71_usborn$PWGTP)

# Calculate average income for each group
avg_income_aapi2 <- weighted.mean(ch71_aapi$adj_inc, ch71_aapi$PWGTP)
avg_income_others2 <- weighted.mean(ch71_others$adj_inc, ch71_others$PWGTP)
avg_income_usborn2 <- weighted.mean(ch71_usborn$adj_inc, ch71_usborn$PWGTP)


# Create data table
library(data.table)
data_table1 <- data.table(
  Group = c("Foreign-born AAPI", "Foreign-born Others", "US-born"),
  EducationalAttainment = c(edu_prop_aapi, edu_prop_others, edu_prop_usborn),
  AverageEarning = c(avg_earning_aapi, avg_earning_others, avg_earning_usborn)
)

data_table2 <- data.table(
  Group = c("Foreign-born AAPI", "Foreign-born Others", "US-born"),
  EducationalAttainment = c(edu_prop_aapi2, edu_prop_others2, edu_prop_usborn2),
  AverageIncome = c(avg_income_aapi2, avg_income_others2, avg_income_usborn2)
)

# Print the data table
print(data_table1)
print(data_table2)

library(writexl)

# Create the Excel file
library(openxlsx)

# Create the Excel file
wb <- createWorkbook()
addWorksheet(wb, "Edu-Income")
addWorksheet(wb, "Edu-Earning")

# Write data_table1 to "Data Table 1" sheet
writeData(wb, "Edu-Income", data_table1)

# Write data_table2 to "Data Table 2" sheet
writeData(wb, "Edu-Earning", data_table2)

# Save the Excel file
saveWorkbook(wb, "Y:/ProductDev/2024/Social Media Posts/VizOfTheWeek/05_May/AAPI_edu_income.xlsx")


# AAPI vs NH White

ch7 <- pums2022_5y[, c("SCHL", "PWGTP", "NATIVITY", "WAOB", "POBP", "ADJINC", "RAC1P", "PERNP", "AGEP", "PINCP","FESRP", "RAC1P", "FHISP")]
ch7$SCHL <- as.numeric(ch7$SCHL)
ch7$factor <- as.numeric(ch7$ADJINC)/1000000
ch7$adj_ern <- ch7$factor*ch7$PERNP
ch7$adj_inc <- ch7$factor*ch7$PINCP

# Check for duplicate column names
duplicates <- duplicated(names(ch7))

if (any(duplicates)) {
  names(ch7) <- make.unique(names(ch7), sep = "_")
}

ch7 <- ch7 %>%
  filter(AGEP >= 25)

# Recode the education level
# ch7 <- ch7 %>%
#   mutate(edu = Recode(SCHL, recodes = "1:15='Less than high school'; 16:17='High school'; 18:20= 'Some college' ; 21 = 'Bachelor degree'; 22:24= 'Graduate or Professional'"))
ch7$edu <- Recode(ch7$SCHL, recodes = "1:15='Less than high school'; 16:17='High school'; 18:20= 'Some college' ; 21 = 'Bachelor degree'; 22:24= 'Graduate or Professional'")

table(ch7$edu)
# Subset the data frames for each group
ch7_aapi <- ch7[ch7$RAC1P %in% c('6', '7'),]  # AAPI 
ch7_NHwhite <- ch7[ch7$FHISP == '0' & !ch7$RAC1P ==1,]  # Non_hispanic white

library(data.table)

# Subset the data frames for each group
ch7_aapi <- ch7[ch7$RAC1P %in% c('6', '7'),]  # AAPI 
ch7_NHwhite <- ch7[ch7$FHISP == '0' & !ch7$RAC1P == 1,]  # Non-Hispanic White

# Calculate proportions of educational attainment for AAPI
edu_prop_aapi1 <- sum(ch7_aapi$PWGTP[ch7_aapi$edu == "Less than high school"]) / sum(ch7_aapi$PWGTP)
edu_prop_aapi2 <- sum(ch7_aapi$PWGTP[ch7_aapi$edu == "High school"]) / sum(ch7_aapi$PWGTP)
edu_prop_aapi3 <- sum(ch7_aapi$PWGTP[ch7_aapi$edu == "Some college"]) / sum(ch7_aapi$PWGTP)
edu_prop_aapi4 <- sum(ch7_aapi$PWGTP[ch7_aapi$edu == "Bachelor degree"]) / sum(ch7_aapi$PWGTP)
edu_prop_aapi5 <- sum(ch7_aapi$PWGTP[ch7_aapi$edu == "Graduate or Professional"]) / sum(ch7_aapi$PWGTP)

# Calculate proportions of educational attainment for Non-Hispanic White
edu_prop_NHwhite1 <- sum(ch7_NHwhite$PWGTP[ch7_NHwhite$edu == "Less than high school"]) / sum(ch7_NHwhite$PWGTP)
edu_prop_NHwhite2 <- sum(ch7_NHwhite$PWGTP[ch7_NHwhite$edu == "High school"]) / sum(ch7_NHwhite$PWGTP)
edu_prop_NHwhite3 <- sum(ch7_NHwhite$PWGTP[ch7_NHwhite$edu == "Some college"]) / sum(ch7_NHwhite$PWGTP)
edu_prop_NHwhite4 <- sum(ch7_NHwhite$PWGTP[ch7_NHwhite$edu == "Bachelor degree"]) / sum(ch7_NHwhite$PWGTP)
edu_prop_NHwhite5 <- sum(ch7_NHwhite$PWGTP[ch7_NHwhite$edu == "Graduate or Professional"]) / sum(ch7_NHwhite$PWGTP)


# Calculate average earnings for AAPI
avg_earning_aapi1 <- weighted.mean(ch7_aapi$adj_ern[ch7_aapi$edu == "Less than high school"], ch7_aapi$PWGTP[ch7_aapi$edu == "Less than high school"])
avg_earning_aapi2 <- weighted.mean(ch7_aapi$adj_ern[ch7_aapi$edu == "High school"], ch7_aapi$PWGTP[ch7_aapi$edu == "High school"])
avg_earning_aapi3 <- weighted.mean(ch7_aapi$adj_ern[ch7_aapi$edu == "Some college"], ch7_aapi$PWGTP[ch7_aapi$edu == "Some college"])
avg_earning_aapi4 <- weighted.mean(ch7_aapi$adj_ern[ch7_aapi$edu == "Bachelor degree"], ch7_aapi$PWGTP[ch7_aapi$edu == "Bachelor degree"])
avg_earning_aapi5 <- weighted.mean(ch7_aapi$adj_ern[ch7_aapi$edu == "Graduate or Professional"], ch7_aapi$PWGTP[ch7_aapi$edu == "Graduate or Professional"])

# Calculate average earnings for Non-Hispanic White
avg_earning_NHwhite1 <- weighted.mean(ch7_NHwhite$adj_ern[ch7_NHwhite$edu == "Less than high school"], ch7_NHwhite$PWGTP[ch7_NHwhite$edu == "Less than high school"])
avg_earning_NHwhite2 <- weighted.mean(ch7_NHwhite$adj_ern[ch7_NHwhite$edu == "High school"], ch7_NHwhite$PWGTP[ch7_NHwhite$edu == "High school"])
avg_earning_NHwhite3 <- weighted.mean(ch7_NHwhite$adj_ern[ch7_NHwhite$edu == "Some college"], ch7_NHwhite$PWGTP[ch7_NHwhite$edu == "Some college"])
avg_earning_NHwhite4 <- weighted.mean(ch7_NHwhite$adj_ern[ch7_NHwhite$edu == "Bachelor degree"], ch7_NHwhite$PWGTP[ch7_NHwhite$edu == "Bachelor degree"])
avg_earning_NHwhite5 <- weighted.mean(ch7_NHwhite$adj_ern[ch7_NHwhite$edu == "Graduate or Professional"], ch7_NHwhite$PWGTP[ch7_NHwhite$edu == "Graduate or Professional"])

 #Format the educational attainment proportions to two decimal points
edu_prop_aapi1 <- format(edu_prop_aapi1, nsmall = 2)
edu_prop_NHwhite1 <- format(edu_prop_NHwhite1, nsmall = 2)
edu_prop_aapi2 <- format(edu_prop_aapi2, nsmall = 2)
edu_prop_NHwhite2 <- format(edu_prop_NHwhite2, nsmall = 2)
edu_prop_aapi3 <- format(edu_prop_aapi3, nsmall = 2)
edu_prop_NHwhite3 <- format(edu_prop_NHwhite3, nsmall = 2)
edu_prop_aapi4 <- format(edu_prop_aapi4, nsmall = 2)
edu_prop_NHwhite4 <- format(edu_prop_NHwhite4, nsmall = 2)
edu_prop_aapi5 <- format(edu_prop_aapi5, nsmall = 2)
edu_prop_NHwhite5 <- format(edu_prop_NHwhite5, nsmall = 2)

# Format the average earnings to integer values
avg_earning_aapi1 <- format(avg_earning_aapi1, big.mark = ",", scientific = FALSE)
avg_earning_NHwhite1 <- format(avg_earning_NHwhite1, big.mark = ",", scientific = FALSE)
avg_earning_aapi2 <- format(avg_earning_aapi2, big.mark = ",", scientific = FALSE)
avg_earning_NHwhite2 <- format(avg_earning_NHwhite2, big.mark = ",", scientific = FALSE)
avg_earning_aapi3 <- format(avg_earning_aapi3, big.mark = ",", scientific = FALSE)
avg_earning_NHwhite3 <- format(avg_earning_NHwhite3, big.mark = ",", scientific = FALSE)
avg_earning_aapi4 <- format(avg_earning_aapi4, big.mark = ",", scientific = FALSE)
avg_earning_NHwhite4 <- format(avg_earning_NHwhite4, big.mark = ",", scientific = FALSE)
avg_earning_aapi5 <- format(avg_earning_aapi5, big.mark = ",", scientific = FALSE)
avg_earning_NHwhite5 <- format(avg_earning_NHwhite5, big.mark = ",", scientific = FALSE)

library(data.table)

# Create data table for educational attainment proportions
data_table1 <- data.table(
  Group = c("AAPI", "Non-Hispanic White"),
  `Less than high school` = c(edu_prop_aapi1, edu_prop_NHwhite1),
  `High school` = c(edu_prop_aapi2, edu_prop_NHwhite2),
  `Some college` = c(edu_prop_aapi3, edu_prop_NHwhite3),
  `Bachelor degree` = c(edu_prop_aapi4, edu_prop_NHwhite4),
  `Graduate or Professional` = c(edu_prop_aapi5, edu_prop_NHwhite5)
)

# Create data table for average earnings
data_table2 <- data.table(
  Group = c("AAPI", "Non-Hispanic White"),
  `Less than high school` = c(avg_earning_aapi1, avg_earning_NHwhite1),
  `High school` = c(avg_earning_aapi2, avg_earning_NHwhite2),
  `Some college` = c(avg_earning_aapi3, avg_earning_NHwhite3),
  `Bachelor degree` = c(avg_earning_aapi4, avg_earning_NHwhite4),
  `Graduate or Professional` = c(avg_earning_aapi5, avg_earning_NHwhite5)
)

# Append the two data tables
appended_data_table <- rbindlist(list(data_table1, data_table2), fill = TRUE)

# Print the appended data table
print(appended_data_table)
write_xlsx(appended_data_table, "Y:/ProductDev/2024/Social Media Posts/VizOfTheWeek/05_May/AAPI_NHW_edu_earn.xlsx")

# Create data frame
data <- data.frame(
  Group = c("AAPI", "Others", "US-born"),
  EducationalAttainment = c(edu_prop_aapi, edu_prop_others, edu_prop_usborn),
  AverageIncome = c(avg_income_aapi, avg_income_others, avg_income_usborn)
)

# Plot the bar chart
ggplot(data, aes(x = Group, y = EducationalAttainment, fill = Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Bachelor or Higher by Group", x = "Group", y = "Proportion") +
  theme_minimal()



#this is for details within AAPI

# Subset the data frame
ch7 <- pums2022_5y[, c("SCHL", "PWGTP", "RACASN", "RACNH", "RACPI", "AGEP", "PINCP","FESRP", "RAC1P", "RAC2P")]
ch7$SCHL <- as.numeric(ch7$SCHL)
# Check for duplicate column names
duplicates <- duplicated(names(ch7))
if (any(duplicates)) {
  names(ch7) <- make.unique(names(ch7), sep = "_")
}

ch7 <- ch7 %>%
  filter(AGEP >= 25)
# Recode the education level
# ch7 <- ch7 %>%
#   mutate(edu = Recode(SCHL, recodes = "21:24 = 'Bachelor or higher'; else = 'HighSchool or lower'"))
ch7$edu <- Recode(ch7$SCHL, recodes = "21:24 = 'Bachelor or higher'; else = 'HighSchool or lower'")

ch72<-ch7%>% filter(POBP %in% c('207','210','212','215','217','231','233','247', '501', '515'))
ch7_NHwhite <- ch7[ch7$FHISP == '0' & !ch7$RAC1P ==1,]  # Non_hispanic white
table(ch72$POBP)
ch72_prop <- ch72 %>%
  group_by(POBP) %>%
  summarize(prop_bachelor_higher = sum(PWGTP[edu == "Bachelor or higher"]) / sum(PWGTP))

ch7_NHwhite_prop <- ch7_NHwhite %>%
  summarize(prop_bachelor_higher = sum(PWGTP[edu == "Bachelor or higher"]) / sum(PWGTP))  
ch7_NHwhite_prop$POBP<-"NH_white"
print(ch72_prop)
print(ch7_NHwhite_prop)
ggplot(ch72_prop, aes(x = POBP, y = prop_bachelor_higher, fill = "Bachelor or Higher")) +
  geom_bar(stat = "identity") +
  ggtitle("Proportion of Bachelor or higher by Asian and Pacific Islander in 2022 Texas") +
  ylab("Proportion") +
  xlab("Asia & Pacific Islander of birth") +
  scale_x_discrete(labels = c("China", "India", "Iran", 
                              "Japan", "Korea","Pakistan", 
                              "Philippines", "Vietnam","Australia", "New Zealand")) +
  scale_fill_manual(values ="gray") +
theme(legend.position = c(0.7, 0.9), 
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.key.width = unit(2, "cm"),
        axis.title.x = element_text(size = 18, face = "bold"), 
        axis.title.y = element_text(size = 18, face = "bold"), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), 
        plot.title = element_text(size = 20, face = "bold")) +
  guides(fill = guide_legend(override.aes = list(fill = "gray")))
ggsave("Y:/ProductDev/2024/Social Media Posts/VizOfTheWeek/05_May/edu_aapi.jpg", plot = last_plot(), width = 10, height = 10, dpi = 300)

#create to data table 
library(dplyr)

ch72_prop <- ch72 %>%
  group_by(POBP) %>%
  summarize(prop_bachelor_higher = sum(PWGTP[edu == "Bachelor or higher"]) / sum(PWGTP)) %>%
  mutate(POBP = case_when(
    POBP == "207" ~ "China",
    POBP == "210" ~ "India",
    POBP == "212" ~ "Iran",
    POBP == "215" ~ "Japan",
    POBP == "217" ~ "Korea",
    POBP == "231" ~ "Pakistan",
    POBP == "233" ~ "Philippines",
    POBP == "247" ~ "Vietnam",
    POBP == "501" ~ "Australia",
    POBP == "515" ~ "New Zealand",
    TRUE ~ POBP
  ))

# Print the updated data frame
print(ch72_prop)

# Append the two data tables
appended_data_table <- rbindlist(list(ch72_prop, ch7_NHwhite_prop), fill = TRUE)

# Print the appended data table
print(appended_data_table)
write_xlsx(appended_data_table, "Y:/ProductDev/2024/Social Media Posts/VizOfTheWeek/05_May/EduAttainment.xlsx")

# Subset the data to include only relevant variables and age groups 2022
ch8 <- pums2022_5y[, c("AGEP", "SEX", "PWGTP", "NATIVITY", "RAC1P")]
ch8 <- ch8[!(is.na(ch8$AGEP) | ch8$AGEP < 0 | ch8$AGEP > 99), ]

# Subset the data for AAPI
ch8_aapi <- ch8[ch8$NATIVITY == '2' & ch8$RAC1P %in% c('6', '7'),]

# Calculate population counts by age, sex, and birthplace
pop_counts <- aggregate(PWGTP ~ AGEP + SEX, data = ch8_aapi, FUN = sum)

# Create two separate data frames for males and females
males <- subset(pop_counts, SEX == '1')
females <- subset(pop_counts, SEX == '2')

# Plot the population pyramids using ggplot2
p1 <- ggplot(males, aes(x = -PWGTP, y = AGEP)) +
  geom_bar(stat = "identity", orientation = "y", fill = "lightblue") +
  scale_y_continuous(limits = c(0, 99), breaks = seq(0, 99, by = 5)) +
  scale_x_continuous(labels = abs, limits = c(-15000, 0)) +
  labs(title = "AAPI Males in Texas 2022", x = "Population", y = "Age") +
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12))

p2 <- ggplot(females, aes(x = PWGTP, y = AGEP)) +
  geom_bar(stat = "identity", orientation = "y", fill = "lightpink") +
  scale_y_continuous(limits = c(0, 99), breaks = seq(0, 99, by = 5)) +
  scale_x_continuous(labels = abs, limits = c(0, 15000)) +
  labs(title = "AAPI Females in Texas 2022", x = "Population", y = "") +
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12))

# Combine the plots into a single figure using gridExtra
pp<-grid.arrange(p1, p2, ncol = 2, widths = c(0.5, 0.5))

ggsave("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/2022_aapi.jpg", plot = pp, width = 12, height = 16, dpi = 500)


# Subset the data for AAPI 2022
ch8_nativ <- ch8[ch8$NATIVITY == '1' ,]

# Calculate population counts by age, sex, and birthplace
pop_counts <- aggregate(PWGTP ~ AGEP + SEX, data = ch8_nativ, FUN = sum)

# Create two separate data frames for males and females
males <- subset(pop_counts, SEX == '1')
females <- subset(pop_counts, SEX == '2')

# Plot the population pyramids using ggplot2
p1 <- ggplot(males, aes(x = -PWGTP, y = AGEP)) +
  geom_bar(stat = "identity", orientation = "y", fill = "lightblue") +
  scale_y_continuous(limits = c(0, 99), breaks = seq(0, 99, by = 5)) +
  scale_x_continuous(labels = abs, limits = c(-250000, 0)) +
  labs(title = "US born Males in Texas 2022", x = "Population", y = "Age") +
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12))

p2 <- ggplot(females, aes(x = PWGTP, y = AGEP)) +
  geom_bar(stat = "identity", orientation = "y", fill = "lightpink") +
  scale_y_continuous(limits = c(0, 99), breaks = seq(0, 99, by = 5)) +
  scale_x_continuous(labels = abs, limits = c(0, 250000)) +
  labs(title = "US born Females in Texas 2022", x = "Population", y = "") +
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12))

# Combine the plots into a single figure using gridExtra
pp<-grid.arrange(p1, p2, ncol = 2, widths = c(0.5, 0.5))

ggsave("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/2022_native.jpg", plot = pp, width = 12, height = 16, dpi = 500)


#2011 pums data
#create excel table from acs 5 year
library(writexl)
library(dplyr)
pums2011_5y<-read_sas("Y:/Data/Census/programs-surveys/acs/data/pums/2011/5-Year/sas/psam_p48.sas7bdat")

# Subset the data to include only relevant variables and age groups 2011
ch9 <- pums2011_5y[, c("AGEP", "SEX", "PWGTP", "NATIVITY", "RAC1P")]
ch9 <- ch9[!(is.na(ch9$AGEP) | ch9$AGEP < 0 | ch9$AGEP > 99), ]

# Subset the data for AAPI 2022
ch9_aapi <- ch9[ch9$NATIVITY == '2' & ch9$RAC1P %in% c('6', '7'),]

# Calculate population counts by age, sex, and birthplace
pop_counts <- aggregate(PWGTP ~ AGEP + SEX, data = ch9_aapi, FUN = sum)

# Create two separate data frames for males and females
males <- subset(pop_counts, SEX == '1')
females <- subset(pop_counts, SEX == '2')

# Plot the population pyramids using ggplot2
p1 <- ggplot(males, aes(x = -PWGTP, y = AGEP)) +
  geom_bar(stat = "identity", orientation = "y", fill = "lightblue") +
  scale_y_continuous(limits = c(0, 99), breaks = seq(0, 99, by = 5)) +
  scale_x_continuous(labels = abs, limits = c(-15000, 0)) +
  labs(title = "AAPI Males in Texas 2011", x = "Population", y = "Age") +
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12))

p2 <- ggplot(females, aes(x = PWGTP, y = AGEP)) +
  geom_bar(stat = "identity", orientation = "y", fill = "lightpink") +
  scale_y_continuous(limits = c(0, 99), breaks = seq(0, 99, by = 5)) +
  scale_x_continuous(labels = abs, limits = c(0, 15000)) +
  labs(title = "AAPI Females in Texas 2011", x = "Population", y = "") +
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12))

# Combine the plots into a single figure using gridExtra
pp<-grid.arrange(p1, p2, ncol = 2, widths = c(0.5, 0.5))

ggsave("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/2011_aapi.jpg", plot = pp, width = 12, height = 16, dpi = 500)


# Subset the data for AAPI 2011
ch9_nativ <- ch9[ch9$NATIVITY == '1' ,]

# Calculate population counts by age, sex, and birthplace
pop_counts <- aggregate(PWGTP ~ AGEP + SEX, data = ch9_nativ, FUN = sum)

# Create two separate data frames for males and females
males <- subset(pop_counts, SEX == '1')
females <- subset(pop_counts, SEX == '2')

# Plot the population pyramids using ggplot2
p1 <- ggplot(males, aes(x = -PWGTP, y = AGEP)) +
  geom_bar(stat = "identity", orientation = "y", fill = "lightblue") +
  scale_y_continuous(limits = c(0, 99), breaks = seq(0, 99, by = 5)) +
  scale_x_continuous(labels = abs, limits = c(-250000, 0)) +
  labs(title = "US born Males in Texas 2011", x = "Population", y = "Age") +
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12))

p2 <- ggplot(females, aes(x = PWGTP, y = AGEP)) +
  geom_bar(stat = "identity", orientation = "y", fill = "lightpink") +
  scale_y_continuous(limits = c(0, 99), breaks = seq(0, 99, by = 5)) +
  scale_x_continuous(labels = abs, limits = c(0, 250000)) +
  labs(title = "US born Females in Texas 2011", x = "Population", y = "") +
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12))

# Combine the plots into a single figure using gridExtra
pp<-grid.arrange(p1, p2, ncol = 2, widths = c(0.5, 0.5))

ggsave("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/weekly_chart/R/2011_native.jpg", plot = pp, width = 12, height = 16, dpi = 500)

#produce data for excel edit foreign born API vs native born API
library(writexl)
library(dplyr)
pums2022_5y<-read_sas("Y:/Data/Census/programs-surveys/acs/data/pums/2022/5-Year/sas/psam_p48.sas7bdat")

ch10 <- pums2022_5y[, c("AGEP", "PWGTP", "NATIVITY", "RAC1P")]
ch10_api <- subset(ch10, RAC1P %in% c('6','7'))

# Subset data for foreign-born and native-born APIs
ch10_api_fb <- subset(ch10_api, NATIVITY == '2')
ch10_api_nb <- subset(ch10_api, NATIVITY == '1')

# Create age group labels
age_groups <- data.frame(age_start = c(0, 19, 25, 36, 65),
                         age_end = c(18, 24, 35, 64, 100),
                         age_range = c("0-18", "19-24", "25-35", "36-64", "65+"),
                         dummy=c(1,2,3,4,5))


# Aggregate PWGTP by age group for foreign-born and native-born APIs
pwgtp_by_age_fb <- aggregate(PWGTP ~ cut(AGEP, breaks = c(age_groups$age_start, Inf), labels = FALSE), data = ch10_api_fb, sum)
names(pwgtp_by_age_fb)[1] <- "dummy"
pwgtp_by_age_nb <- aggregate(PWGTP ~ cut(AGEP, breaks = c(age_groups$age_start, Inf), labels = FALSE), data = ch10_api_nb, sum)
names(pwgtp_by_age_nb)[1] <- "dummy"
# Merge with age_groups data frame to add age range
pwgtp_by_age_fb <- merge(pwgtp_by_age_fb, age_groups, by.x = "dummy", by.y = "dummy")
pwgtp_by_age_nb <- merge(pwgtp_by_age_nb, age_groups, by.x = "dummy", by.y = "dummy")

# Rename columns
names(pwgtp_by_age_fb)[2] <- "pwgtp_sum_fb_api"
names(pwgtp_by_age_nb)[2] <- "pwgtp_sum_nb_api"

# Merge foreign-born and native-born data frames
pwgtp_by_age <- merge(pwgtp_by_age_fb, pwgtp_by_age_nb, by = "dummy")


# View the resulting data frame
pwgtp_by_age

pwgtp_by_age <- pwgtp_by_age %>%
  select(age_range.x, pwgtp_sum_fb_api, pwgtp_sum_nb_api)

#save in to excel file
library(writexl)

write_xlsx(pwgtp_by_age, "Y:/ProductDev/2024/Social Media Posts/VizOfTheWeek/05_May/pwgtp_by_age.xlsx")

#Texas whole

ch10 <- pums2022_5y[, c("AGEP", "PWGTP", "NATIVITY", "RAC1P")]

# Subset data for foreign-born and native-born APIs
ch10_fb <- subset(ch10, NATIVITY == '2')
ch10_nb <- subset(ch10, NATIVITY == '1')

# Create age group labels
age_groups <- data.frame(age_start = c(0, 19, 25, 36, 65),
                         age_end = c(18, 24, 35, 64, 100),
                         age_range = c("0-18", "19-24", "25-35", "36-64", "65+"),
                         dummy=c(1,2,3,4,5))


# Aggregate PWGTP by age group for foreign-born and native-born APIs
pwgtp_by_age_fb <- aggregate(PWGTP ~ cut(AGEP, breaks = c(age_groups$age_start, Inf), labels = FALSE), data = ch10_fb, sum)
names(pwgtp_by_age_fb)[1] <- "dummy"
pwgtp_by_age_nb <- aggregate(PWGTP ~ cut(AGEP, breaks = c(age_groups$age_start, Inf), labels = FALSE), data = ch10_nb, sum)
names(pwgtp_by_age_nb)[1] <- "dummy"
# Merge with age_groups data frame to add age range
pwgtp_by_age_fb <- merge(pwgtp_by_age_fb, age_groups, by.x = "dummy", by.y = "dummy")
pwgtp_by_age_nb <- merge(pwgtp_by_age_nb, age_groups, by.x = "dummy", by.y = "dummy")

# Rename columns
names(pwgtp_by_age_fb)[2] <- "pwgtp_sum_fb_tx"
names(pwgtp_by_age_nb)[2] <- "pwgtp_sum_nb_tx"

# Merge foreign-born and native-born data frames
pwgtp_by_age <- merge(pwgtp_by_age_fb, pwgtp_by_age_nb, by = "dummy")


# View the resulting data frame
pwgtp_by_age <- pwgtp_by_age %>%
  select(age_range.x, pwgtp_sum_fb_tx, pwgtp_sum_nb_tx)

pwgtp_by_age

#save in to excel file
library(writexl)

write_xlsx(pwgtp_by_age, "Y:/ProductDev/2024/Social Media Posts/VizOfTheWeek/05_May/pwgtp_by_age_tx.xlsx")

# Subset the data frame
ch11 <- pums2022_5y[, c("SCHL", "PWGTP", "RACASN", "RACNH", "RACPI", "AGEP", "PINCP","FESRP", "RAC1P", "RAC2P")]
ch11$SCHL <- as.numeric(ch11$SCHL)
# Check for duplicate column names
duplicates <- duplicated(names(ch11))

if (any(duplicates)) {
  names(ch11) <- make.unique(names(ch11), sep = "_")
}

ch11 <- ch11 %>%
  filter(AGEP >= 25)
# Recode the education level
ch11$edu <- Recode(ch11$SCHL, recodes = "21:24 = 'Bachelor or higher'; else = 'HighSchool or lower'")

ch11_AAPI <- ch11[ch11$RACASN == '1' | ch11$RACNH == '1' | ch11$RACPI == '1',]  # AAPI
table(ch11_AAPI$RAC2P)

ch11_AAPI_d <- ch11_AAPI %>%
  group_by(RAC2P) %>%
  summarize(obs = n(),
  prop_bachelor_higher = sum(PWGTP[edu == "Bachelor or higher"]) / sum(PWGTP)) %>%
  mutate(Race = case_when(
    RAC2P == '38' ~ 'Asian Indian',
    RAC2P == '39' ~ 'Bangladeshi',
    RAC2P == '40' ~ 'Bhutanese',
    RAC2P == '41' ~ 'Burmese',
    RAC2P == '42' ~ 'Cambodian',
    RAC2P == '43' ~ 'Chinese, except Taiwanese',
    RAC2P == '44' ~ 'Taiwanese',
    RAC2P == '45' ~ 'Filipino',
    RAC2P == '46' ~ 'Hmong',
    RAC2P == '47' ~ 'Indonesian',
    RAC2P == '48' ~ 'Japanese',
    RAC2P == '49' ~ 'Korean',
    RAC2P == '50' ~ 'Laotian',
    RAC2P == '51' ~ 'Malaysian',
    RAC2P == '52' ~ 'Mongolian',
    RAC2P == '53' ~ 'Nepalese',
    RAC2P == '54' ~ 'Pakistani',
    RAC2P == '55' ~ 'Sri Lankan',
    RAC2P == '56' ~ 'Thai',
    RAC2P == '57' ~ 'Vietnamese',
    RAC2P == '58' ~ 'Other Asian ',
    RAC2P == '59' ~ 'All combinations of Asian races only',
    RAC2P == '60' ~ 'Native Hawaiian',
    RAC2P == '61' ~ 'Samoan',
    RAC2P == '62' ~ 'Tongan',
    RAC2P == '63' ~ 'Chamorro',
    RAC2P == '64' ~ 'Marshallese',
    RAC2P == '65' ~ 'Fijian',
    RAC2P == '66' ~ 'Other Native Hawaiian and Other Pacific Islander',
    RAC2P == '67' ~ 'Some Other Race',
    RAC2P == '68' ~ 'Two or More Races',
    TRUE ~ NA_character_
  ))
print(ch11_AAPI_d)

ch11_TX<- ch11%>%
  summarize(obs = n(), prop_bachelor_higher = sum(PWGTP[edu == "Bachelor or higher"]) / sum(PWGTP))  
ch11_TX$RAC2P<-"Texas_all"
ch11_TX$Race<-"Texas_all"

print(ch11_TX)

ch11_AAPI<- ch11_AAPI %>%
  summarize(obs = n(), prop_bachelor_higher = sum(PWGTP[edu == "Bachelor or higher"]) / sum(PWGTP))
ch11_AAPI$RAC2P<-"AAPI_all"
ch11_AAPI$Race<-"AAPI_all"
print(ch11_AAPI)

# Append the two data tables
appended_data_table <- rbindlist(list(ch11_AAPI_d, ch11_AAPI,ch11_TX), fill = TRUE)

double_check <- appended_data_table %>%
  filter(obs>700)%>%
  arrange(desc(prop_bachelor_higher))

print(double_check)


# Print the appended data table
print(appended_data_table)
write_xlsx(appended_data_table, "Y:/ProductDev/2024/Social Media Posts/VizOfTheWeek/05_May/EduAttainment.xlsx")
