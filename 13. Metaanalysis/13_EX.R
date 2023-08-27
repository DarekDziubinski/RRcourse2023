library(meta)
library(readxl)

setwd("C:\\Users\\dziub\\RRcourse2023\\13. Metaanalysis")
data <- read_excel("data/metaanalysis_data.xlsx")

View(data)
str(data)
data$Dominant_Author_Gender <- ifelse(data$`Female authors` > data$`Male authors`, "Female", "Male")
data$Dominant_Author_Gender <- as.factor(data$Dominant_Author_Gender)

# I.I combine the effects / For boy toys:
m.boys <- metacont(n.e = data$N_boys,
                   mean.e = data$Mean_boys_play_male,
                   sd.e = data$SD_boys_play_male,
                   n.c = data$N_girls,
                   mean.c = data$Mean_girls_play_male,
                   sd.c = data$SD_girls_play_male,
                   data = data,
                   studlab = paste(data$Study),
                   comb.fixed = TRUE,
                   comb.random = TRUE)
m.boys

# Output / Conclusions
# The model indicates that there is a significant effect in the included studies (p>value), both in the fixed-effects model and in the random-effects model. 
# However, there is also significant heterogeneity (I^2) between studies, suggesting that not all studies report the same effect size.


# I.II combine the effects / For girl toys:
m.girls <- metacont(n.e = data$N_boys,
                    mean.e = data$Mean_boys_play_female,
                    sd.e = data$SD_boys_play_female,
                    n.c = data$N_girls,
                    mean.c = data$Mean_girls_play_female,
                    sd.c = data$SD_girls_play_female,
                    data = data,
                    studlab = paste(data$Study),
                    comb.fixed = TRUE,
                    comb.random = TRUE)
m.girls

# Output / Conclusions
# The model indicates that there is a significant effect in the included studies (p>value), both in the fixed-effects model and in the random-effects model. 
# However, there is also significant heterogeneity (I^2) between studies, suggesting that not all studies report the same effect size.