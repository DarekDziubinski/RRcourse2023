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

# II. create a funnel plot
m.boys %>% funnel()
m.girls %>% funnel()

# In my opinion, the funnel plot has an asymmetric shape, indicating a possible publication error or some other source of asymmetry. 
# The higher heterogeneity (I^2 = 79.9%) causes them to be widely scattered, especially in the lower part of the plot. 
# The points are stacked around values of -53.5 (for the fixed effects model / MD value) and -79.5 (for the random effects model / MD value). 
# Overall, thedata suggest that there is a significant difference between study groups. (BOTH CHARTS / detailed description for the second chart)

# III. check if methods / quality affect the results
m.quality_boys <- m.boys %>% metareg(~ `NOS score`)
m.quality_girls <- m.girls %>% metareg(~ `NOS score`)
m.quality_boys
m.quality_girls

# For boys, quality (assessed by the 'NOS score') may influence the results, but this relationship is not certain (p-value).
# The estimate for the NOS score had a p-value of 0.0548. Although this is close to the significance threshold of 0.05, 
# it is not fully statistically significant. Therefore, the conclusion is that in boys, the quality of the examination may affect the results,
# but this relationship is not certain.
# For girls, there is a statistically significant positive correlation between quality (NOS score) and effect size.
