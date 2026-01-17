load("/Users/adrielbustillos/Downloads/ICPSR_39096/DS0001/39096-0001-Data.rda")
library(dplyr)
library(tidyr)
library(car)
library(rsq)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(stargazer)
library(officer)
library(flextable)
library(xml2)
library(sandwich)
library(ggpattern)
library(janitor)
library(dplyr)
library(margins)
library(effects)
library(pscl)

cmps2020 <- da39096.0001



cmps2020 %>% count(S5_AGE, .drop = F)

cmps2020<-
  cmps2020 %>% mutate(unionbinary = 
                         case_when(Q171R4 == "(0) NO TO: Union or work association leaders" ~ 0,
                                   Q171R4 == "(1) Union or work association leaders"~ 1,
                                   TRUE ~NA_real_))
cmps2020 <- 
  cmps2020 %>% mutate(inequality = 
                        case_when(Q417_Q424R5 == "(1) Strongly support"~1, 
                                  Q417_Q424R5 == "(2) Support" ~ 1,
                                  Q417_Q424R5 == "(3) Somewhat support" ~ 1,
                                  Q417_Q424R5 == "(4) Somewhat oppose" ~ 0,
                                  Q417_Q424R5 == "(5) Oppose" ~ 0,
                                  Q417_Q424R5 == "(6) Strongly oppose" ~ 0,
                                
                                  TRUE ~ NA_real_))

cmps2020 <-
  cmps2020 %>% mutate(age =
                        case_when(S5_AGE == "(2) 18-29"~"(1) 18-29", 
                                  S5_AGE == "(3) 30-39"~"(2) 30-39", 
                                  S5_AGE == "(4) 40-49"~"(3) 40-49", 
                                  S5_AGE == "(5) 50-59"~"(4) 50-59",
                                  S5_AGE == "(6) 60-69"~"(5) 60-69",
                                  S5_AGE == "(7) 70 +"~"(6) 70 +",
                                  
                                  TRUE ~ NA_character_))

cmps2020 %>% count(age)

#Model 1: Control
model1 <- glm(
  inequality ~ S2_RACE_PRIME + S3B + age + S13 + S14,
  data = cmps2020,
  family = binomial,
  na.action = na.omit
)

summary(model1)
summary(
  margins(model1, change= "minmax"))

preds1<-
  summary(
    margins(model1, change= "minmax"))

preds1 <- preds1 %>%
  mutate(
    variable = case_when(
      factor == "S2_RACE_PRIME(2) Hispanic or Latino" ~ "Race (Hispanic or Latino)",
      factor == "S2_RACE_PRIME(3) Black or African American" ~ "Race (Black)",
      factor == "S2_RACE_PRIME(4) Asian American" ~ "Race (Asian American)",
      factor == "S2_RACE_PRIME(5) American Indian/Native American" ~ "Race (American Indian)",
      factor == "S2_RACE_PRIME(6) Arab, Middle Eastern or North African" ~ "Race (Arab/MENA)",
      factor == "S2_RACE_PRIME(7) Native Hawaiian" ~ "Race (Native Hawaiian)",
      factor == "S2_RACE_PRIME(8) Not Hawaiian, but other Pacific Islander" ~ "Race (Pacific Islander)",
      
      factor == "S3B(2) Woman" ~ "Gender(Woman)",
      factor == "S3B(3) Non-binary" ~ "Gender(Non-binary)",
      factor == "S3B(4) Something else (Specify)" ~ "Gender(Something else)",
      
      factor == "age(2) 30-39" ~ "Age (30–39)",
      factor == "age(3) 40-49" ~ "Age (40–49)",
      factor == "age(4) 50-59" ~ "Age (50–59)",
      factor == "age(5) 60-69" ~ "Age (60–69)",
      factor == "age(6) 70 +"  ~ "Age (70+)",
      
      factor == "S13(2) Some High School, but did not graduate" ~ "Education(Some HS)",
      factor == "S13(3) High School graduate or GED"           ~ "Education (HS Grad/GED)",
      factor == "S13(4) Some college"                          ~ "Education(Some college)",
      factor == "S13(5) Associates, 2-year degree"             ~ "Education (Associates)",
      factor == "S13(6) Bachelors, 4-year degree"              ~ "Education (Bachelors)",
      factor == "S13(7) Post-graduate degree"                  ~ "Education (Post-graduate)",
      
      factor == "S14(2) Large suburb near large city"          ~ "Community Views (Large suburb)",
      factor == "S14(3) Small suburb near small town or city"  ~ "Community Views (Small suburb)",
      factor == "S14(4) Small town or small city"              ~ "Community Views (Small town)",
      factor == "S14(5) Rural area"                            ~ "Community Views (Rural area)",
      
      TRUE ~ NA_character_
    )
  )

preds1 <- preds1 %>%
  mutate(
    variable = factor(variable, levels = rev(c(
      "Race (Hispanic or Latino)",
      "Race (Black)",
      "Race (Asian American)",
      "Race (American Indian)",
      "Race (Arab/MENA)",
      "Race (Native Hawaiian)",
      "Race (Pacific Islander)",
      
      "Gender(Woman)",
      "Gender(Non-binary)",
      "Gender(Something else)",
      
      "Age (30–39)",
      "Age (40–49)",
      "Age (50–59)",
      "Age (60–69)",
      "Age (70+)",
      
      "Education(Some HS)",
      "Education (HS Grad/GED)",
      "Education(Some college)",
      "Education (Associates)",
      "Education (Bachelors)",
      "Education (Post-graduate)",
      
      "Community Views (Large suburb)",
      "Community Views (Small suburb)",
      "Community Views (Small town)",
      "Community Views (Rural area)"
    ))),
    sig = ifelse(lower > 0 | upper < 0,
                 "Significant (p < .05)",
                 "Not significant")
  )

ggplot(preds1,
       aes(x = variable,
           y = AME,
           ymin = lower,
           ymax = upper,
           color = sig)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "maroon") +
  geom_pointrange(size = 0.7) +
  coord_flip() +
  scale_color_manual(values = c(
    "Significant (p < .05)" = "black",
    "Not significant"       = "grey60"
  )) +
  theme_bw(base_size = 13) +
  theme(
    axis.text.y  = element_text(size = 10),
    plot.title   = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Variable",
    y = "Average Marginal Effect",
    color = ""
  )

ggsave("model1_ame.pdf", width = 8, height = 10)   # height increased

#Model 2: H1
model2 <- glm(
  inequality ~ unionbinary + S2_RACE_PRIME + S3B + age + 
     S13 + S14,
  data = cmps2020,
  family = "binomial"
  #na.action = na.omit
)

summary(model2)


preds<-
summary(
margins(model2, change= "minmax"))

view(preds)

library(dplyr)
library(ggplot2)

## START OVER: create a fresh `variable` from `factor`
preds <- preds %>%
  mutate(
    variable = case_when(
      factor == "unionbinary" ~ "Union Representations",
      
      factor == "S2_RACE_PRIME(2) Hispanic or Latino" ~ "Race (Hispanic or Latino)",
      factor == "S2_RACE_PRIME(3) Black or African American" ~ "Race (Black)",
      factor == "S2_RACE_PRIME(4) Asian American" ~ "Race (Asian American)",
      factor == "S2_RACE_PRIME(5) American Indian/Native American" ~ "Race (American Indian)",
      factor == "S2_RACE_PRIME(6) Arab, Middle Eastern or North African" ~ "Race (Arab/MENA)",
      factor == "S2_RACE_PRIME(7) Native Hawaiian" ~ "Race (Native Hawaiian)",
      factor == "S2_RACE_PRIME(8) Not Hawaiian, but other Pacific Islander" ~ "Race (Pacific Islander)",
      
      factor == "S3B(2) Woman" ~ "Gender(Woman)",
      factor == "S3B(3) Non-binary" ~ "Gender(Non-binary)",
      factor == "S3B(4) Something else (Specify)" ~ "Gender(Something else)",
      
      factor == "age(2) 30-39" ~ "Age (30–39)",
      factor == "age(3) 40-49" ~ "Age (40–49)",
      factor == "age(4) 50-59" ~ "Age (50–59)",
      factor == "age(5) 60-69" ~ "Age (60–69)",
      factor == "age(6) 70 +"  ~ "Age (70+)",
      
      factor == "S13(2) Some High School, but did not graduate" ~ "Education(Some HS)",
      factor == "S13(3) High School graduate or GED"           ~ "Education (HS Grad/GED)",
      factor == "S13(4) Some college"                          ~ "Education(Some college)",
      factor == "S13(5) Associates, 2-year degree"             ~ "Education (Associates)",
      factor == "S13(6) Bachelors, 4-year degree"              ~ "Education (Bachelors)",
      factor == "S13(7) Post-graduate degree"                  ~ "Education (Post-graduate)",
      
      factor == "S14(2) Large suburb near large city"          ~ "Community Views (Large suburb)",
      factor == "S14(3) Small suburb near small town or city"  ~ "Community Views (Small suburb)",
      factor == "S14(4) Small town or small city"              ~ "Community Views (Small town)",
      factor == "S14(5) Rural area"                            ~ "Community Views (Rural area)",
      
      TRUE ~ NA_character_
    )
  )

unique(preds$variable)

preds <- preds %>%
  mutate(
    variable = factor(variable, levels = rev(c(
      "Union Representations",
      
      "Race (Hispanic or Latino)",
      "Race (Black)",
      "Race (Asian American)",
      "Race (American Indian)",
      "Race (Arab/MENA)",
      "Race (Native Hawaiian)",
      "Race (Pacific Islander)",
      
      "Gender(Woman)",
      "Gender(Non-binary)",
      "Gender(Something else)",
      
      "Age (30–39)",
      "Age (40–49)",
      "Age (50–59)",
      "Age (60–69)",
      "Age (70+)",
      
      "Education(Some HS)",
      "Education (HS Grad/GED)",
      "Education(Some college)",
      "Education (Associates)",
      "Education (Bachelors)",
      "Education (Post-graduate)",
      
      "Community Views (Large suburb)",
      "Community Views (Small suburb)",
      "Community Views (Small town)",
      "Community Views (Rural area)"
    ))),
    sig = ifelse(lower > 0 | upper < 0,
                 "Significant (p < .05)",
                 "Not significant")
  )

ggplot(preds,
       aes(x = variable,
           y = AME,
           ymin = lower,
           ymax = upper,
           color = sig)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "maroon") +
  geom_pointrange(size = 0.7) +
  coord_flip() +
  scale_color_manual(values = c(
    "Significant (p < .05)" = "black",
    "Not significant"       = "grey60"
  )) +
  theme_bw(base_size = 13) +
  theme(
    axis.text.y  = element_text(size = 10),
    plot.title   = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Variable",
    y = "Average Marginal Effect",
    color = ""
  )

ggsave("model2_ame.png", width = 8, height = 6, dpi = 300)

ggsave("model2_ame.pdf", width = 8, height = 10)   # height increased

plot(margins(model2, variable = "unionbinary"))

#Model 3: Reverse Check-Endogeneity 
model3 <- glm(
  unionbinary ~ inequality + S2_RACE_PRIME + S3B + age + S13 + S14,
  data = cmps2020,
  family = binomial,
  na.action = na.omit
)

summary(model3)

summary(margins(model3, change = "minmax"))

preds3<-
  summary(
    margins(model3, change= "minmax"))

view(preds3)
preds3 <- preds3 %>%
  mutate(
    variable = case_when(
      factor == "inequality" ~ "Support for Racial Inequality Education",
      
      factor == "S2_RACE_PRIME(2) Hispanic or Latino" ~ "Race (Hispanic or Latino)",
      factor == "S2_RACE_PRIME(3) Black or African American" ~ "Race (Black)",
      factor == "S2_RACE_PRIME(4) Asian American" ~ "Race (Asian American)",
      factor == "S2_RACE_PRIME(5) American Indian/Native American" ~ "Race (American Indian)",
      factor == "S2_RACE_PRIME(6) Arab, Middle Eastern or North African" ~ "Race (Arab/MENA)",
      factor == "S2_RACE_PRIME(7) Native Hawaiian" ~ "Race (Native Hawaiian)",
      factor == "S2_RACE_PRIME(8) Not Hawaiian, but other Pacific Islander" ~ "Race (Pacific Islander)",
      
      factor == "S3B(2) Woman" ~ "Gender(Woman)",
      factor == "S3B(3) Non-binary" ~ "Gender(Non-binary)",
      factor == "S3B(4) Something else (Specify)" ~ "Gender(Something else)",
      
      factor == "age(2) 30-39" ~ "Age (30–39)",
      factor == "age(3) 40-49" ~ "Age (40–49)",
      factor == "age(4) 50-59" ~ "Age (50–59)",
      factor == "age(5) 60-69" ~ "Age (60–69)",
      factor == "age(6) 70 +"  ~ "Age (70+)",
      
      factor == "S13(2) Some High School, but did not graduate" ~ "Education(Some HS)",
      factor == "S13(3) High School graduate or GED"           ~ "Education (HS Grad/GED)",
      factor == "S13(4) Some college"                          ~ "Education(Some college)",
      factor == "S13(5) Associates, 2-year degree"             ~ "Education (Associates)",
      factor == "S13(6) Bachelors, 4-year degree"              ~ "Education (Bachelors)",
      factor == "S13(7) Post-graduate degree"                  ~ "Education (Post-graduate)",
      
      factor == "S14(2) Large suburb near large city"          ~ "Community Views (Large suburb)",
      factor == "S14(3) Small suburb near small town or city"  ~ "Community Views (Small suburb)",
      factor == "S14(4) Small town or small city"              ~ "Community Views (Small town)",
      factor == "S14(5) Rural area"                            ~ "Community Views (Rural area)",
      
      TRUE ~ NA_character_
    )
  )

unique(preds$variable)

preds3 <- preds3 %>%
  mutate(
    variable = factor(variable, levels = rev(c(
      "Support for Racial Inequality Education",
      
      "Race (Hispanic or Latino)",
      "Race (Black)",
      "Race (Asian American)",
      "Race (American Indian)",
      "Race (Arab/MENA)",
      "Race (Native Hawaiian)",
      "Race (Pacific Islander)",
      
      "Gender(Woman)",
      "Gender(Non-binary)",
      "Gender(Something else)",
      
      "Age (30–39)",
      "Age (40–49)",
      "Age (50–59)",
      "Age (60–69)",
      "Age (70+)",
      
      "Education(Some HS)",
      "Education (HS Grad/GED)",
      "Education(Some college)",
      "Education (Associates)",
      "Education (Bachelors)",
      "Education (Post-graduate)",
      
      "Community Views (Large suburb)",
      "Community Views (Small suburb)",
      "Community Views (Small town)",
      "Community Views (Rural area)"
    ))),
    sig = ifelse(lower > 0 | upper < 0,
                 "Significant (p < .05)",
                 "Not significant")
  )

ggplot(preds3,
       aes(x = variable,
           y = AME,
           ymin = lower,
           ymax = upper,
           color = sig)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "maroon") +
  geom_pointrange(size = 0.7) +
  coord_flip() +
  scale_color_manual(values = c(
    "Significant (p < .05)" = "black",
    "Not significant"       = "grey60"
  )) +
  theme_bw(base_size = 13) +
  theme(
    axis.text.y  = element_text(size = 10),
    plot.title   = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Variable",
    y = "Average Marginal Effect",
    color = ""
  )

ggsave("model3_ame.pdf", width = 8, height = 10)   # height increased

#Model 4: Interaction-Race Theoretical Extension
model4 <- glm(
  inequality ~ unionbinary * S2_RACE_PRIME + S3B + age + S13 + S14,
  data = cmps2020,
  family = binomial,
  #na.action = na.omit
)



#Statistical Testing
anova(model1, model2, test = "Chisq") ##not working?

pR2(model2)

vif(model2)
plot(model2)


pred <- ifelse(predict(model2, type="response") > 0.5, 1, 0)
mean(pred == cmps2020$inequality)
