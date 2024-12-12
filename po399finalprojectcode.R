# author: Cecily Wang-Munoz
# date: 12/10/2024
# PO399



install.packages(c("geofacet", "tidyverse", "sf", "ggplot2", "dplyr", "readr", "tmap", "stargazer"))
library(tidyverse)

library(sf)       #shapefile
library(ggplot2)  
library(dplyr)    
library(readr)   
library(tmap) 
library(broom)

# datasets
voter_turnout <- readRDS("voter_registration_and_turnout_2020.rds")
demographics <- readRDS("demographic_data_by_county_2020.rds")
urban_rural <- readRDS("urban_rural_codes.rds")
county_shapefile <- readRDS("usa_county_shapefile.rds")


# looking at each of the datasets to look at columns before merging later
str(voter_turnout)
str(demographics)
str(urban_rural)
str(county_shapefile)

# _______________________________ Data cleaning and processing ________________________________________________
# Merge data by fips codes so that we have all the counties we need that overlap within the different datasets
data <- voter_turnout %>%
  inner_join(demographics, by = "fips") %>%
  inner_join(urban_rural, by = "fips") %>%
  rename(
    state = state.x,
    state_name = state_name.x,
    state_code = state_code.x
  ) %>%
  #cleaning it up a little by removing extra columns that showed up
  select(-c(state.y, state_name.y, state_code.y))  


# Calculating turnout rate, scaling median income, and encoding "urban" and "rural" 
#Turnout rate is calculated as: Total votes cast/totalregistered voters
data <- data %>%
  mutate(
    WhiteTurnout = ifelse(registered_White > 0, voted_White / registered_White, NA),
    BlackTurnout = ifelse(registered_Black > 0, voted_Black / registered_Black, NA),
    HispanicTurnout = ifelse(registered_Hispanic > 0, voted_Hispanic / registered_Hispanic, NA),
    AsianTurnout = ifelse(registered_Asian > 0, voted_Asian / registered_Asian, NA),
    OtherTurnout = ifelse(registered_Other > 0, voted_Other / registered_Other, NA),
    TurnoutRate = (voted_White + voted_Black + voted_Hispanic + voted_Asian + voted_Other) /
      (registered_White + registered_Black + registered_Hispanic + registered_Asian + registered_Other),
    Urban = ifelse(urban_rural_code <= 3, "Urban", "Rural"),
    MedianIncomeScaled = scale(median_household_income)
  )


# a left join of the shapefile with data for mapping by fips
map_data <- county_shapefile %>%
  left_join(data, by = "fips")

# checking for mismatched fips just incase 
mismatches <- anti_join(county_shapefile, data, by = "fips")
if (nrow(mismatches) > 0) {
  print("Mismatched fips codes:")
  print(mismatches)
} else {
  print("No mismatched fips codes")
}

#removing NA
data <- data %>%
  filter(!is.na(TurnoutRate), !is.na(MedianIncomeScaled))


# _____________________________ Regression models _____________________________________

# MODEL 1: overall turnout using race-specific turnout rates, median income, and urbanization: 
model_race <- lm(TurnoutRate ~ WhiteTurnout + BlackTurnout + HispanicTurnout + AsianTurnout + MedianIncomeScaled + Urban, data = data)

# MODEL 2: interaction effects between urbanization and race specific turnout rates
model_interaction <- lm(TurnoutRate ~ WhiteTurnout * Urban + BlackTurnout * Urban + HispanicTurnout * Urban + AsianTurnout * Urban + MedianIncomeScaled, data = data)

summary(model_race)
summary(model_interaction)

# summaries to csv
write.csv(broom::tidy(model_race), "model_race_summary.csv")
write.csv(broom::tidy(model_interaction), "model_interaction_summary.csv")

# Third regression analysis (***not used in poster***)
model <- lm(TurnoutRate ~ MedianIncomeScaled + Urban, data = data)
summary(model)

# __________________Regression visualizations: ___________

#MODEL 1
model_interaction <- lm(TurnoutRate ~ WhiteTurnout * Urban + BlackTurnout * Urban + 
                          HispanicTurnout * Urban + AsianTurnout * Urban + 
                          MedianIncomeScaled, data = data)

# Extract coefficients and confidence intervals
coef_data <- broom::tidy(model_interaction, conf.int = TRUE)

# Filter out intercept for visualization clarity
coef_data <- coef_data[coef_data$term != "(Intercept)", ]

# VISUAL:  Regression Coefficients with confidence intervals to see immportance of each var
coef_plot <- ggplot(coef_data, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "darkblue", width = 0.7) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "red") +
  coord_flip() +
  labs(
    title = "Regression Coefficients with Confidence Intervals",
    x = "Predictor Variables",
    y = "Coefficient Estimate"
  ) +
  theme_minimal()

ggsave("regression_coefficients_plot.png", coef_plot, width = 10, height = 6)

print(coef_plot)

# VISUAL: Scatter plot showing predicted vs actual turnout rates
# here we are adding the predicted values to the dataset:
data$predicted <- predict(model_interaction, newdata = data)

scatter_plot <- ggplot(data, aes(x = predicted, y = TurnoutRate)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Predicted vs. Actual Turnout Rates",
    x = "Predicted Turnout",
    y = "Actual Turnout"
  ) +
  theme_minimal()

ggsave("predicted_vs_actual_plot.png", scatter_plot, width = 8, height = 6)

print(scatter_plot)

# VISUAL: Turnout rates by county
tm_shape(map_data) +
  tm_polygons("TurnoutRate",
              palette = "Blues",
              title = "Voter Turnout Rate (%)") +
  tm_layout(title = "2020 Voter Turnout by County",
            legend.outside = TRUE,
            frame = FALSE,
            title.position = c("left", "top"),
            legend.position = c("right", "bottom"))


# VISUAL: Urban vs. Rural Turnout
tm_shape(map_data) +
  tm_facets("Urban", free.coords = FALSE) +
  tm_polygons("TurnoutRate",
              palette = "Reds",
              title = "Turnout Rate (%)") +
  tm_layout(
    title = "Urban vs. Rural Voter Turnout",
    legend.outside = TRUE,
    frame = FALSE,
    title.position = c("left", "top"),
    legend.position = c("right", "bottom")
  )


# maps to files
tmap_save(
  tm_shape(map_data) +
    tm_polygons("TurnoutRate", palette = "Blues", title = "Turnout Rate") +
    tm_layout(title = "Voter Turnout by County", legend.outside = TRUE),
  filename = "voter_turnout_map.png"

)
tmap_save(
  tm_shape(map_data) +
    tm_facets("Urban", free.coords = FALSE) +
    tm_polygons("TurnoutRate", palette = "Reds", title = "Turnout Rate by Urban/Rural") +
    tm_layout(title = "Urban vs. Rural Voter Turnout", legend.outside = TRUE),
  filename = "urban_rural_turnout_map.png"
)

# VISUAL: Turnout vs. Median Income (****not used in poster***)
ggplot(data, aes(x = MedianIncomeScaled, y = TurnoutRate, color = Urban)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", formula = 'y ~ x', se = FALSE, color = "black") +
  scale_color_manual(values = c("Urban" = "blue", "Rural" = "orange")) +
  labs(
    title = "Turnout Rate vs. Median Income",
    x = "Median Income (Scaled)",
    y = "Turnout Rate (%)",
    color = "Urbanization"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", legend.title.align = 0.5)

print(turnout_income_plot)
ggsave("turnout_income_plot.png", turnout_income_plot, width = 8.39, height = 5.69)


# VISUAL (***not used in poster) turnout by state chloropleth
state_map_data <- county_shapefile %>%
  left_join(data, by = "fips") %>%
  group_by(state_name) %>%
  summarise(AverageTurnout = mean(TurnoutRate, na.rm = TRUE))

tm_shape(state_map_data) +
  tm_polygons("AverageTurnout",
              palette = "Blues",
              title = "Average Turnout (%)") +
  tm_layout(
    title = "Voter Turnout by State (2020)",
    legend.outside = TRUE,
    frame = FALSE
  )



# ____________________________ Turnout by state____________________________
# ******the following were not incorporated into poster due to choosing conciseness over too many visuals since im focusing on county in my research question; 

#turnout by state
state_turnout <- data %>%
  group_by(state_name) %>%
  summarise(AverageTurnout = mean(TurnoutRate, na.rm = TRUE))

# Bar chart for average turnout by state
turnout_bar_chart <- ggplot(state_turnout, aes(x = reorder(state_name, -AverageTurnout), y = AverageTurnout)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  coord_flip() +
  labs(
    title = "Average Voter Turnout by State",
    x = "State",
    y = "Average Turnout Rate"
  ) +
  theme_minimal()
print(turnout_bar_chart)
ggsave("turnout_by_state_bar_chart.png", turnout_bar_chart, width = 10, height = 8)


# ____________________________ Turnout vs demographic vars ____________________________
#need to scale because side by side in poster and need scales to match on y axis from 0 to 100 percent [0,1]
common_scale <- scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

# URBAN AREAS
turnout_income_urban <- ggplot(data %>% filter(Urban == "Urban"), 
                               aes(x = MedianIncomeScaled, y = TurnoutRate)) +
  geom_point(alpha = 0.7, color = "darkblue") +
  geom_smooth(method = "lm", formula = 'y ~ x', se = FALSE, color = "red") +
  labs(
    title = "Urban Areas",
    x = "Median Income (Scaled)",
    y = "Turnout Rate"
  ) +
  common_scale +
  scale_x_continuous(limits = c(min(data$MedianIncomeScaled), max(data$MedianIncomeScaled))) +
  theme_minimal()

# RURAL AREA
turnout_income_rural <- ggplot(data %>% filter(Urban == "Rural"), 
                               aes(x = MedianIncomeScaled, y = TurnoutRate)) +
  geom_point(alpha = 0.7, color = "darkblue") +
  geom_smooth(method = "lm", formula = 'y ~ x', se = FALSE, color = "red") +
  labs(
    title = "Rural Areas",
    x = "Median Income (Scaled)",
    y = "Turnout Rate"
  ) +
  common_scale +
  scale_x_continuous(limits = c(min(data$MedianIncomeScaled), max(data$MedianIncomeScaled))) +
  theme_minimal()


ggsave("turnout_vs_income_urban.png", turnout_income_urban, width = 8, height = 6)
ggsave("turnout_vs_income_rural.png", turnout_income_rural, width = 8, height = 6)
print(turnout_income_urban)
print(turnout_income_rural)


