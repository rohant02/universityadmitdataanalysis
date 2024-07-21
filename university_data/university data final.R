library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(corrplot)
library(maps)
library(ggmap)
library(zipcodeR)
library(dplyr)
library(purrr)
library(zipcodeR)
library(ggplot2)
library(maps)
library(stargazer)

data <- read.csv('Kmeans_assignment_data.csv')

# View the structure of the data-set
str(data)

summary(data)

colSums(is.na(data))


# Create new columns for acceptance rate and enrollment rate
data <- data %>%
  mutate(AcceptanceRate = Accept / Apps * 100,
         EnrollmentRate = Enroll / Accept * 100)
# Creating a new column for type of institution.
data <- data %>%
  mutate(Type_of_institution = ifelse(Private == "Yes", "Private", "Public"))

#One university has 48k applications which is way more than normal so we are excluding it
filtered_data <- data %>% filter(Apps <= 40000)


p1 <- ggplot(filtered_data, aes(x = Apps)) + 
  geom_histogram(binwidth = 500, fill = "blue", color = "black") + 
  labs(title = "Distribution of Applications", x = "Applications", y = "Frequency")

p2 <- ggplot(data, aes(x = Accept)) + 
  geom_histogram(binwidth = 500, fill = "green", color = "black") + 
  labs(title = "Distribution of Acceptances", x = "Acceptances", y = "Frequency")

p3 <- ggplot(data, aes(x = Enroll)) + 
  geom_histogram(binwidth = 200, fill = "red", color = "black") + 
  labs(title = "Distribution of Enrollments", x = "Enrollments", y = "Frequency")

p4 <- ggplot(data, aes(x = AcceptanceRate)) + 
  geom_histogram(binwidth = 5, fill = "purple", color = "black") + 
  labs(title = "Distribution of Acceptance Rate", x = "Acceptance Rate (%)", y = "Frequency")

p5 <- ggplot(data, aes(x = EnrollmentRate)) + 
  geom_histogram(binwidth = 5, fill = "orange", color = "black") + 
  labs(title = "Distribution of Enrollment Rate", x = "Enrollment Rate (%)", y = "Frequency")

p6 <- ggplot(data, aes(x = Grad.Rate)) + 
  geom_histogram(binwidth = 5, fill = "cyan", color = "black") + 
  labs(title = "Distribution of Graduation Rate", x = "Graduation Rate (%)", y = "Frequency")


grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)


postcode <- read.csv("usn/USNews-College-Scraper/data.csv")

# Select relevant columns from postcode data
postcodes1 <- postcode %>%
  select(institution.displayName, institution.zip)

# Merge data with postcode data
merged_data <- merge(data, postcodes1, by.x = "X", by.y = "institution.displayName", all.x = TRUE)

filtered_data <- merged_data %>%
  filter(!is.na(institution.zip))

# Define a function to check if a ZIP code is valid using tryCatch
is_valid_zip <- function(zip) {
  tryCatch({
    !is.null(zipcodeR::geocode_zip(zip))
  }, error = function(e) {
    FALSE
  })
}

# Filter out invalid ZIP codes
valid_data <- filtered_data %>%
  filter(sapply(institution.zip, is_valid_zip))

# Geocode the valid zip codes using zipcodeR
geocoded_data <- valid_data %>%
  mutate(geocode = map(institution.zip, ~zipcodeR::geocode_zip(.x))) %>%
  unnest(cols = c(geocode))

# Filter out invalid coordinates (latitude and longitude ranges for the contiguous USA)
geocoded_data <- geocoded_data %>%
  filter(lat >= 24 & lat <= 49, lng >= -125 & lng <= -66)

# View the geocoded data
head(geocoded_data)

# Get the map of the USA
usa_map <- map_data("state")

# Plot the universities on the map
ggplot() +
  geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), fill = "lightgrey", color = "white") +
  geom_point(data = geocoded_data, aes(x = lng, y = lat), color = "red", size = 2, alpha = 0.7) +
  labs(title = "Universities in the USA", x = "Longitude", y = "Latitude") +
  theme_minimal()



p7 <- ggplot(filtered_data, aes(x = Type_of_institution, y = Apps)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Applications by Institution Type", x = "Institution Type", y = "Applications")
print(p7)


p8 <- ggplot(data, aes(x = Type_of_institution, y = AcceptanceRate)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Acceptances by Institution Type", x = "Institution Type", y = "Acceptance Rate")

print(p8)

p9 <- ggplot(data, aes(x = Type_of_institution, y = Enroll)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Enrollments by Institution Type", x = "Institution Type", y = "Enrollments")


print(p9)

p10 <- ggplot(data, aes(x = Top10perc, y = AcceptanceRate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Acceptance Rate vs. Top 10% High School Class Percentage", x = "Top 10% High School Class (%)", y = "Acceptance Rate (%)")

print(p10)


p11 <- ggplot(data, aes(x = Top25perc, y = AcceptanceRate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Acceptance Rate vs. Top 25% High School Class Percentage", x = "Top 25% High School Class (%)", y = "Acceptance Rate (%)")

print(p11)


p12 <- ggplot(data, aes(x = S.F.Ratio, y = Grad.Rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(title = "Graduation Rate vs. Student-to-Faculty Ratio", x = "Student-to-Faculty Ratio", y = "Graduation Rate (%)")

print(p12)

# Select only the numerical columns for the correlation matrix
numerical_data <- data %>%
  select(Apps, Accept, Enroll, Top10perc, Top25perc, F.Undergrad, P.Undergrad,
         Outstate, Room.Board, Books, Personal, PhD, Terminal, S.F.Ratio,
         perc.alumni, Expend, Grad.Rate, AcceptanceRate, EnrollmentRate)

# Calculate the correlation matrix
correlation_matrix <- cor(numerical_data)

# Create a heat map of the correlation matrix

corrplot(correlation_matrix, method = "color", type = "lower", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7, 
         col = colorRampPalette(c("blue", "white", "red"))(200))

# Calculate summary statistics for alumni donation rates
summary_stats <- data %>%
  group_by(Type_of_institution) %>%
  summarise(
    mean_donation_rate = mean(perc.alumni, na.rm = TRUE),
    median_donation_rate = median(perc.alumni, na.rm = TRUE),
    sd_donation_rate = sd(perc.alumni, na.rm = TRUE)
  )

print(summary_stats)

# Visualize the comparison of alumni donation rates between private and public institutions
p <- ggplot(data, aes(x = Type_of_institution, y = perc.alumni, fill = Type_of_institution)) +
  geom_boxplot() +
  labs(title = "Alumni Donation Rates by Institution Type",
       x = "Type of Institution",
       y = "Alumni Donation Rate (%)") +
  scale_fill_manual(values = c("Private" = "skyblue", "Public" = "lightgreen"))

print(p)


p <- ggplot(filtered_data, aes(x = Expend, y = Grad.Rate, color = Type_of_institution)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Expenditure vs. Graduation Rate by Institution Type",
       x = "Expenditure per Student ($)",
       y = "Graduation Rate (%)") +
  theme_minimal()

print(p)

# Separate data for public and private institutions
public_data <- filtered_data %>% filter(Type_of_institution == "Public")
private_data <- filtered_data %>% filter(Type_of_institution == "Private")

# Linear regression model for public institutions
public_model <- lm(Grad.Rate ~ Expend, data = public_data)
summary(public_model)

# Linear regression model for private institutions
private_model <- lm(Grad.Rate ~ Expend, data = private_data)
summary(private_model)

# Comparing the models
stargazer(public_model, private_model, type = "text", 
          title = "Regression Results: Graduation Rate vs. Expenditure",
          column.labels = c("Public Institutions", "Private Institutions"),
          covariate.labels = c("Expenditure per Student ($)"),
          dep.var.labels = "Graduation Rate (%)",
          no.space = TRUE)