# This is for the Google data analytics capstone project Cyclistic bike share analysis # nolint

#This script uses tidyverse, ggplot2, lubridate, ggmap, geospehere, and dplyr packages #nolint
# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggmap)
library(geosphere)
library(dplyr)
library(scales)
library(conflicted)
# Resolve conflicts between packages
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("count", "dplyr")

# Set the visulaization theme
# Set visualization theme
theme_set(theme_minimal() +
            theme(plot.title = element_text(face = "bold", size = 16),
                  plot.subtitle = element_text(color = "gray40"),
                  legend.position = "top"))

# Load the data
q1_2019 <- read_csv("/home/sanjayan/Documents/coursera/capstone/capstone 1_cyclist/dataset/Divvy_Trips_2019_Q1/Divvy_Trips_2019_Q1.csv") # nolint: line_length_linter.
q1_2020 <- read_csv("/home/sanjayan/Documents/coursera/capstone/capstone 1_cyclist/dataset/Divvy_Trips_2020_Q1 (1)/Divvy_Trips_2020_Q1.csv") # nolint: line_length_linter.

#================================
# STEP 2: DATA WRANGLING
#================================
# Standardize column names across datasets

q1_2019_clean <- q1_2019 %>%
  rename(
    ride_id = trip_id,
    rideable_type = bikeid,
    started_at = start_time,
    ended_at = end_time,
    start_station_name = from_station_name,
    start_station_id = from_station_id,
    end_station_name = to_station_name,
    end_station_id = to_station_id,
    member_casual = usertype
  ) %>%
  mutate(across(c(ride_id, rideable_type), as.character))

#Combine datasets
all_trips <- bind_rows(q1_2019_clean, q1_2020) %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, tripduration)) #nolint

#================================
# STEP 3: DATA CLEANING
#================================
all_trips_clean <- all_trips %>%
  # Consolidate user types
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member",
                                "Customer" = "casual")) %>%
  # Calculate ride duration in minutes
  mutate(ride_length = as.numeric(difftime(ended_at, started_at, units = "mins"))) %>% #nolint
  # Add temporal features
  mutate(
    date = as.Date(started_at),
    month = month(started_at, label = TRUE, abbr = FALSE),
    day = day(started_at),
    year = year(started_at),
    day_of_week = wday(started_at, label = TRUE, abbr = FALSE, week_start = 1),
    hour = hour(started_at),
    ride_type = if_else(start_station_name == end_station_name, 
                        "Round Trip", "Point-to-Point"),
    season = case_when(
      month %in% c("December", "January", "February") ~ "Winter",
      month %in% c("March", "April", "May") ~ "Spring",
      month %in% c("June", "July", "August") ~ "Summer",
      TRUE ~ "Fall"
    )
  ) %>%
  # Filter invalid rides
  filter(
    ride_length > 1 & ride_length < 1440,  # 1 min < rides < 24 hrs
    !is.na(start_station_name)             # Remove rides without station info
  )

# Data quality report
cat("Data Quality Report:\n")
cat("Original rows:", nrow(all_trips), "\n")
cat("Cleaned rows:", nrow(all_trips_clean), "\n")
cat("Rows removed:", nrow(all_trips) - nrow(all_trips_clean), "\n")
cat("Casual riders:", sum(all_trips_clean$member_casual == "casual"), "\n")
cat("Member riders:", sum(all_trips_clean$member_casual == "member"), "\n")

#================================
# STEP 4: ANALYSIS & VISUALIZATION
#================================
# Business Question : How do annual members and casual riders use bikes differently? 

# Ride Duration Analysis
duration_summary <- all_trips_clean %>%
  group_by(member_casual, day_of_week) %>%
  summarise(avg_duration = mean(ride_length),
            median_duration = median(ride_length),
            .groups = 'drop')


# Visualization: Ride Duration Patterns
ggplot(duration_summary, aes(day_of_week, avg_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Ride Duration by User Type", 
       subtitle = "Casual riders take significantly longer trips, especially on weekends", #nolint
       x = "", y = "Duration (minutes)", fill = "User Type") +
  scale_fill_manual(values = c("#FF6F61", "#6A5ACD")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("avg_ride_duration_by_user_type.png", width = 10, height = 6, dpi = 300)


# Ride Frequency Analysis
frequency_summary <- all_trips_clean %>%
  count(member_casual, day_of_week, name = "ride_count") %>%
  group_by(member_casual) %>%
  mutate(percentage = ride_count / sum(ride_count) * 100)



# Visualization: Weekly Usage Patterns
ggplot(frequency_summary, aes(day_of_week, ride_count, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Rides per Weekday", 
       subtitle = "Members use bikes consistently while casual riders peak on weekends", #nolint
       x = "", y = "Number of Rides", fill = "User Type") +
  scale_y_continuous(labels = comma_format()) +
  scale_fill_manual(values = c("#FF6F61", "#6A5ACD"))
ggsave("rides_per_weekday.png", width = 10, height = 6, dpi = 300)



# Time-of-Day Analysis
hourly_summary <- all_trips_clean %>%
  count(member_casual, hour, name = "ride_count") %>%
  group_by(member_casual) %>%
  mutate(percentage = ride_count / sum(ride_count) * 100)



# Visualization: Hourly Usage Patterns
ggplot(hourly_summary, aes(hour, ride_count, color = member_casual)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Hourly Usage Patterns",
       subtitle = "Members show strong commute peaks (8-9am, 5-6pm)",
       x = "Hour of Day", y = "Number of Rides", color = "User Type") +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  scale_color_manual(values = c("#FF6F61", "#6A5ACD")) +
  geom_vline(xintercept = c(8, 17), linetype = "dashed", alpha = 0.5)
ggsave("hourly_usage_patterns.png", width = 10, height = 6, dpi = 300)



# Seasonal Analysis
seasonal_summary <- all_trips_clean %>%
  count(member_casual, season, month, name = "ride_count") %>%
  group_by(member_casual) %>%
  mutate(percentage = ride_count / sum(ride_count) * 100)



# Visualization: Seasonal Usage Trends
ggplot(seasonal_summary, aes(month, ride_count, group = member_casual, color = member_casual)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Seasonal Usage Trends",
       subtitle = "Casual ridership surges in warmer months - prime conversion period",
       x = "", y = "Number of Rides", color = "User Type") +
  scale_y_continuous(labels = comma_format()) +
  scale_color_manual(values = c("#FF6F61", "#6A5ACD")) +
  facet_wrap(~season, scales = "free_x", nrow = 1)
ggsave("seasonal_usage_trends.png", width = 10, height = 6, dpi = 300)


# Business Question : How can digital media influence casual riders?

# Top Stations Analysis
top_stations <- all_trips_clean %>%
  filter(member_casual == "casual") %>%
  count(start_station_name, sort = TRUE) %>%
  drop_na() %>%
  head(10) %>%
  mutate(start_station_name = fct_reorder(start_station_name, n))

# Visualization: Top Casual Rider Stations
ggplot(top_stations, aes(n, start_station_name)) +
  geom_col(fill = "#59A14F") +
  labs(title = "Top 10 Stations for Casual Riders",
       subtitle = "Ideal locations for targeted digital marketing",
       x = "Number of Rides", y = "") +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5)
ggsave("top_stations_casual_riders.png", width = 10, height = 6, dpi = 300)

# Peak Usage Times for Casual Riders
casual_peak <- all_trips_clean %>%
  filter(member_casual == "casual") %>%
  count(day_of_week, hour) %>%
  group_by(day_of_week) %>%
  mutate(peak = n == max(n))

# Visualization: Peak Usage Times
ggplot(casual_peak, aes(hour, day_of_week, fill = n)) +
  geom_tile() +
  geom_point(data = filter(casual_peak, peak), shape = 8, size = 2, color = "gold") +
  scale_fill_gradient(low = "#EDF8FB", high = "#2CA25F") +
  labs(title = "Peak Usage Times for Casual Riders",
       subtitle = "Gold stars indicate peak usage times for each weekday",
       x = "Hour of Day", y = "", fill = "Number of Rides") +
  scale_x_continuous(breaks = seq(0, 23, by = 3))
ggsave("peak_usage_times_casual_riders.png", width = 10, height = 6, dpi = 300)

# Membership Conversion Funnel
conversion_funnel <- tibble(
  stage = c("All Casual Riders", "Regular Users (4+ rides)", "Frequent Users (8+ rides)", "High Savings Potential"),
  count = c(
    n_distinct(filter(all_trips_clean, member_casual == "casual")$ride_id),
    nrow(filter(cost_analysis, total_rides >= 4)),
    nrow(filter(cost_analysis, total_rides >= 8)),
    nrow(filter(cost_analysis, savings > 100))
  )
)

# Visualization: Conversion Funnel
ggplot(conversion_funnel, aes(stage, count, group = 1)) +
  geom_line(color = "#4E79A7", size = 1.5) +
  geom_point(size = 4, color = "#F28E2B") +
  geom_text(aes(label = scales::comma(count)), vjust = -1) +
  labs(title = "Membership Conversion Funnel",
       subtitle = "Identifying high-potential conversion targets among casual riders",
       x = "Conversion Stage", y = "Number of Riders") +
  scale_y_continuous(labels = comma_format(), limits = c(0, max(conversion_funnel$count) * 1.1))
ggsave("membership_conversion_funnel.png", width = 10, height = 6, dpi = 300)

#================================
# STEP 5: EXPORT RESULTS
#================================

# Save data summaries
write_csv(duration_summary, "ride_duration_summary.csv")
write_csv(frequency_summary, "ride_frequency_summary.csv")
write_csv(hourly_summary, "hourly_usage_summary.csv")
write_csv(seasonal_summary, "seasonal_usage_summary.csv")
# Save detailed analysis results
write_csv(all_trips_clean, "all_trips_cleaned.csv")
write_csv(top_stations, "top_casual_stations.csv")

# Save combined analysis results
analysis_results <- list(
  ride_duration = duration_summary,
  ride_frequency = frequency_summary,
  top_stations = top_stations
)
# Save as RDS for future use
saveRDS(analysis_results, "full_analysis_results.rds")

# Final report
cat("\nANALYSIS COMPLETE\n")
cat("=================\n")
cat("Key Findings:\n")
cat("- Casual riders take ",
    round(mean(filter(duration_summary, member_casual == "casual")$avg_duration)),
    " min rides on average vs ",
    round(mean(filter(duration_summary, member_casual == "member")$avg_duration)),
    " min for members\n")
cat("- Casual weekend ridership is ",
    round(max(filter(frequency_summary, member_casual == "casual")$ride_count) /
          min(filter(frequency_summary, member_casual == "casual")$ride_count)),
    " times higher than weekday ridership\n")
cat("- Top conversion opportunity: ",
    as.character(top_stations$start_station_name[1]),
    " station with ", top_stations$n[1], " casual rides\n")
cat("\nResults saved to working directory\n")
