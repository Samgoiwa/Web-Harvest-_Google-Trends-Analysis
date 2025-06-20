# --------------------------------------------
# 📦 Install and Load Required Libraries
# --------------------------------------------
if (!require(gtrendsR)) install.packages("gtrendsR")#Comments from Djima
if (!require(tidyverse)) install.packages("tidyverse")

library(gtrendsR)
library(tidyverse)
library(dplyr)

# --------------------------------------------
# 🔍 Define Keywords and Parameters
# --------------------------------------------
keywords <- c("Gen Z", "finance bill", "Wantam", "killings")
geo <- "KE"  # Kenya
timeframe <- "2024-05-01 2025-06-17"  # Adjust as needed

# --------------------------------------------
# 📥 Fetch Google Trends Data
# --------------------------------------------
trend_data <- gtrends(keyword = keywords, geo = geo, time = timeframe)
trend_data

# --------------------------------------------
# 📈 Interest Over Time
# --------------------------------------------
time_data <- trend_data$interest_over_time %>%
  as_tibble() %>%
  select(date, keyword, hits) %>%
  mutate(hits = as.numeric(hits))

# --------------------------------------------
# 🖼️ Plot Time Series Trends
# --------------------------------------------
ggplot(time_data, aes(x = date, y = hits, color = keyword)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "📊 Google Trends in Kenya (May–June 2025)",
    subtitle = "Search Interest for Gen Z, Finance Bill, Wantam, and Killings",
    x = "Date",
    y = "Relative Search Interest (0–100)",
    color = "Search Term"
  ) +
  theme_minimal(base_size = 13)

# --------------------------------------------
# 🌍 Optional: Regional Search Interest Heatmap
# --------------------------------------------
# Extract and clean region data
region_data <- trend_data$interest_by_city %>%
  as_tibble() %>%
  mutate(location = as.character(location)) %>%
  filter(!is.na(hits)) %>%
  arrange(desc(hits)) %>%
  slice_max(order_by = hits, n = 10)

# Preview
print(region_data)

# Plot: Top 10 Regions with Search Term Interest
library(ggplot2)

ggplot(region_data, aes(x = reorder(location, hits), y = hits, fill = keyword)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  labs(
    title = "Top 10 Regions in Kenya by Google Search Interest",
    subtitle = paste("Keywords:", paste(unique(region_data$keyword), collapse = ", ")),
    x = "Region (County)",
    y = "Search Interest (0–100)",
    fill = "Search Term"
  ) +
  theme_minimal(base_size = 13)
