library(ggplot2)
library(dplyr)

tariff_data <- read.csv("WITS-USA_2022_tariffdata.csv", stringsAsFactors = FALSE)


tariff_data$HSCode <- sub(" -.*", "", tariff_data$Product)         # Extract HS code (the part before " - ")
tariff_data$Description <- sub("^[0-9]+ - ", "", tariff_data$Product)  # Extract description (after the code and hyphen)

tariff_data$Description <- gsub("\\s+", " ", tariff_data$Description)

tariff_data$MFNRate <- as.numeric(tariff_data$MFNRate)
tariff_data$AppliedTariff <- as.numeric(tariff_data$AppliedTariff)


tariff_data$HSChapter <- substr(tariff_data$HSCode, 1, 2)
tariff_data$Category <- recode(tariff_data$HSChapter,
                               "01" = "Live Animals",
                               "02" = "Meat & Edible Meat Offal",
                               "03" = "Fish & Seafood")

overall_avg_tariff <- mean(tariff_data$AppliedTariff, na.rm = TRUE)

category_avg <- tariff_data %>%
  group_by(Category) %>%
  summarize(AverageTariff = mean(AppliedTariff, na.rm = TRUE))

top10_products <- tariff_data %>%
  arrange(desc(AppliedTariff)) %>%
  slice(1:10)

top10_products <- top10_products %>%
  select(HSCode, Description, AppliedTariff, MFNRate, IsTraded)

cat("Top 10 products (HS code) with highest applied tariffs on imports from China to USA:\n")
print(top10_products[, c("HSCode", "Description", "AppliedTariff")])


ggplot(top10_products, aes(x = reorder(paste(HSCode, Description, sep=" - "), AppliedTariff), 
                           y = AppliedTariff)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  # flip coordinates for horizontal bars
  labs(title = "Top 10 Highest U.S. Tariffs on Imports from China (2022)",
       x = "Product (HS Code - Description)",
       y = "Applied Tariff Rate (%)") +
  theme_minimal()

category_avg_with_overall <- rbind(category_avg, 
                                   data.frame(Category = "Overall Average", AverageTariff = overall_avg_tariff))

cat("\nAverage Applied Tariff by Product Category vs Overall:\n")
print(category_avg_with_overall)

high_tariff_categories <- category_avg %>%
  filter(AverageTariff > overall_avg_tariff) %>%
  pull(Category)

