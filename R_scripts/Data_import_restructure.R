# Libraries
library(dplyr)
library(tidyr)

# Set wd
setwd("C:/Users/steven.guidos/OneDrive - NINA/Desktop/R/Red_fox_diet")

# Import red fox metabarcoding data
fox_diet <- read.delim(
  "Data/rodrev_combined_plant_vert_negcorrected_Vvulpes_only_12102026.txt",
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

#Check out the structure
head(fox_diet)
summary(fox_diet)

# As of now the data is in long format, which makes it difficult to get an overview
# of which samples have which species, we should convert to wide format for preliminary
# analysis

# Define taxonomic columns, including ecological categories
taxa_cols <- c("final_phylum","final_class", "final_order", "final_family", "final_genus",
  "final_species", "category", "possible_dogfood")

# Convert to wide format
fox_diet_wide <- fox_diet %>%
  group_by(marker, across(all_of(taxa_cols)), Sample_ID) %>%
  summarise(
    amount = sum(amount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Sample_ID,
    values_from = amount,
    values_fill = 0
  ) %>%
  relocate(
    category,
    possible_dogfood,
    .after = final_species
  )

# Create a new df which will show which samples have important metadata and which are lacking

# Create sample metadata overview
sample_overview <- fox_diet %>%
  select(
    Sample_ID,
    sample_source,
    Sample_date,
    Latitude,
    Longitude) %>%
  distinct() %>%
  mutate(
    has_source = !is.na(sample_source) & trimws(sample_source) != "",
    has_date = !is.na(Sample_date) & trimws(Sample_date) != "",
    has_latitude = !is.na(Latitude),
    has_longitude = !is.na(Longitude),
    has_coordinates = has_latitude & has_longitude,
    complete_metadata = has_source & has_date & has_coordinates) %>%
  arrange(Sample_ID)

# All of the samples from the arctic fox monitoring program are missing lat and lon.
# and some of these samples are even missing date. 





