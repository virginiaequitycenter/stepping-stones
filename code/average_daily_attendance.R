# Stepping Stones Data: Average Daily Attendance ----
# Updated 2023-02-11
# Contributor: Michele Claibourn
# Acquire data from Virginia DOE Superintendent's Annual Report
# Charlottesville, Albemarle, State
#
# Proposed Citation
# Virginia Department of Education, Superintendent's Annual Report, "Table 8: Number of Days Taught, ADA, ADM ." 2002-2022.
# https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/superintendent-s-annual-report


# Libraries ----
library(tidyverse)
library(here)
library(readxl)
library(janitor)


# Create vector of urls ----

urls <- c(
  "https://www.doe.virginia.gov/home/showpublisheddocument/19287/638042787714630000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/19301/638042791091270000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/19301/638042791091270000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/19323/638042793435930000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/19383/638042794375570000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/19437/638042794787700000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/40544/638078245181830000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/40518/638078230701830000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/40492/638078149207470000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/40464/638078131810500000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/40434/638078110489830000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/40410/638078103453270000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/40382/638078096076070000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/40352/638078084871730000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/40322/638078065679100000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/40292/638078034617800000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/40260/638077510203670000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/40232/638077479514300000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/40200/638077393061230000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/40164/638077357214370000",
  "https://www.doe.virginia.gov/home/showpublisheddocument/40114/638077315815600000"
  )

# create vector of destination file names ----
dest <- paste0("datadownloads/ada/", "table8_", c(2022:2002), rep(c(".xlsx", ".xls"), c(9, 12)))

# download files ----
if (!dir.exists(here("datadownloads/ada"))) 
  {dir.create(here("datadownloads/ada"))}

walk2(urls, dest, download.file, method = "curl", extra = "-k")


# read in files ----
test <- read_excel(dest[10], sheet = 1, skip = 6)

ada_data <- map(dest, ~read_excel(.x, sheet = 1, skip = 6, n_max = 136))
names(ada_data) <- 2022:2002 # add year as names for list

ada_data <- map(ada_data, ~ select(.x, 1:13)) # reduce dfs that read in with extra columns

# rename columns to be identical across years
colnames = c("division_number", "division_name", "days_taught_elem",
             "days_taught_sec", "adm_elem", "adm_sec", "adm_total", 
             "ada_elem", "ada_sec", "ada_total", "pct_att_elem", 
             "pct_at_sec", "pct_att_total") 

ada_data <- map(ada_data, ~ rename_with(., ~ colnames))

# change everything into character for consistency across dfs
ada_data <- map(ada_data, ~mutate_all(., as.character))

# bind into data frame
ada_all <- bind_rows(ada_data, .id = "year")


# Pull cville, alb data ----
ada <- ada_all %>% 
  filter(division_name %in% c("Albemarle County", "Charlottesville City", "State Totals")) %>% 
  select(year, division_number, division_name, adm_total, ada_total, pct_att_total) %>% 
  mutate(year = as.numeric(year),
         adm_total = as.numeric(adm_total),
         ada_total = as.numeric(ada_total),
         pct_att_total = as.numeric(pct_att_total),
         name = str_remove(division_name, "County|City|Totals"),
         name = str_trim(name),
         name = ifelse(name == "State", "Virginia", name))

# recreate percent as check
ada <- ada %>% 
  mutate(percent = (ada_total/adm_total)*100)

# have a peek
ggplot(ada, aes(x = year, y = percent, color = name)) +
  geom_line()


# save data ----
write_csv(ada, "data/average_daily_attendance.csv")
