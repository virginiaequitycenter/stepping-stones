# Stepping Stones Data: Divorces ----
# Updated 2023-02-03
# Contributor: Michele Claibourn
# Acquire data from VDH Health Stats site: Recorded Divorces
# Charlottesville, Albemarle, State
#
# Proposed Citation
# Virginia Department of Health, Division of Health Statistics, "Recorded Divorces by City/County of Occurrence." 2000-2018.
# https://apps.vdh.virginia.gov/HealthStats/stats.htm


# Libraries ----
library(tidyverse)
library(janitor)
library(tabulizer)


# Read in a single pdf report ----
## Infant mortality ----
pdf18 <- "https://apps.vdh.virginia.gov/HealthStats/documents/pdf/DivCC18.pdf"
# define area to read
# pdfarea <- locate_areas(pdf18, 1) # selected manually
pdfarea <- list(setNames(c(72, 84, 760, 530), 
                         c("top", "left", "bottom", "right")))
list18 <- extract_tables(pdf18, area = pdfarea, pages = 1,
                         guess = FALSE, output = "data.frame")

df18 <- list18[[1]]
df18 <- df18 %>% 
  clean_names() %>% 
  select(locality = city_county, divorces = total) %>% 
  filter(locality %in% c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY", "COMMONWEALTH OF VIRGINIA")) %>% 
  mutate(year = 2018, 
         locality = str_to_title(locality),
         divorces = str_remove(divorces, ","),
         divorces = as.numeric(divorces))

# Make a function ----
# set pdfarea
# extract_appvdh: pull state, locality values from app vdh pdf
# arguments: pdfurl, pdfarea, year, localities, 
extract_appvdh <- function(pdfurl, year, localities) {
 
  list <- extract_tables(pdfurl, area = pdfarea, pages = 1,
                           guess = FALSE, output = "data.frame")
  
  dflist <- list[[1]]

  df <- dflist %>% 
    clean_names() %>% 
    select(locality = city_county, divorces = total) %>% 
    filter(locality %in% c(localities, "COMMONWEALTH OF VIRGINIA")) %>% 
    mutate(year = year, 
           locality = str_to_title(locality),
           divorces = str_remove(divorces, ","),
           divorces = as.numeric(divorces))
  
  return(df)
}

# Apply function ----
## make a list of years, pdfs ----
years <- tibble(y = seq(0,18,1)) %>% 
  mutate(y = as.character(y),
         y = str_pad(y, width = 2, side = "left", pad = "0"))

pdflista <- paste0("https://apps.vdh.virginia.gov/HealthStats/documents/pdf/DivCC",years$y[15:19],".pdf")
pdflistb <- paste0("https://apps.vdh.virginia.gov/HealthStats/documents/2010/pdfs/DivCC",years$y[1:14],".pdf")

pdflist <- c(pdflistb, 
             pdflista)

pdf_year <- tibble(pdf = pdflist, year = seq(2000,2018,1))

## set pdfarea ----
# pdfarea <- locate_areas(pdflis[2], 1) # selected manually
pdfarea <- list(setNames(c(72, 84, 765, 530), 
                         c("top", "left", "bottom", "right")))

## apply to list ----
div_list <- map2(pdf_year$pdf, pdf_year$year, 
                     ~extract_appvdh(.x, .y, 
                                     localities = c("ALBEMARLE COUNTY", "CHARLOTTESVILLE CITY"))) 

divorces <- bind_rows(div_list)


# Bring in population data ----
# using wcc pop data
pop <- read_csv("data/pop_data_wcc.csv")
divorces <- divorces %>% 
  mutate(locality = str_remove(locality, "Commonwealth Of "))

divorces_pop <- divorces %>% 
  left_join(pop)

divorces_pop <- divorces_pop %>% 
  mutate(divorce_rate = (divorces/pop_wcc)*1000)

# have a peek
ggplot(divorces_pop, aes(x = year, y = divorce_rate, color = locality)) +
  geom_line()


# Save data ----
write_csv(divorces_pop, "data/divorce_vdh.csv")
