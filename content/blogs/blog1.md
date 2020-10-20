---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: ""
draft: false
image: pic10.jpg
keywords: ""
slug: ipsum
title: U.S. Elections
---


```{r challenge1, echo=FALSE, out.width="90%"}

knitr::include_graphics(here::here("images", "figure3.jpeg"), error = FALSE)

# Read csv file and import into dataframe
cdc_males <- read_csv("data/CDC_Males.csv")

# Calculate Spearman correlation coefficient
correl_cdc <- cdc_males %>% 
  filter(type.fac == "Firearm-related") %>% 
  drop_na(adjusted.suicide.White, adjusted.homicide.White) %>% 
  summarise(correl = cor(adjusted.suicide.White, adjusted.homicide.White, method = "spearman"))

# Create Scatterplot
cdc_males[!is.na(cdc_males$gun.house.prev.category),] %>% # get rid of all NAs in gun.house.prev.category
  filter(type.fac == "Firearm-related") %>% 
  ggplot(aes(x = adjusted.suicide.White, y = adjusted.homicide.White, colour = gun.house.prev.category, rm.na = TRUE)) +
  geom_point(aes(size = average.pop.white)) + # Make datapoint size dependent on average white population size
  scale_size_area(breaks = c(500000, 1500000, 3000000, 7000000), # Specify category breaks for datapoint size
                  labels = c('500k', '1.5m', '3m', '7m'), max_size = 14) +
  scale_color_manual(values = c("10.2%-19.9%" = "#fef0d9", # specify colours of range buckets
                                "20.0%-34.9%" = "#fdcc8a",
                                "35.0%-44.9%" = "#fc8d59",
                                "45.0%-65.5%" = "#d7301f")) +
  ggrepel::geom_text_repel(aes(label = ST), # Add state labels to datapoints
                  size = 5,
                  color = "Black") +
  annotate(x = 25, y = 0.5, # Add correlation to graph
         label = paste("Spearman Rho: ", round(correl_cdc, 2)), 
         geom="text", size=5) +
  theme_light() +
  labs(
    title = "Annual rates of firearm homicide and suicide among white men, by state, household firearm ownership",
    x = "White Suicide Rate (per 100,000 per Year)",
    y = "White Homicide Rate (per 100,000 per Year)",
    size = "White Population",
    colour = "Gun Ownership"
  )
```




```{r, echo=FALSE}
# Make sure you use vroom() as it is significantly faster than read.csv()
CA_contributors_2016 <- vroom::vroom(here::here("data","CA_contributors_2016.csv"))

zipcode <- vroom("http://www.uszipcodelist.com/zip_code_database.csv")

library(tidytext)

cleaned_state_data <- zipcode %>%
  select(zip, primary_city) %>%
  mutate_at("zip", as.numeric) # Make sure zip are numeric values

Contribution_plot <- left_join(CA_contributors_2016, cleaned_state_data, by = c("zip" = "zip")) %>%
  select(cand_nm, contb_receipt_amt, zip, primary_city) %>%
  filter(cand_nm %in% c("Trump, Donald J.", "Clinton, Hillary Rodham")) %>% # filter for zip codes where DT OR HC received contributions
  group_by(primary_city, cand_nm) %>%
  summarise(total_in_primary_city = sum(contb_receipt_amt)) %>%
  arrange(desc(total_in_primary_city)) %>%
  ungroup %>%
  group_by(cand_nm) %>%
  slice_head(n = 10) 

# Create column plot where we reorder cities for both candidates according to highest total amounts with reorder_within()
ggplot(Contribution_plot, aes(x = total_in_primary_city, y =  reorder_within(primary_city, total_in_primary_city, cand_nm), fill = cand_nm)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~cand_nm, scales = "free") +
  scale_y_reordered() + # clean the y-axis 
  scale_x_continuous(labels=scales::dollar_format()) + # clean the x-axis
  labs(
    title = "Where did candidates raise most money?",
    x = "Amount raised",
    y = element_blank()
  ) +
   theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
         strip.background = element_rect(color = "black", size = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_line(colour = "grey", size = 0.5),
        panel.grid.major = element_line(colour = "grey", size = 0.5)) +
  scale_fill_manual(values = c("Clinton, Hillary Rodham" = "#1C49C3",
                                "Trump, Donald J." = "#D80D22"))

```

