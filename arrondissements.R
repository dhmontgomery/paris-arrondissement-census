library(tidyverse)
library(rvest)

# Start with a list of all our URLs; other than "1er" they all follow a common pattern
arrondissements <- imap(c("https://fr.wikipedia.org/wiki/1er_arrondissement_de_Paris", 
						  paste0("https://fr.wikipedia.org/wiki/", 2:20, "e_arrondissement_de_Paris")),
	~read_html(.) %>% # Read in the URLs
		html_nodes("table:not(.infobox_v2)") %>% # Filter to just the HTML tables, but exclude the top infobox
		keep(.p = function(x) str_detect(x, "Population|Date")) %>% # Filter to just the tables we want
		# The 15th Arrondissement's table is formatted differently, hence "Date" as an alternate search term
		html_table(fill = TRUE) %>% # Read the tables as data frames
		purrr::pluck(1) %>% # Select the data elements 
		select(1:3) %>% # Remove extraneous columns
		set_names("year", "population", "density_km2") %>% # Assign clean column names
		mutate(across(everything(), ~str_remove_all(., "\\s") %>% parse_number())) %>% # Convert our data to numeric format
		mutate(arrondissement = .y)) %>% # Label each table by arrondissement
	reduce(bind_rows) %>% # Combine all the tables into one
	mutate(density_km2 = case_when(arrondissement == 15 ~ NA_real_, TRUE ~ density_km2)) # Remove bad density data from the 15th
write_csv(arrondissements, "arrondissements_pop.csv") # Save to disk