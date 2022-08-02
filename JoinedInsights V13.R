# Install and load packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, tidyverse, ggplot2, rmarkdown, lubridate,
               skimr, doParallel, patchwork, tidyquant, plotly, DataExplorer,
               rvest, RSelenium, netstat, stringr, tidyr, seleniumPipes,
               webshot, strex, tidygeocoder, sf, mapview,
               tigris, rnaturalearth, DataExplorer, correlationfunnel)

library(correlationfunnel)
library(DataExplorer)
options(scipen=999)
library(readxl)

pharmacists_joined_tbl = read_excel("Pharmacists_Joined_To_Pharmacies_2022_V13.xlsx", guess_max = 25000)

pharmacists_active_joined_tbl = pharmacists_joined_tbl %>% 
    filter(role == "Pharmacist") %>% 
    filter(part %in% c("Part A", "Part B")) %>% 
    filter(workplaces != "This pharmacy professional has not reported a workplace.") %>%
    drop_na(wpg2)

pharmacists_active_joined_tbl %>%
    ggplot(aes(wpg, fill = international_grad)) +
    geom_histogram(stat = "count") +
    coord_flip()

pharmacists_active_joined_tbl %>% select(international_grad, wpg) %>%  plot_correlation(maxcat = 100)

pharmacists_joined_tbl %>% 
    filter(role == "Pharmacist") %>% 
    filter(pharmacist_status == "Can provide patient care") %>% 
    filter(part %in% c("Part A", "Part B")) %>% 
    filter(workplaces != "This pharmacy professional has not reported a workplace.") %>%
    drop_na(wpg2) %>% 
    count(wpg2, international_grad) %>% 
    mutate(wpg2 = wpg2 %>% as_factor() %>% fct_reorder(n)) %>% 
    ggplot(aes(x = wpg2, y = n, fill = international_grad)) +
    geom_bar(stat = "identity") +
    labs(
    title = "Where Do Currently Registered Part A/B Pharmacists That Have Reported a Workplace On OCP Work?",
    x = "",
    y = "Count" 
    ) +
    coord_flip() +
    theme_tq()
    
##
# Graph of pharmacists vs wpg2

pharmacists_active_joined_tbl %>% 
    group_by(wpg2, international_grad, fsa_city) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    filter(fsa_city %in% c("Toronto", 'Old Toronto', "Brampton", "Missausauga", "Windsor", "Ottawa", "London", "Markham", "Etobicoke", "Barrie", "North York")) %>% 
    mutate(wpg2 = as_factor(wpg2) %>% fct_reorder(count)) %>% 
    arrange(-count) %>%
    ggplot(aes(x = wpg2, y = count, fill = international_grad)) + 
    geom_bar(stat = "identity") + 
    coord_flip() +
    facet_wrap(~ fsa_city) +
    labs(
        title = "Where Do Pharmacists Work in different FSA's Relative to International Grad Status?",

    )

corr1 = pharmacists_cies_tbl %>% 
    #mutate(international_grad = ifelse(international_grad == "Yes", 1, 0)) %>% 
    mutate(pharmacist_current_initial_registration_date = year(pharmacist_current_initial_registration_date) %>% as.numeric() )


#1.1 Correlation Analysis - pharmacies pharmacists----
#Investigating the relationships with avetinc
corr1 %>% 
    drop_na(international_grad_employees_employed_percentage) %>% 
    select(employees_employed, 
           international_grad_employees_employed, international_grad_employees_employed_percentage,
           wpg, part, gender, international_grad, stores_working_at) %>%
    binarize() %>% 
    correlate("international_grad__Yes") %>% 
    plot_correlation_funnel()

corr1 %>% ggplotly()


years_tbl = pharmacists_active_joined_tbl %>%
    filter(pharmacist_current_initial_registration_date > "2000-07-09", pharmacist_current_initial_registration_date < "2010-07-09")

years_tbl %>% 
    group_by(wpg, international_grad) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    mutate(wpg = as_factor(wpg) %>% fct_reorder(count)) %>% 
    arrange(-count) %>% 
    ggplot(aes(x = wpg, y = count, fill = international_grad)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
        title = "Where Do Pharmacists Registered With OCP Between 2000 and 2010 Work?",
        x = "",
        y = ""
    ) +
    theme_tq()

# Time 2010-2020

years_tbl = pharmacists_active_joined_tbl %>% 
    filter(pharmacist_current_initial_registration_date > "2010-07-09", pharmacist_current_initial_registration_date < "2020-07-09")

year_tbl = pharmacists_active_joined_tbl %>% 
    filter(education_site == "University of Toronto")

year_tbl %>% 
    group_by(wpg, international_grad) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    mutate(wpg = as_factor(wpg) %>% fct_reorder(count)) %>% 
    arrange(-count) %>% 
    ggplot(aes(x = wpg, y = count, fill = international_grad)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
        title = "Where Do University of Toronto Pharmacist Graduates Work?",
        x = "",
        y = ""
    ) +
    theme_tq()
