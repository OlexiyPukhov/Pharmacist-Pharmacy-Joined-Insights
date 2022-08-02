if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, tidyverse, ggplot2, rmarkdown, lubridate,
               skimr, doParallel, patchwork, tidyquant, plotly, DataExplorer,
               rvest, RSelenium, netstat, stringr, tidyr, seleniumPipes,
               webshot, strex, GGally, trelliscopejs, tidygeocoder, mapview, sf, optbin, data.table, openxlsx)

library(rio)
options(scipen=999)

pharmacies_tbl = import("OCP_Pharmacies_Completed_2022_V12.rds")
org_pharmacies_tbl = pharmacies_tbl

pharmacists_tbl = import("OCP_Pharmacists_2022_V12.rds")
org_pharmacists_tbl = pharmacists_tbl

org_pharmacists_tbl = pharmacists_tbl

pharmacists_tbl = pharmacists_tbl %>% 
    separate_rows(pharmacy_locations_primary_keys_url, sep = "\\,")

pharmacists_tbl_joined_1 = pharmacists_tbl %>% 
    left_join(pharmacies_tbl, by = c("pharmacy_locations_primary_keys_url" = "pharmacy_url"), keep = TRUE)

pharmacies_tbl = import("OCP_Pharmacies_Completed_2022_V12.rds")
org_pharmacies_tbl = pharmacies_tbl

pharmacies_tbl = pharmacies_tbl %>% 
    separate_rows(pharmacy_staffs_primary_keys_url, sep = "\\,")

pharmacists_tbl = import("OCP_Pharmacists_2022_V12.rds")
org_pharmacists_tbl = pharmacists_tbl

pharmacists_tbl_joined_2 = pharmacists_tbl %>% 
    left_join(pharmacies_tbl, by = c("pharmacist_url" = "pharmacy_staffs_primary_keys_url"), keep = TRUE)

pharmacists_tbl_joined_3 = pharmacists_tbl_joined_2 %>% 
    rbind(pharmacists_tbl_joined_1) 

pharmacists_tbl_joined_3 = pharmacists_tbl_joined_3 %>% 
    select(-pharmacist_url, -pharmacy_url, -pharmacy_staffs_primary_keys_url, -pharmacy_locations_primary_keys_url)
pharmacists_tbl_joined_3 = pharmacists_tbl_joined_3 %>% distinct()

pharmacists_tbl_joined = pharmacists_tbl_joined_3

index_to_remove = vector(mode = 'list')

for (i in 1:nrow(pharmacists_tbl_joined)) {
    
    searched_pharmacist = pharmacists_tbl_joined %>% 
        filter(registration_number == pharmacists_tbl_joined$registration_number[i])
    
    is_there_na = searched_pharmacist %>%
        select(name.y) %>% 
        is.na() %>% 
        any()
    
    is_there_only_na = searched_pharmacist %>%
        select(name.y) %>% 
        is.na() %>% 
        all()
    
    if (is_there_na & !is_there_only_na & is.na(pharmacists_tbl_joined$name.y[i])) {
        index_to_remove = append(index_to_remove, i)
    }
    
    print(str_glue("Pharmacist index {i}"))
    
}


pharmacists_tbl_joined = pharmacists_tbl_joined %>% 
    filter(!row_number() %in% index_to_remove)

write_rds(pharmacists_tbl_joined, "pending2.rds")

pharmacists_tbl_joined = import("pending2.rds")


pharmacists_tbl_joined = pharmacists_tbl_joined %>% 
    rename(pharmacy_worksite = name.y,
           pharmacy_status = status.y)

 pharmacists_tbl_joined = pharmacists_tbl_joined %>% 
     mutate(wpg = ifelse(((
         str_detect(workplaces, "This pharmacy professional has not reported a workplace.") |
             str_detect(workplaces, "This person is not entitled to practice and does not have a workplace.")
     )),
     yes = ifelse(is.na(pharmacy_worksite), "No workplaces", wpg), 
     no = wpg))
 
 pharmacists_tbl_joined = pharmacists_tbl_joined %>% 
      mutate(wpg2 = ifelse(((
          str_detect(wpg, "No workplaces")
      )),
      yes = "No workplaces", no = wpg2))
 
 pharmacists_tbl_joined = pharmacists_tbl_joined %>% 
     mutate(wpg3 = ifelse(((
         str_detect(wpg2, "No workplaces")
     )),
     yes = "No workplaces", no = wpg3))
 

clinical_non_clinical_sites1 = pharmacists_tbl_joined %>%
    group_by(registration_number, wpg3) %>% 
    tally() %>% 
    ungroup() %>% 
    mutate(clinical_sites_working_at = ifelse(wpg3 == "Clinical", n, 0)) %>% 
    group_by(registration_number) %>%
    summarize(clinical_sites_working_at = ifelse(!is.na(max(clinical_sites_working_at)), max(clinical_sites_working_at), 0)) %>% 
    ungroup()
    
clinical_non_clinical_sites2 = pharmacists_tbl_joined %>%
    group_by(registration_number, wpg3) %>% 
    tally() %>% 
    mutate(non_clinical_sites_working_at = ifelse(wpg3 == "Non-clinical", n, 0)) %>% 
    ungroup() %>% 
    group_by(registration_number) %>%
    summarize(non_clinical_sites_working_at = ifelse(!is.na(max(non_clinical_sites_working_at)), max(non_clinical_sites_working_at), 0)) %>% 
    ungroup()

pharmacists_tbl_joined = pharmacists_tbl_joined %>% 
    left_join(clinical_non_clinical_sites1, by = c("registration_number" = "registration_number")) %>% 
    left_join(clinical_non_clinical_sites2, by = c("registration_number" = "registration_number"))

org_pharmacists_tbl = org_pharmacists_tbl %>% 
    left_join(clinical_non_clinical_sites1, by = c("registration_number" = "registration_number")) %>% 
    left_join(clinical_non_clinical_sites2, by = c("registration_number" = "registration_number"))

pharmacists_tbl_joined = pharmacists_tbl_joined %>% 
    rename(pharmacist_name = name.x,
           pharmacist_status = status.x)

pharmacists_tbl_joined = pharmacists_tbl_joined %>% 
    mutate(works_both_clinical_and_non = ifelse(clinical_sites_working_at > 0 & non_clinical_sites_working_at > 0, 1, 0))

pharmacists_tbl_joined = pharmacists_tbl_joined %>% 
    mutate(fte_per_store = ifelse(stores_working_at != 0, 1/stores_working_at, 0))

pharmacists_tbl_joined = pharmacists_tbl_joined %>% 
    mutate(fte_per_store = ifelse(works_both_clinical_and_non == 1 & wpg3 == "Clinical", 0.1*fte_per_store, fte_per_store)) %>% 
    mutate(fte_per_store = ifelse(works_both_clinical_and_non == 1 & wpg3 == "Non-clinical", 0.9*fte_per_store, fte_per_store))

pharmacists_tbl_joined = pharmacists_tbl_joined %>% 
    group_by(accreditation_number) %>% 
    mutate(fte_store_total = sum(fte_per_store)) %>% 
    ungroup()

fsa_city_tbl = import("FSA-City.xlsx")

fsa_city_tbl = fsa_city_tbl %>% 
    rename(fsa_city = "Place Name") %>% 
    rename(fsa = "FSA Code")

pharmacists_tbl_joined = pharmacists_tbl_joined %>% 
    mutate(postal_code = str_trim(postal_code, side = "both")) %>% 
    mutate(fsa = str_before_first(postal_code, " ")) %>% 
    left_join(fsa_city_tbl, keep = TRUE)

openxlsx::write.xlsx(pharmacists_tbl_joined, "Pharmacists_Joined_To_Pharmacies_2022_V13.xlsx")

###
# Pharmacies to Pharmacists 
###
# 
# pharmacies_tbl = import("OCP_Pharmacies_Completed_2022_V12.rds")
# org_pharmacies_tbl = pharmacies_tbl
# 
# pharmacists_tbl = import("OCP_Pharmacists_2022_V12.rds")
# org_pharmacists_tbl = pharmacists_tbl
# 
# pharmacies_tbl = pharmacies_tbl %>% 
#     separate_rows(pharmacy_staffs_primary_keys_url, sep = "\\,")
# 
# #pharmacists_tbl = pharmacists_tbl %>% 
# #    separate_rows(pharmacy_locations_primary_keys_url, sep = "\\,")
# 
# 
# 
# ##
# 
# pharmacies_tbl = import("OCP_Pharmacies_Completed_2022_V12.rds")
# org_pharmacies_tbl = pharmacies_tbl
# 
# pharmacists_tbl = import("OCP_Pharmacists_2022_V12.rds")
# org_pharmacists_tbl = pharmacists_tbl
# 
# pharmacists_tbl = pharmacists_tbl %>% 
#     separate_rows(pharmacy_locations_primary_keys_url, sep = "\\,")
# 
# pharmacies_tbl_joined_1 = pharmacies_tbl %>% 
#     left_join(org_pharmacists_tbl, by = c("pharmacy_url" = "pharmacy_locations_primary_keys_url"), keep = TRUE)
# 
# 
# pharmacies_tbl = import("OCP_Pharmacies_Completed_2022_V12.rds")
# org_pharmacies_tbl = pharmacies_tbl
# 
# pharmacists_tbl = import("OCP_Pharmacists_2022_V12.rds")
# org_pharmacists_tbl = pharmacists_tbl
# 
# pharmacies_tbl = pharmacies_tbl %>% 
#     separate_rows(pharmacy_staffs_primary_keys_url, sep = "\\,")
# 
# pharmacies_tbl_joined_2 = pharmacies_tbl %>% 
#     left_join(pharmacists_tbl, by = c("pharmacy_staffs_primary_keys_url" = "pharmacist_url"), keep = TRUE)
# 
# pharmacies_tbl_joined_3 = pharmacies_tbl_joined_2 %>% 
#     rbind(pharmacies_tbl_joined_1) 
# 
# pharmacies_tbl_joined_3 = pharmacies_tbl_joined_3 %>% 
#     select(-pharmacist_url, -pharmacy_url, -pharmacy_staffs_primary_keys_url, -pharmacy_locations_primary_keys_url)
# 
# pharmacies_tbl_joined_3 = pharmacies_tbl_joined_3 %>% distinct()
# 
# pharmacies_tbl_joined = pharmacies_tbl_joined_3
# 
# index_to_remove = vector(mode = 'list')
# 
# for (i in 1:nrow(pharmacies_tbl_joined)) {
#     
#     searched_pharmacy = pharmacies_tbl_joined %>% 
#         filter(accreditation_number == pharmacists_tbl_joined$accreditation_number[i])
#     
#     is_there_na = searched_pharmacy %>%
#         select(name.y) %>% 
#         is.na() %>% 
#         any()
#     
#     is_there_only_na = searched_pharmacy %>%
#         select(name.y) %>% 
#         is.na() %>% 
#         all()
#     
#     if (is_there_na & !is_there_only_na & is.na(pharmacies_tbl_joined$name.y[i])) {
#         index_to_remove = append(index_to_remove, i)
#     }
#     
#     print(str_glue("Pharmacy index {i}"))
#     
# }
# ##
# 
# pharmacies_tbl_joined = pharmacies_tbl_joined %>% 
#     filter(!row_number() %in% index_to_remove)
# 
# write_rds(pharmacies_tbl_joined, "pending3.rds")
# 
# pharmacies_tbl_joined = import("pending3.rds")
# 
# pharmacies_tbl_joined = pharmacies_tbl_joined %>% 
#     left_join(clinical_non_clinical_sites1, by = c("registration_number" = "registration_number")) %>% 
#     left_join(clinical_non_clinical_sites2, by = c("registration_number" = "registration_number"))
# 
# org_pharmacies_tbl = org_pharmacies_tbl %>% 
#     left_join(clinical_non_clinical_sites1, by = c("registration_number" = "registration_number")) %>% 
#     left_join(clinical_non_clinical_sites2, by = c("registration_number" = "registration_number"))
# 
# pharmacies_tbl_joined = pharmacies_tbl_joined %>% 
#     rename(pharmacy_name = name.x,
#            pharmacy_status = status.x)
# 
# pharmacies_tbl_joined = pharmacies_tbl_joined %>% 
#     mutate(works_both_clinical_and_non = ifelse(clinical_sites_working_at > 0 & non_clinical_sites_working_at > 0, 1, 0))
# 
# pharmacies_tbl_joined = pharmacies_tbl_joined %>% 
#     mutate(fte_per_store = ifelse(stores_working_at != 0, 1/stores_working_at, 0))
# 
# pharmacies_tbl_joined = pharmacies_tbl_joined %>% 
#     mutate(fte_per_store = ifelse(works_both_clinical_and_non == 1 & wpg3 == "Clinical", 0.1*fte_per_store, fte_per_store)) %>% 
#     mutate(fte_per_store = ifelse(works_both_clinical_and_non == 1 & wpg3 == "Non-clinical", 0.9*fte_per_store, fte_per_store))
# 
# pharmacies_tbl_joined = pharmacies_tbl_joined %>% 
#     group_by(accreditation_number) %>% 
#     mutate(fte_store_total = sum(fte_per_store)) %>% 
#     ungroup()
# 
# pharmacies4 = pharmacies_tbl_joined %>% 
#     anti_join(pharmacists_tbl_joined, by = c("registration_number" = "registration_number"))
# 
#     

org_pharmacists_tbl = org_pharmacists_tbl %>% 
    select(-pharmacist_url, -pharmacy_locations_primary_keys_url)

org_pharmacies_tbl = org_pharmacies_tbl %>% 
    select(-pharmacy_url, -pharmacy_staffs_primary_keys_url)

openxlsx::write.xlsx(org_pharmacies_tbl, "OCP_Pharmacies_2022_V12.xlsx")
openxlsx::write.xlsx(org_pharmacists_tbl, "OCP_Pharmacists_2022_V12.xlsx")
