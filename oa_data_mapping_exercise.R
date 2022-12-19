#####################################
# @author Miqdad Asaria
# @date 13 December 2022
####################################

library("tidyr")
library("dplyr")
library("janitor")
library("tmap")
library("sf")
# library("tidyverse")

oa_religion_data = read_csv("data/OA data/TS030-2021-1-filtered-2022-12-12T08_41_07Z.csv") %>%
  clean_names()

oa_ethnicity_data = read_csv("data/OA data/TS021-2021-1-filtered-2022-12-12T08_43_15Z.csv") %>%
  rename(oa21cd = 'Output Areas') %>%
  pivot_wider(id_cols = oa21cd, names_from = 'Ethnic group (20 categories)', values_from = Observation) %>%
  clean_names()

oa_total_population = oa_religion_data %>% 
  group_by(output_areas) %>%
  summarise(total_population = sum(observation)) %>%
  rename(oa21cd = output_areas)

oa_religion_population = oa_religion_data %>%
  pivot_wider(id_cols = output_areas, names_from = religion_10_categories, values_from = observation) %>%
  clean_names() %>%
  rename(oa21cd = output_areas)

oa_ward_mapping = read_csv("data/OA_(2021)_to_Ward_to_LTLA_to_UTLA_to_Region_to_Country_(May_2022)_Lookup_in_England_and_Wales.csv") %>% 
  clean_names() %>%
  select(oa21cd, wd22cd, wd22nm, ltla22cd, ltla22nm) 

oa_pcon_mapping = read_csv("data/OAs_(2021)_to_Westminster_Parliamentary_Constituency_to_Regions_(May_2022)_Lookup_in_England_and_Wales.csv") %>%
  clean_names() %>%
  select(-object_id)

oa_lsoa_mapping = read_csv("data/OA21_LSOA21_MSOA21_LAD22_EW_LU.csv") %>%
  clean_names()

oa_master_file = oa_ward_mapping %>%
  left_join(oa_pcon_mapping) %>%
  left_join(oa_lsoa_mapping) %>%
  left_join(oa_total_population) %>%
  left_join(oa_religion_population) %>%
  left_join(oa_ethnicity_data)

# write_csv(oa_master_file, "output/oa_religion_ethnicity_master_file.csv")

# remove all these temporary variables from memory
rm(oa_religion_data, oa_religion_population, oa_total_population, oa_ethnicity_data, oa_ward_mapping, oa_pcon_mapping, oa_lsoa_mapping)

# you can start from here if you uncomment the line below and have the master file saved in the output directory
# oa_master_file = read_csv("output/oa_religion_ethnicity_master_file.csv")

# load ward shapefiles for UK
ward_map = st_read("shapefiles/Wards_(May_2022)_Boundaries_UK_BGC_V3/", stringsAsFactors = FALSE)

# load parliamentary constituency shapefiles for UK
pcon_map = st_read("shapefiles/Westminster_Parliamentary_Constituencies_(Dec_2021)_UK_BGC/", stringsAsFactors = FALSE)

#####################################################
#
# Function to aggregate data and draw maps given a
# ltla_list = list of one or more local authorities by name
# level = aggregation level i.e. ward or pcon
# variable = either a religion or an ethnicity
# full list of options given below and example calls follow
#
#####################################################

# possible levels include: 
#"ward"
#"pcon"
# possible variables include for religion: 
# does_not_apply
# no_religion
# christian
# buddhist
# hindu
# jewish
# muslim
# sikh
# other_religion
# not_answered
# possible variables include for ethnicity: 
# asian_asian_british_or_asian_welsh_bangladeshi
# asian_asian_british_or_asian_welsh_chinese
# asian_asian_british_or_asian_welsh_indian
# asian_asian_british_or_asian_welsh_pakistani
# asian_asian_british_or_asian_welsh_other_asian
# black_black_british_black_welsh_caribbean_or_african_african
# black_black_british_black_welsh_caribbean_or_african_caribbean
# black_black_british_black_welsh_caribbean_or_african_other_black
# mixed_or_multiple_ethnic_groups_white_and_asian
# mixed_or_multiple_ethnic_groups_white_and_black_african
# mixed_or_multiple_ethnic_groups_white_and_black_caribbean
# mixed_or_multiple_ethnic_groups_other_mixed_or_multiple_ethnic_groups
# white_english_welsh_scottish_northern_irish_or_british
# white_irish
# white_gypsy_or_irish_traveller
# white_roma
# white_other_white
# other_ethnic_group_arab
# other_ethnic_group_any_other_ethnic_group
draw_map = function(ltla_list, level, variable){
  # restrict data to only those local authorities requested
  ltla_oa_data = oa_master_file %>%
    filter(ltla22nm %in% ltla_list)
  
  # aggregate data and subset map file accordingly
  if(level == "ward"){
    agg_oa_data = ltla_oa_data %>%
      group_by(wd22cd, wd22nm, ltla22nm) %>%
      summarise(total_population = sum(total_population),
                variable_population = sum(get(variable))) %>%
      mutate(variable_percent = round(100*variable_population/total_population,2))
    
    map = ward_map %>% 
      inner_join(agg_oa_data, by = c("WD22CD"="wd22cd"))
  } else if(level == "pcon") {
    agg_oa_data = ltla_oa_data %>%
      group_by(wpc22cd, wpc22nm, ltla22nm) %>%
      summarise(total_population = sum(total_population),
                variable_population = sum(get(variable))) %>%
      mutate(variable_percent = round(100*variable_population/total_population,2))
    
    map = pcon_map %>% 
      inner_join(agg_oa_data, by = c("PCON21CD"="wpc22cd"))
  }
  
  # use the shapefile to create the map usinng tmap
  map = tm_shape(map) +
    tm_polygons(col="variable_percent",
                palette = "-viridis",
                legend.format=list(fun=function(x) paste0(formatC(x, digits=1, format="f"), "%")),
                palette="seq",
                border.col = "white", 
                border.alpha = 0.5,
                title=paste0(str_to_title(str_replace_all(variable,"_"," "))," (%)")) +
    tm_facets(by = c("ltla22nm"), ncol = 1) +
    tm_credits("Source: Office for National Statistics licensed under the Open Government Licence v.3.0
             Contains OS data © Crown copyright and database right 2022", size=0.5)
  
  # save the resulting map to the output directory
  output_type="png"
  tmap_save(map, filename=paste0("output/",level,"_map_",variable,"_",paste0(ltla_list, collapse = "_"),".",output_type))
  
}



# some examples - will save maps into output directory
draw_map(ltla_list = c("Harrow"), level = "pcon", variable = "muslim")
draw_map(ltla_list = c("Harrow","Brent"), level = "ward", variable = "hindu")
draw_map(ltla_list = c("Manchester","Birmingham"), level = "ward", variable = "asian_asian_british_or_asian_welsh_pakistani")
    

