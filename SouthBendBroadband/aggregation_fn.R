require('dplyr')
require('magrittr')

modeling_aggregations <- function(dfs, split = F, filterplace = F, place = 71000){
  # Add aggregations and percentages to each df
  mod_dfs <- dfs %>% 
    lapply( FUN = function( d, split_, filterplace_, place_ ){
      new_df <- d
      if( filterplace_ ){
        new_df <- new_df %>% filter( PlaceCode == place_ )
      }
      new_df <- new_df %>% 
        rename( 
          # Education Fields  
          Education_College_Tot = Population_Bachelors_Degree_Plus,
          Education_College_PCT = Population_Bachelors_Degree_Plus_PCT,
          
          # Income Fields
          Income_Workers_Tot = Population_Labor_Force,
          Income_Workers_Employed_Tot = Population_Civilian_Labor_Force,
          Income_Workers_Unemployed_Tot = Population_Civilian_Labor_Force_Unemployed,
          Income_Median_Family_Tot = Household_Income_Family_Median,
          Income_Median_NonFamily_Tot = Household_Income_Non_Family_Median,
          Income_Sub_30K_Tot = Households_Income_Sub_30K,
          Income_Sub_75K_Tot = Households_Income_Sub_75K,
          Income_Over_75K_Tot = Households_Income_75K_Up,
          Income_Workers_PCT = PPopulation_Labor_Force_PCT,
          Income_Workers_Employed_PCT = Population_Civilian_Labor_Force_PCT,
          Income_Workers_Unemployed_PCT = Population_Civilian_Labor_Force_Unemployed_PCT,
          Income_Sub_30K_PCT = Households_Income_Sub_30K_PCT,
          Income_Sub_75K_PCT = Households_Income_Sub_75K_PCT,
          Income_Over_75K_PCT = Households_Income_75K_Up_PCT,
          
          # Housing Fields
          Housing_Units_Tot = Housing_Units_Total,
          Housing_Own_Tot = Housing_Units_Owner_Occupied,
          Housing_Rent_Tot = Housing_Units_Renter_Occupied,
          Housing_Occupied_Tot = Housing_Units_Occupied,
          Housing_Vacancy_Tot = Housing_Units_Vacant,
          Housing_HH_Tot = Households_Total,
          Housing_HH_Family_Tot = Households_Family,
          Housing_HH_Family_Married_Tot = Households_Family_Married,
          Housing_HH_Non_Family_Tot = Households_Non_Family,
          Housing_Value_25th_Percentile_Tot = Housing_Owner_Value_25th_Percentile,
          Housing_Value_50th_Percentile_Tot = Housing_Owner_Value_50th_Percentile,
          Housing_Value_75th_Percentile_Tot = Housing_Owner_Value_75th_Percentile,
          Housing_Population_Per_Household_Tot = Population_Per_Occupied_Housing_Unit,
          Housing_Own_PCT = Housing_Units_Owner_Occupied_PCT,
          Housing_Rent_PCT = Housing_Units_Renter_Occupied_PCT,
          Housing_Occupied_PCT = Housing_Units_Occupied_PCT,
          Housing_Vacancy_PCT = Housing_Units_Vacant_PCT,
          Housing_HH_Family_PCT = Households_Family_PCT,
          Housing_HH_Family_Married_PCT = Households_Family_Married_PCT,
          Housing_HH_Non_Family_PCT = Households_Non_Family_PCT,
          Housing_HH_Alone_PCT = Households_Non_Family_Alone_PCT,
          
          # Demographic Fields
          Demographic_Median_Age_Tot = Population_Age_Median,
          Demographic_Population_Tot = Population_Total,
          Demographic_Population_Density_Tot = Population_Density,
          Demographic_Male_Tot = Population_Male,
          Demographic_Female_Tot = Population_Female,
          Demographic_White_Tot = Population_White,
          Demographic_Black_Tot = Population_Black,
          Demographic_Native_Tot = Population_Native, 
          Demographic_Asian_Tot = Population_Asian,
          Demographic_Pacific_Tot = Population_Pacific,
          Demographic_Other_Ethnicity_Tot = Population_Other_Ethnicity,
          Demographic_Two_Plus_Ethnicity_Tot = Population_Two_Plus_Ethnicities,
          Demographic_Age_Sub_21_PCT = Population_Age_Under_21_PCT,
          Demographic_Age_21_To_64_PCT = Population_Age_21_To_64_PCT,
          Demographic_Age_Over_64_PCT = Population_Age_65_And_Over_PCT,
          Demographic_Male_PCT = Population_Male_PCT,
          Demographic_Female_PCT = Population_Female_PCT,
          Demographic_White_PCT = Population_White_PCT,
          Demographic_Black_PCT = Population_Black_PCT,
          Demographic_Native_PCT = Population_Native_PCT, 
          Demographic_Asian_PCT = Population_Asian_PCT,
          Demographic_Pacific_PCT = Population_Pacific_PCT,
          Demographic_Other_Ethnicity_PCT = Population_Other_Ethnicity_PCT,
          Demographic_Two_Plus_Ethnicity_PCT = Population_Two_Plus_Ethnicities_PCT,
          
          # Commute Fields
          Commute_Home_Tot = Population_Commute_Worked_At_Home,
          Commute_Alone_Tot = Population_Commute_Vehicle_Alone,
          Commute_Carpool_Tot = Population_Commute_Carpool,
          Commute_Home_PCT = Population_Commute_Worked_At_Home_PCT,
          Commute_Alone_PCT = Population_Commute_Vehicle_Alone_PCT,
          Commute_Carpool_PCT = Population_Commute_Carpool_PCT,
          
          # WebAccess Fields
          WebAccess_DBA_Tot = FCC_Number_Fixed_Broadband_DBAs,
          WebAccess_Providers_Tot = FCC_Number_Fixed_Broadband_Providers,
          WebAccess_Technologies_Tot = FCC_Number_Available_Technologies,
          WebAccess_BB_Techs_Tot = FCC_Number_Available_Broadband_Technologies,
          WebAccess_BB_Consumer_Companies_Tot = FCC_Number_Consumer_Broadband_Companies,
          WebAccess_BB_Consumer_Providers_Tot = FCC_Number_Consumer_Broadband_Providers,
          WebAccess_BB_Consumer_DBA_Tot = FCC_Number_Consumer_Broadband_DBAs,
          WebAccess_BB_Consumer_Blocks_Tot = FCC_Number_Blocks_With_Consumer_Broadband,
          WebAccess_BB_100M_Companies_Tot = FCC_Number_Consumer_Broadband_100Mbps_Companies,
          WebAccess_BB_100M_Providers_Tot = FCC_Number_Consumer_Broadband_100Mbps_Providers,
          WebAccess_BB_100M_DBA_Tot = FCC_Number_Consumer_Broadband_100Mbps_DBAs,
          WebAccess_BB_100M_Blocks_Tot = FCC_Number_Blocks_With_100Mbps_Consumer_Broadband,
          WebAccess_BB_1G_Companies_Tot = FCC_Number_Consumer_Broadband_1GB_Companies,
          WebAccess_BB_1G_Providers_Tot = FCC_Number_Consumer_Broadband_1GB_Providers,
          WebAccess_BB_1G_DBA_Tot = FCC_Number_Consumer_Broadband_1GBM_DBAs,
          WebAccess_BB_1G_Blocks_Tot = FCC_Number_Blocks_With_1GB_Consumer_Broadband,
          WebAccess_Max_Ad_Down_Tot = FCC_Maximun_MaxAdDown,
          WebAccess_Min_Ad_Down_Tot = FCC_Minimum_MaxAdDown,
          WebAccess_Max_Ad_Up_Tot = FCC_Maximum_MaxAdUp,
          WebAccess_Min_Ad_Up_Tot = FCC_Minimum_MaxAdUp,
          WebAccess_Max_CIR_Down_Tot = FCC_Maximun_MaxCIRDown,
          WebAccess_Min_CIR_Down_Tot = FCC_Minimum_MaxCIRDown,
          WebAccess_Max_CIR_Up_Tot = FCC_Maximum_MaxCIRUp,
          WebAccess_Min_CIR_Up_Tot = FCC_Minimum_MaxCIRUp,
          WebAccess_Land_Tot = FCC_Land_Area_Covered_Consumer_Broadband,
          WebAccess_Water_Tot = FCC_Water_Area_Covered_Consumer_Broadband,
          WebAccess_Land_100M_Tot = FCC_Land_Area_Covered_100Mbps_Consumer_Broadband,
          WebAccess_Water_100M_Tot = FCC_Water_Area_Covered_100Mbps_Consumer_Broadband,
          WebAccess_Land_1G_Tot = FCC_Land_Area_Covered_1GB_Consumer_Broadband,
          WebAccess_Water_1G_Tot = FCC_Water_Area_Covered_1GB_Consumer_Broadband,
          WebAccess_Block_Tot = FCC_Blocks_Covered_Consumer_Broadband,
          WebAccess_Block_100M_Tot = FCC_Blocks_Covered_100Mbps_Consumer_Broadband,
          WebAccess_Block_1G_Tot = FCC_Blocks_Covered_1GB_Consumer_Broadband,
          WebAccess_Land_PCT = FCC_Land_Area_Covered_Consumer_Broadband_PCT,
          WebAccess_Land_100M_PCT = FCC_Land_Area_Covered_100Mbps_Consumer_Broadband_PCT,
          WebAccess_Land_1G_PCT = FCC_Land_Area_Covered_1GB_Consumer_Broadband_PCT,
          
          # Broadband Fields
          Broadband_any_Computer_Tot = Households_With_Any_Computer,
          Broadband_Laptop_or_Desktop_Tot = Households_With_Desktop_Laptop,
          Broadband_SmartPhone_or_Tablet_Tot = Households_With_Smartphone_Tablet,
          Broadband_Internet_Tot = Households_With_Broadband_Sat_Internet,
          Broadband_Computer_Plus_Internet_Tot = Households_With_Computer_And_Broadband,
          Broadband_any_Computer_PCT = Households_With_Any_Computer_PCT,
          Broadband_Laptop_or_Desktop_PCT = Households_With_Desktop_Laptop_PCT,
          Broadband_SmartPhone_or_Tablet_PCT = Households_With_Smartphone_Tablet_PCT,
          Broadband_Internet_PCT = Households_With_Broadband_Sat_Internet_PCT,
          Broadband_Computer_Plus_Internet_PCT = Households_With_Computer_And_Broadband_PCT
        ) 
      # Fields unique to Tracts
      if( all(c('Population_Native_Born', 'Population_Native_Born_PCT',
                'Population_Speaks_Only_English', 'Population_Speaks_Only_English_PCT',
                'Population_With_Disability', 'Population_With_Disability_PCT') %in% 
              names(new_df)) ) {
        new_df <- new_df %>% rename( 
          Demographic_Native_Born_Tot = Population_Native_Born,
          Demographic_Native_Born_PCT = Population_Native_Born_PCT,
          Demographic_Speaks_Only_English_Tot = Population_Speaks_Only_English,
          Demographic_Speaks_Only_English_PCT = Population_Speaks_Only_English_PCT,
          Demographic_Disability_Tot = Population_With_Disability,
          Demographic_Disability_PCT = Population_With_Disability_PCT
        )
      }
      if( split_ == T ){
        return(list(
          # get totals data
          'Tot' = new_df %>% 
            select(ends_with('Tot')) %>% 
            rename_all((function(x){str_remove(x, pattern = '_Tot')})) %>%
            mutate_all((function(x){ifelse(is.na(x), 0, x)})),
          
          # get percentage data
          'Pct' = new_df %>% 
            select(ends_with('PCT')) %>% 
            rename_all((function(x){str_remove(x, pattern = '_PCT')})) %>%
            mutate_all((function(x){ifelse(is.na(x), 0, x)}))
        ))
      } else {
        return( new_df )
      }
    }, split, filterplace, place )
  return( mod_dfs )
}
