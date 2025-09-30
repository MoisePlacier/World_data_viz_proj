# projet data viz 

L’OCDE dispose de nombreuses bases statistiques sur la santé (dépenses, ressources médicales, résultats sanitaires, comportements, etc.). Aujourd’hui ces informations sont dispersées et difficiles à explorer de manière unifiée. L’objectif du projet est de développer un tableau de bord interactif qui facilite l’analyse transversale de différents indicateurs liés à la santé.

# Public cible

- Décideurs publics (ministères de la santé, agences nationales) : besoin de comparer leur pays aux pairs OCDE pour orienter les politiques.

- Chercheurs et analystes : accès simplifié aux indicateurs pour études comparatives.

- Grand public et journalistes spécialisés : besoin d’indicateurs synthétiques et visuels pour vulgariser les enjeux.

# Problématiques à adresser : 

Comment comparer efficacement les dépenses de santé par habitant et leur évolution ?

Quels écarts entre pays dans les ressources médicales ?

Quels progrès ou retards dans les indicateurs de santé publique (espérance de vie, mortalité évitable, maladies chroniques) ?

Quels liens entre dépenses, ressources et résultats pour identifier des systèmes plus efficients ?



# Objectifs du tableau de bord

Rassembler en un seul outil les principaux datasets « Santé » de l’OCDE.

Permettre la sélection interactive d’indicateurs et la comparaison entre pays et dans le temps.

Fournir des visualisations intuitives (cartes, séries temporelles, classements).







variables avec moins de 15% de NA : 

 [1] "Reference.area"                                                                                                                
 [2] "Agriculture_agriculture_tonnes_of_co2equivalent"                                                                               
 [3] "Agriculture_energy_tonnes_of_co2equivalent"                                                                                    
 [4] "Agriculture_industrial_processes_and_product_use_tonnes_of_co2equivalent"                                                      
 [5] "Agriculture_irrigation_area_hectares"                                                                                          
 [6] "Agriculture_organic_farming_hectares"                                                                                          
 [7] "Agriculture_total_agricultural_land_area_hectares"                                                                             
 [8] "Agriculture_total_carbone_dioxide_co2_from_agriculture_tonnes_of_co2equivalent"                                                
 [9] "Agriculture_total_greenhouse_gas_emissions_by_source_without_lulucf_tonnes_of_co2equivalent"                                   
[10] "Agriculture_total_greenhouse_gas_emissions_from_agriculture_tonnes_of_co2equivalent"                                           
[11] "Agriculture_total_methane_ch4_from_agriculture_tonnes_of_co2equivalent"                                                        
[12] "Agriculture_total_nitrous_oxide_n2o_from_agriculture_tonnes_of_co2equivalent"                                                  
[13] "Agriculture_total_sales_of_agricultural_pesticides_tonnes"                                                                     
[14] "Agriculture_waste_tonnes_of_co2equivalent"                                                                                     
[15] "Exposition_pollution_population_share_exposed_percentage_of_population"                                                        
[16] "Health_life_expectancy_years"                                                                                                  
[17] "Medicament_pharmaceutical_consumption_defined_daily_dose_per_1_000_inhabitants_per_day"                                        
[18] "Mortalite_evitable_avoidable_mortality_deaths"                                                                                 
[19] "Mortalite_evitable_avoidable_mortality_deaths_per_100_000_inhabitants"                                                         
[20] "Mortalite_evitable_preventable_mortality_deaths"                                                                               
[21] "Mortalite_evitable_preventable_mortality_deaths_per_100_000_inhabitants"                                                       
[22] "Mortalite_evitable_treatable_mortality_deaths"                                                                                 
[23] "Mortalite_evitable_treatable_mortality_deaths_per_100_000_inhabitants"                                                         
[24] "PIB_gross_domestic_product_us_dollars_ppp_converted"                                                                           
[25] "PIB_par_Habitant_gross_domestic_product_per_capita_us_dollars_per_person_ppp_converted"                                        
[26] "PIB_sante_expenditure_percentage_of_gdp"                                                                                       
[27] "Qualite_des_soins_acute_myocardial_infarction_30_day_mortality_during_same_hospital_admission_unlinked_data_per_100_admissions"
[28] "Qualite_des_soins_asthma_and_chronic_obstructive_pulmonary_disease_hospital_admission_per_100_000_inhabitants"                 
[29] "Qualite_des_soins_asthma_hospital_admission_per_100_000_inhabitants"                                                           
[30] "Qualite_des_soins_chronic_obstructive_pulmonary_disease_hospital_admission_per_100_000_inhabitants"                            
[31] "Qualite_des_soins_congestive_heart_failure_hospital_admission_per_100_000_inhabitants"                                         
[32] "Qualite_des_soins_deprecated_congestive_heart_failure_and_hypertension_hospital_admission_per_100_000_inhabitants"             
[33] "Qualite_des_soins_deprecated_hypertension_hospital_admission_per_100_000_inhabitants"                                          
[34] "Qualite_des_soins_diabetes_hospital_admission_per_100_000_inhabitants"                                                         
[35] "Qualite_des_soins_haemorrhagic_stroke_30_day_mortality_during_same_hospital_admission_unlinked_data_per_100_admissions"        
[36] "Qualite_des_soins_ischaemic_stroke_30_day_mortality_during_same_hospital_admission_unlinked_data_per_100_admissions"           
[37] "Ressources_Sante_annual_income_factor_of_average_annual_wage"                                                                  
[38] "Ressources_Sante_annual_income_factor_of_gdp_per_capita"                                                                       
[39] "Ressources_Sante_annual_income_national_currency"                                                                              
[40] "Ressources_Sante_annual_income_us_dollars_exchange_rate_converted"                                                             
[41] "Ressources_Sante_annual_income_us_dollars_ppp_converted"                                                                       
[42] "Ressources_Sante_health_and_social_employment_per_1_000_inhabitants"                                                           
[43] "Ressources_Sante_health_and_social_employment_per_1_000_live_births"                                                           
[44] "Ressources_Sante_health_and_social_employment_percentage_of_employment"                                                        
[45] "Ressources_Sante_health_and_social_employment_percentage_of_workers_in_the_same_health_profession"                             
[46] "Ressources_Sante_health_and_social_employment_percentage_of_workers_in_the_same_health_profession_and_sex"                     
[47] "Ressources_Sante_health_and_social_employment_persons"                                                                         
[48] "Ressources_Sante_health_graduates_per_100_000_inhabitants"                                                                     
[49] "Ressources_Sante_health_graduates_per_1_000_practising_physicians_in_the_same_health_profession"                               
[50] "Ressources_Sante_health_graduates_persons"                                                                                     
[51] "Ressources_Sante_hospital_employment_per_1_000_inhabitants"                                                                    
[52] "Ressources_Sante_hospital_employment_persons"  