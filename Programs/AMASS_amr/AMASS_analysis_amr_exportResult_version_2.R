# Created on 20th April 2022
#----Sub-divide from AMASSplus released on 25th March 2021
#----[Section 2]Edit exported column from 'organism' to 'organism3' of exp_rpt2_1() function

# Call essential functions from AMASS_function.R####
source("./Programs/AMASS_amr/AMASS_analysis_amr_function_version_2.R")

# Aggregated data to be exported in csv: Report 1 ####
exp_rpt1_1 <- function(MicroData2,HospData2) {
  rpt1_month <- data.frame(c("January", "February", "March", "April",
                                        "May", "June", "July", "August",
                                        "September", "October", "November", "December"),
                                      c(length(which(MicroData2$MonthSpc=="January")),
                                        length(which(MicroData2$MonthSpc=="February")),
                                        length(which(MicroData2$MonthSpc=="March")),
                                        length(which(MicroData2$MonthSpc=="April")),
                                        length(which(MicroData2$MonthSpc=="May")),
                                        length(which(MicroData2$MonthSpc=="June")),
                                        length(which(MicroData2$MonthSpc=="July")),
                                        length(which(MicroData2$MonthSpc=="August")),
                                        length(which(MicroData2$MonthSpc=="September")),
                                        length(which(MicroData2$MonthSpc=="October")),
                                        length(which(MicroData2$MonthSpc=="November")),
                                        length(which(MicroData2$MonthSpc=="December"))),
                                      c(length(which(HospData2$MonthAdm=="January")),
                                        length(which(HospData2$MonthAdm=="February")),
                                        length(which(HospData2$MonthAdm=="March")),
                                        length(which(HospData2$MonthAdm=="April")),
                                        length(which(HospData2$MonthAdm=="May")),
                                        length(which(HospData2$MonthAdm=="June")),
                                        length(which(HospData2$MonthAdm=="July")),
                                        length(which(HospData2$MonthAdm=="August")),
                                        length(which(HospData2$MonthAdm=="September")),
                                        length(which(HospData2$MonthAdm=="October")),
                                        length(which(HospData2$MonthAdm=="November")),
                                        length(which(HospData2$MonthAdm=="December"))))
  names(rpt1_month) <- c("Month",
                                    "Number_of_specimen_in_microbiology_data_file",
                                    "Number_of_hospital_records_in_hospital_admission_data_file")
  return(rpt1_month)
}
exp_rpt1_2 <- function(MicroData2) {
  rpt1_month <- data.frame(c("January", "February", "March", "April",
                                        "May", "June", "July", "August",
                                        "September", "October", "November", "December"),
                                      c(length(which(MicroData2$MonthSpc=="January")),
                                        length(which(MicroData2$MonthSpc=="February")),
                                        length(which(MicroData2$MonthSpc=="March")),
                                        length(which(MicroData2$MonthSpc=="April")),
                                        length(which(MicroData2$MonthSpc=="May")),
                                        length(which(MicroData2$MonthSpc=="June")),
                                        length(which(MicroData2$MonthSpc=="July")),
                                        length(which(MicroData2$MonthSpc=="August")),
                                        length(which(MicroData2$MonthSpc=="September")),
                                        length(which(MicroData2$MonthSpc=="October")),
                                        length(which(MicroData2$MonthSpc=="November")),
                                        length(which(MicroData2$MonthSpc=="December"))))
  names(rpt1_month) <- c("Month",
                                    "Number_of_specimen_in_microbiology_data_file")
  return(rpt1_month)
  
}

# Aggregated data to be exported in csv: Report 2 ####
exp_rpt2_1 <- function(MicroData_bsi) {
  rpt2_org <- data.frame(table(MicroData_bsi$organism3))
  names(rpt2_org) <- c("Organism", "Number_of_blood_specimens_culture_positive_for_the_organism")
  return (rpt2_org)
}
exp_rpt2_2 <- function(blood_dedup_sa,blood_dedup_es,blood_dedup_sp,blood_dedup_ss,blood_dedup_ec,blood_dedup_kp,blood_dedup_pa,blood_dedup_as) {
  rpt2_org_survey <- data.frame(c("Staphylococcus aureus", "Enterococcus spp.",
                                                  "Streptococcus pneumoniae", "Salmonella spp.",
                                                  "Escherichia coli", "Klebsiella pneumoniae",
                                                  "Pseudomonas aeruginosa", "Acinetobacter spp."),
                                                c(nrow(blood_dedup_sa), nrow(blood_dedup_es),
                                                  nrow(blood_dedup_sp), nrow(blood_dedup_ss),
                                                  nrow(blood_dedup_ec), nrow(blood_dedup_kp),
                                                  nrow(blood_dedup_pa), nrow(blood_dedup_as)))
  names(rpt2_org_survey) <- c("Organism", "Number_of_blood_specimens_culture_positive_deduplicated")
  return (rpt2_org_survey)
}

## Tables in Report 2
exp_rpt2_3 <- function(isoRep_blood_stapha_graph,isoRep_blood_enterococcus_graph,isoRep_blood_streptopneu_graph,isoRep_blood_salmonella_graph,
                       isoRep_blood_ecoli_graph,isoRep_blood_klebp_graph,isoRep_blood_pseudoa_graph,isoRep_blood_acine_graph) {
  isoRep_blood_stapha_graph[,8:10] <- NULL
  isoRep_blood_stapha_graph$Organism <- "Staphylococcus aureus"
  isoRep_blood_enterococcus_graph[,8:10] <- NULL
  isoRep_blood_enterococcus_graph$Organism <- "Enterococcus spp."
  isoRep_blood_streptopneu_graph[,8:10] <- NULL
  isoRep_blood_streptopneu_graph$Organism <- "Streptococcus pneumoniae"
  isoRep_blood_salmonella_graph[,8:10] <- NULL
  isoRep_blood_salmonella_graph$Organism <- "Salmonella spp."
  isoRep_blood_ecoli_graph[,8:10] <- NULL
  isoRep_blood_ecoli_graph$Organism <- "Escherichia coli"
  isoRep_blood_klebp_graph[,8:10] <- NULL
  isoRep_blood_klebp_graph$Organism <- "Klebsiella pneumoniae"
  isoRep_blood_pseudoa_graph[,8:10] <- NULL
  isoRep_blood_pseudoa_graph$Organism <- "Pseudomonas aeruginosa"
  isoRep_blood_acine_graph[,8:10] <- NULL
  isoRep_blood_acine_graph$Organism <- "Acinetobacter spp."
  rpt2_table <- rbind(isoRep_blood_stapha_graph, isoRep_blood_enterococcus_graph,
                         isoRep_blood_streptopneu_graph, isoRep_blood_salmonella_graph,
                         isoRep_blood_ecoli_graph, isoRep_blood_klebp_graph,
                         isoRep_blood_pseudoa_graph, isoRep_blood_acine_graph)
  rpt2_table <- rpt2_table[movecolumn(names(rpt2_table), "Organism first")]
  return(rpt2_table)
}
  
# Aggregated data to be exported in csv: Report 3 ####
exp_rpt3_1 <- function(blood_dedup_sa,blood_dedup_es,blood_dedup_sp,blood_dedup_ss,
                       blood_dedup_ec,blood_dedup_kp,blood_dedup_pa,blood_dedup_as,
                       merged_blood_dedup_sa,merged_blood_dedup_es,merged_blood_dedup_sp,merged_blood_dedup_ss,
                       merged_blood_dedup_ec,merged_blood_dedup_kp,merged_blood_dedup_pa,merged_blood_dedup_as) {
  rpt3_origin <- data.frame(c("Staphylococcus aureus", "Enterococcus spp.",
                                       "Streptococcus pneumoniae", "Salmonella spp.",
                                       "Escherichia coli", "Klebsiella pneumoniae",
                                       "Pseudomonas aeruginosa", "Acinetobacter spp."),
                                     c(nrow(blood_dedup_sa), nrow(blood_dedup_es),
                                       nrow(blood_dedup_sp), nrow(blood_dedup_ss),
                                       nrow(blood_dedup_ec), nrow(blood_dedup_kp),
                                       nrow(blood_dedup_pa), nrow(blood_dedup_as)),
                                     c(nrow(merged_blood_dedup_sa), nrow(merged_blood_dedup_es),
                                       nrow(merged_blood_dedup_sp), nrow(merged_blood_dedup_ss),
                                       nrow(merged_blood_dedup_ec), nrow(merged_blood_dedup_kp),
                                       nrow(merged_blood_dedup_pa), nrow(merged_blood_dedup_as)),
                                     c(length(which(merged_blood_dedup_sa$InfOri==0)), length(which(merged_blood_dedup_es$InfOri==0)),
                                       length(which(merged_blood_dedup_sp$InfOri==0)), length(which(merged_blood_dedup_ss$InfOri==0)),
                                       length(which(merged_blood_dedup_ec$InfOri==0)), length(which(merged_blood_dedup_kp$InfOri==0)),
                                       length(which(merged_blood_dedup_pa$InfOri==0)), length(which(merged_blood_dedup_as$InfOri==0))),
                                     c(length(which(merged_blood_dedup_sa$InfOri==1)), length(which(merged_blood_dedup_es$InfOri==1)),
                                       length(which(merged_blood_dedup_sp$InfOri==1)), length(which(merged_blood_dedup_ss$InfOri==1)),
                                       length(which(merged_blood_dedup_ec$InfOri==1)), length(which(merged_blood_dedup_kp$InfOri==1)),
                                       length(which(merged_blood_dedup_pa$InfOri==1)), length(which(merged_blood_dedup_as$InfOri==1))))
  names(rpt3_origin) <- c("Organism", "Number_of_patients_with_blood_culture_positive",
                                     "Number_of_patients_with_blood_culture_positive_merged_with_hospital_data_file",
                                     "Community_origin", "Hospital_origin")
  rpt3_origin$Unknown_origin <- rpt3_origin$Number_of_patients_with_blood_culture_positive - 
    rpt3_origin$Number_of_patients_with_blood_culture_positive_merged_with_hospital_data_file
  return (rpt3_origin)
}
## Tables in Report 3
exp_rpt3_2 <- function(co_extRep_blood_stapha_graph,co_extRep_blood_enterococcus_graph,co_extRep_blood_streptopneu_graph,co_extRep_blood_salmonella_graph,
                       co_extRep_blood_ecoli_graph,co_extRep_blood_klebp_graph,co_extRep_blood_pseudoa_graph,co_extRep_blood_acines_graph,
                       ho_extRep_blood_stapha_graph,ho_extRep_blood_enterococcus_graph,ho_extRep_blood_streptopneu_graph,ho_extRep_blood_salmonella_graph,
                       ho_extRep_blood_ecoli_graph,ho_extRep_blood_klebp_graph,ho_extRep_blood_pseudoa_graph,ho_extRep_blood_acines_graph) {
  co_extRep_blood_stapha_graph[,8:10] <- NULL
  co_extRep_blood_stapha_graph$Organism <- "Staphylococcus aureus"
  co_extRep_blood_stapha_graph$Infection_origin <- "Community"
  co_extRep_blood_enterococcus_graph[,8:10] <- NULL
  co_extRep_blood_enterococcus_graph$Organism <- "Enterococcus spp."
  co_extRep_blood_enterococcus_graph$Infection_origin <- "Community"
  co_extRep_blood_streptopneu_graph[,8:10] <- NULL
  co_extRep_blood_streptopneu_graph$Organism <- "Streptococcus pneumoniae"
  co_extRep_blood_streptopneu_graph$Infection_origin <- "Community"
  co_extRep_blood_salmonella_graph[,8:10] <- NULL
  co_extRep_blood_salmonella_graph$Organism <- "Salmonella spp."
  co_extRep_blood_salmonella_graph$Infection_origin <- "Community"
  co_extRep_blood_ecoli_graph[,8:10] <- NULL
  co_extRep_blood_ecoli_graph$Organism <- "Escherichia coli"
  co_extRep_blood_ecoli_graph$Infection_origin <- "Community"
  co_extRep_blood_klebp_graph[,8:10] <- NULL
  co_extRep_blood_klebp_graph$Organism <- "Klebsiella pneumoniae"
  co_extRep_blood_klebp_graph$Infection_origin <- "Community"
  co_extRep_blood_pseudoa_graph[,8:10] <- NULL
  co_extRep_blood_pseudoa_graph$Organism <- "Pseudomonas aeruginosa"
  co_extRep_blood_pseudoa_graph$Infection_origin <- "Community"
  co_extRep_blood_acines_graph[,8:10] <- NULL
  co_extRep_blood_acines_graph$Organism <- "Acinetobacter spp."
  co_extRep_blood_acines_graph$Infection_origin <- "Community"

  ho_extRep_blood_stapha_graph[,8:10] <- NULL
  ho_extRep_blood_stapha_graph$Organism <- "Staphylococcus aureus"
  ho_extRep_blood_stapha_graph$Infection_origin <- "Hospital"
  ho_extRep_blood_enterococcus_graph[,8:10] <- NULL
  ho_extRep_blood_enterococcus_graph$Organism <- "Enterococcus spp."
  ho_extRep_blood_enterococcus_graph$Infection_origin <- "Hospital"
  ho_extRep_blood_streptopneu_graph[,8:10] <- NULL
  ho_extRep_blood_streptopneu_graph$Organism <- "Streptococcus pneumoniae"
  ho_extRep_blood_streptopneu_graph$Infection_origin <- "Hospital"
  ho_extRep_blood_salmonella_graph[,8:10] <- NULL
  ho_extRep_blood_salmonella_graph$Organism <- "Salmonella spp."
  ho_extRep_blood_salmonella_graph$Infection_origin <- "Hospital"
  ho_extRep_blood_ecoli_graph[,8:10] <- NULL
  ho_extRep_blood_ecoli_graph$Organism <- "Escherichia coli"
  ho_extRep_blood_ecoli_graph$Infection_origin <- "Hospital"
  ho_extRep_blood_klebp_graph[,8:10] <- NULL
  ho_extRep_blood_klebp_graph$Organism <- "Klebsiella pneumoniae"
  ho_extRep_blood_klebp_graph$Infection_origin <- "Hospital"
  ho_extRep_blood_pseudoa_graph[,8:10] <- NULL
  ho_extRep_blood_pseudoa_graph$Organism <- "Pseudomonas aeruginosa"
  ho_extRep_blood_pseudoa_graph$Infection_origin <- "Hospital"
  ho_extRep_blood_acines_graph[,8:10] <- NULL
  ho_extRep_blood_acines_graph$Organism <- "Acinetobacter spp."
  ho_extRep_blood_acines_graph$Infection_origin <- "Hospital"

  rpt3_table <- rbind(co_extRep_blood_stapha_graph, co_extRep_blood_enterococcus_graph,
                         co_extRep_blood_streptopneu_graph, co_extRep_blood_salmonella_graph,
                         co_extRep_blood_ecoli_graph, co_extRep_blood_klebp_graph,
                         co_extRep_blood_pseudoa_graph, co_extRep_blood_acines_graph,
                         ho_extRep_blood_stapha_graph, ho_extRep_blood_enterococcus_graph,
                         ho_extRep_blood_streptopneu_graph, ho_extRep_blood_salmonella_graph,
                         ho_extRep_blood_ecoli_graph, ho_extRep_blood_klebp_graph,
                         ho_extRep_blood_pseudoa_graph, ho_extRep_blood_acines_graph)
  rpt3_table <- rpt3_table[movecolumn(names(rpt3_table), "Infection_origin first")]
  rpt3_table <- rpt3_table[movecolumn(names(rpt3_table), "Organism first")]
  return (rpt3_table)
}

# Aggregated data to be exported in csv: Report 4 ####
## Frequency of organisms under survey per 100,000 tested patients
exp_rpt4_1 <- function(incidence_blood) {
  names(incidence_blood) <- c("Organism", "Number_of_patients",
                              "frequency_per_tested", "frequency_per_tested_lci",
                              "frequency_per_tested_uci")
  return(incidence_blood)
}
## Frequency of priority pathogens under survey per 100,000 tested patients
exp_rpt4_2 <- function(incidence_blood_antibiotic) {
  names(incidence_blood_antibiotic) <- c("Organism", "Priority_pathogen",
                                         "Number_of_patients", "frequency_per_tested",
                                         "frequency_per_tested_lci", "frequency_per_tested_uci")
  incidence_blood_antibiotic$Priority_pathogen <- gsub(pattern = "\n", "", incidence_blood_antibiotic$Priority_pathogen)
  return(incidence_blood_antibiotic)
}
  
# Aggregated data to be exported in csv: Report 5 ####
## Community-origin: Frequency of organisms under survey per 100,000 tested patients
exp_rpt5_1 <- function(incidence_blood_co) {
  names(incidence_blood_co) <- c("Organism", "Number_of_patients",
                                 "frequency_per_tested", "frequency_per_tested_lci",
                                 "frequency_per_tested_uci")
  incidence_blood_co$Infection_origin <- "Community"
  return(incidence_blood_co)
}
## Community-origin: Frequency of priority pathogens under survey per 100,000 tested patients
exp_rpt5_2 <- function(incidence_blood_antibiotic_co) {
  names(incidence_blood_antibiotic_co) <- c("Organism", "Priority_pathogen",
                                            "Number_of_patients", "frequency_per_tested",
                                            "frequency_per_tested_lci", "frequency_per_tested_uci")
  incidence_blood_antibiotic_co$Priority_pathogen <- gsub(pattern = "\n", "", incidence_blood_antibiotic_co$Priority_pathogen)
  incidence_blood_antibiotic_co$Infection_origin <- "Community"
  return (incidence_blood_antibiotic_co)
}
## Hospital-origin: Frequency of priority pathogens under survey per 100,000 tested patients
exp_rpt5_3 <- function(incidence_blood_ho) {
  names(incidence_blood_ho) <- c("Organism", "Number_of_patients",
                                 "frequency_per_tested", "frequency_per_tested_lci",
                                 "frequency_per_tested_uci")
  incidence_blood_ho$Infection_origin <- "Hospital"
  return(incidence_blood_ho)
}
## Hospital-origin: Frequency of priority pathogens under survey per 100,000 tested patients
exp_rpt5_4 <- function(incidence_blood_antibiotic_ho) {
  names(incidence_blood_antibiotic_ho) <- c("Organism", "Priority_pathogen",
                                            "Number_of_patients", "frequency_per_tested",
                                            "frequency_per_tested_lci", "frequency_per_tested_uci")
  incidence_blood_antibiotic_ho$Priority_pathogen <- gsub(pattern = "\n", "", incidence_blood_antibiotic_ho$Priority_pathogen)
  incidence_blood_antibiotic_ho$Infection_origin <- "Hospital"
  return(incidence_blood_antibiotic_ho)
}

# Aggregated data to be exported in csv: Report 6 ####
exp_rpt6 <- function(co_extRep_blood_sa_deathgraph,co_extRep_blood_es_deathgraph,co_extRep_blood_sp_deathgraph,co_extRep_blood_ss_deathgraph,
                     co_extRep_blood_ec_deathgraph,co_extRep_blood_kp_deathgraph,co_extRep_blood_pa_deathgraph,co_extRep_blood_as_deathgraph) {
  co_extRep_blood_sa_deathgraph <- co_extRep_blood_sa_deathgraph[,c("Antibiotic", "Mortality", "lowerCI", "upperCI")]
  co_extRep_blood_sa_deathgraph$Infection_origin <- "Community-origin"
  co_extRep_blood_sa_deathgraph$Organism <- "Staphylococcus aureus"
  co_extRep_blood_es_deathgraph <- co_extRep_blood_es_deathgraph[,c("Antibiotic", "Mortality", "lowerCI", "upperCI")]
  co_extRep_blood_es_deathgraph$Infection_origin <- "Community-origin"
  co_extRep_blood_es_deathgraph$Organism <- "Enterococcus spp."
  co_extRep_blood_sp_deathgraph <- co_extRep_blood_sp_deathgraph[,c("Antibiotic", "Mortality", "lowerCI", "upperCI")]
  co_extRep_blood_sp_deathgraph$Infection_origin <- "Community-origin"
  co_extRep_blood_sp_deathgraph$Organism <- "Streptococcus pneumoniae"
  co_extRep_blood_ss_deathgraph <- co_extRep_blood_ss_deathgraph[,c("Antibiotic", "Mortality", "lowerCI", "upperCI")]
  co_extRep_blood_ss_deathgraph$Infection_origin <- "Community-origin"
  co_extRep_blood_ss_deathgraph$Organism <- "Salmonella spp."
  co_extRep_blood_ec_deathgraph <- co_extRep_blood_ec_deathgraph[,c("Antibiotic", "Mortality", "lowerCI", "upperCI")]
  co_extRep_blood_ec_deathgraph$Infection_origin <- "Community-origin"
  co_extRep_blood_ec_deathgraph$Organism <- "Escherichia coli"
  co_extRep_blood_kp_deathgraph <- co_extRep_blood_kp_deathgraph[,c("Antibiotic", "Mortality", "lowerCI", "upperCI")]
  co_extRep_blood_kp_deathgraph$Infection_origin <- "Community-origin"
  co_extRep_blood_kp_deathgraph$Organism <- "Klebsiella pneumoniae"
  co_extRep_blood_pa_deathgraph <- co_extRep_blood_pa_deathgraph[,c("Antibiotic", "Mortality", "lowerCI", "upperCI")]
  co_extRep_blood_pa_deathgraph$Infection_origin <- "Community-origin"
  co_extRep_blood_pa_deathgraph$Organism <- "Pseudomonas aeruginosa"
  co_extRep_blood_as_deathgraph <- co_extRep_blood_as_deathgraph[,c("Antibiotic", "Mortality", "lowerCI", "upperCI")]
  co_extRep_blood_as_deathgraph$Infection_origin <- "Community-origin"
  co_extRep_blood_as_deathgraph$Organism <- "Acinetobacter spp."
  
  ho_extRep_blood_sa_deathgraph <- ho_extRep_blood_sa_deathgraph[,c("Antibiotic", "Mortality", "lowerCI", "upperCI")]
  ho_extRep_blood_sa_deathgraph$Infection_origin <- "Hospital-origin"
  ho_extRep_blood_sa_deathgraph$Organism <- "Staphylococcus aureus"
  ho_extRep_blood_es_deathgraph <- ho_extRep_blood_es_deathgraph[,c("Antibiotic", "Mortality", "lowerCI", "upperCI")]
  ho_extRep_blood_es_deathgraph$Infection_origin <- "Hospital-origin"
  ho_extRep_blood_es_deathgraph$Organism <- "Enterococcus spp."
  ho_extRep_blood_sp_deathgraph <- ho_extRep_blood_sp_deathgraph[,c("Antibiotic", "Mortality", "lowerCI", "upperCI")]
  ho_extRep_blood_sp_deathgraph$Infection_origin <- "Hospital-origin"
  ho_extRep_blood_sp_deathgraph$Organism <- "Streptococcus pneumoniae"
  ho_extRep_blood_ss_deathgraph <- ho_extRep_blood_ss_deathgraph[,c("Antibiotic", "Mortality", "lowerCI", "upperCI")]
  ho_extRep_blood_ss_deathgraph$Infection_origin <- "Hospital-origin"
  ho_extRep_blood_ss_deathgraph$Organism <- "Salmonella spp."
  ho_extRep_blood_ec_deathgraph <- ho_extRep_blood_ec_deathgraph[,c("Antibiotic", "Mortality", "lowerCI", "upperCI")]
  ho_extRep_blood_ec_deathgraph$Infection_origin <- "Hospital-origin"
  ho_extRep_blood_ec_deathgraph$Organism <- "Escherichia coli"
  ho_extRep_blood_kp_deathgraph <- ho_extRep_blood_kp_deathgraph[,c("Antibiotic", "Mortality", "lowerCI", "upperCI")]
  ho_extRep_blood_kp_deathgraph$Infection_origin <- "Hospital-origin"
  ho_extRep_blood_kp_deathgraph$Organism <- "Klebsiella pneumoniae"
  ho_extRep_blood_pa_deathgraph <- ho_extRep_blood_pa_deathgraph[,c("Antibiotic", "Mortality", "lowerCI", "upperCI")]
  ho_extRep_blood_pa_deathgraph$Infection_origin <- "Hospital-origin"
  ho_extRep_blood_pa_deathgraph$Organism <- "Pseudomonas aeruginosa"
  ho_extRep_blood_as_deathgraph <- ho_extRep_blood_as_deathgraph[,c("Antibiotic", "Mortality", "lowerCI", "upperCI")]
  ho_extRep_blood_as_deathgraph$Infection_origin <- "Hospital-origin"
  ho_extRep_blood_as_deathgraph$Organism <- "Acinetobacter spp."
  
  rpt6_table <- rbind(co_extRep_blood_sa_deathgraph, co_extRep_blood_es_deathgraph,
                      co_extRep_blood_sp_deathgraph, co_extRep_blood_ss_deathgraph,
                      co_extRep_blood_ec_deathgraph, co_extRep_blood_kp_deathgraph,
                      co_extRep_blood_pa_deathgraph, co_extRep_blood_as_deathgraph,
                      ho_extRep_blood_sa_deathgraph, ho_extRep_blood_es_deathgraph,
                      ho_extRep_blood_sp_deathgraph, ho_extRep_blood_ss_deathgraph,
                      ho_extRep_blood_ec_deathgraph, ho_extRep_blood_kp_deathgraph,
                      ho_extRep_blood_pa_deathgraph, ho_extRep_blood_as_deathgraph)
  
  rpt6_table <- rpt6_table[movecolumn(names(rpt6_table), "Infection_origin first")]
  rpt6_table <- rpt6_table[movecolumn(names(rpt6_table), "Antibiotic first")]
  rpt6_table <- rpt6_table[movecolumn(names(rpt6_table), "Organism first")]
  names(rpt6_table) <- c("Organism", "Antibiotic", "Infection_origin",
                            "Mortality", "Mortality_lower_95ci", "Mortality_upper_95ci")
  return(rpt6_table)
}

# Data stratified by infection origin, gender, and age group ####
fun_aggreg_table <- function(fun_aggre_tb_data, AST, nonAMR, AMR){
  a <- fun_aggre_tb_data
  b <- data.frame(a$organism3, AST, a$InfOri,
                  a$gender_cat, a$YearAge_label)
  names(b) <- c("Organism", "AMR", "Origin_of_infection", "Gender", "Age_group")
  b$AMR_str <- NULL
  b$AMR_str <- replace(b$AMR_str, b$AMR==0, values = nonAMR)
  b$AMR_str <- replace(b$AMR_str, b$AMR==1, values = AMR)
  c <- b %>%
    # order data by origin of infection, gender, and age in year
    dplyr::group_by(Organism, AMR_str, Origin_of_infection, Gender, Age_group) %>%
    dplyr::mutate(inanalysis=row_number()) %>%
    dplyr::summarise(count=max(inanalysis))
  return(c)
}
fun_aggreg_table2 <- function(fun_aggre_tb_data){
  a <- fun_aggre_tb_data
  a$AST3gcCarb <- NULL
  a$AST3gcCarb <- replace(a$AST3gcCarb, a$AST3gc==0 & a$ASTCarbapenem==0, values = "3GCS−carb−S")
  a$AST3gcCarb <- replace(a$AST3gcCarb, a$AST3gc==1 & a$ASTCarbapenem==0, values = "3GCNS−carb−S")
  a$AST3gcCarb <- replace(a$AST3gcCarb, a$AST3gc==1 & a$ASTCarbapenem==1, values = "3GCNS−carb−NS")
  b <- data.frame(a$organism3, a$AST3gcCarb , a$InfOri,
                  a$gender_cat, a$YearAge_label)
  names(b) <- c("Organism", "AST3gcCarb", "Origin_of_infection", "Gender", "Age_group")
  c <- b %>%
    # order data by origin of infection, gender, and age in year
    dplyr::group_by(Organism, AST3gcCarb, Origin_of_infection, Gender, Age_group) %>%
    dplyr::mutate(inanalysis=row_number()) %>%
    dplyr::summarise(count=max(inanalysis))
  return(c)
}

# Log for user ####
# Function for main text (bold)
fun_maintext_format_bold <- function(ypos, text1){
  text(x = 1, y = ypos, paste(text1), 
       cex = maintext_cex, col = "grey25", family=maintext_family, font=2, adj=0)
}

logfile_1 <- function(MicroData2) {
  maintext_cex = 1.2
  maintext_family = "sans"
  
  par(oma=c(0,0,0,0), mar=c(0,0,0,0))
  layout(matrix(c(1), nrow=1, ncol=1, byrow=TRUE))
  plot(c(0,12), c(0,66), ann = F, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
  ## missing specimen date
  fun_titletext_format(ypos=62, "Data verification log file")
  # Introduction sentence
  fun_maintext_format_bold(ypos=60, "This is a log file for user to verify the data set read by the AMASS application.")
  fun_maintext_format_reg(ypos=58, "Please review the following information carefully before interpreting the AMR surveillance report")
  fun_maintext_format_reg(ypos=57, "generated by the AMASS application.")
  # Section 1: missing data
  ### Microbiology data
  fun_maintext_format_bold(ypos=54.5, "Missing data- Microbiology data set (file name: microbiology_data)")
  text(x=1, y=53, paste("The number of observations with missing specimen date:", length(which(is.na(MicroData2$specimen_collection_date)))),
       cex = maintext_cex, col = "grey25", family=maintext_family, font=1, adj=0)
  text(x=1, y=51.5, paste("The number of observations with missing specimen type:", length(which(is.na(MicroData2$spctype)))),
       cex = maintext_cex, col = "grey25", family=maintext_family, font=1, adj=0)
  text(x=1, y=50, paste("The number of observations with missing culture result:", length(which(is.na(MicroData2$organism)))),
       cex = maintext_cex, col = "grey25", family=maintext_family, font=1, adj=0)
  text(x=1, y=48.5, paste("Example format of specimen date:", "[", MicroData2$specimen_collection_date[1], "]", "[", MicroData2$specimen_collection_date[2], "]"),
       cex = maintext_cex, col = "grey25", family=maintext_family, font=1, adj=0)
  ### Hospital admission data
  fun_maintext_format_bold(ypos=46.5, "Missing data- Hospital admission data set (file name: hospital_admission_data)")
  text(x=1, y=45, paste("The number of observations with missing admission date:", length(which(is.na(HospData2$date_of_admission)))),
       cex = maintext_cex, col = "grey25", family=maintext_family, font=1, adj=0)
  text(x=1, y=43.5, paste("The number of observations with missing discharge date:", length(which(is.na(HospData2$date_of_discharge)))),
       cex = maintext_cex, col = "grey25", family=maintext_family, font=1, adj=0)
  text(x=1, y=42, paste("The number of observations with missing outcome:", length(which(is.na(HospData2$discharge_status)))),
       cex = maintext_cex, col = "grey25", family=maintext_family, font=1, adj=0)
  text(x=1, y=40.5, paste("Example format of admission date:", "[", HospData2$date_of_admission[1], "]", "[", HospData2$date_of_admission[2], "]"),
       cex = maintext_cex, col = "grey25", family=maintext_family, font=1, adj=0)
  text(x=1, y=39, paste("Example format of discharge date:", "[", HospData2$date_of_discharge[1], "]", "[", HospData2$date_of_discharge[2], "]"),
       cex = maintext_cex, col = "grey25", family=maintext_family, font=1, adj=0)
  ### Merged data
  fun_maintext_format_bold(ypos=37, "Missing data- Merged data set (merged by the AMASS application)")
  text(x=1, y=35.5, paste("The number of observations with missing specimen date:", length(which(is.na(raw_HospMicroData_bsi$DateSpc)))),
       cex = maintext_cex, col = "grey25", family=maintext_family, font=1, adj=0)
  text(x=1, y=34, paste("The number of observations with missing admission date:", length(which(is.na(raw_HospMicroData_bsi$DateAdm)))),
       cex = maintext_cex, col = "grey25", family=maintext_family, font=1, adj=0)
  text(x=1, y=32.5, paste("The number of observations with missing discharge date:", length(which(is.na(raw_HospMicroData_bsi$DateDis)))),
       cex = maintext_cex, col = "grey25", family=maintext_family, font=1, adj=0)
  text(x=1, y=31, paste("The number of observations with missing age:", length(which(is.na(raw_HospMicroData_bsi$age_year)))),
       cex = maintext_cex, col = "grey25", family=maintext_family, font=1, adj=0)
  text(x=1, y=29.5, paste("The number of observations with missing gender:", length(which(is.na(raw_HospMicroData_bsi$gender)))),
       cex = maintext_cex, col = "grey25", family=maintext_family, font=1, adj=0)
  text(x=1, y=28, paste("The number of observations with missing infection origin data:", length(which(is.na(raw_HospMicroData_bsi$InfOri_hosp1)))),
       cex = maintext_cex, col = "grey25", family=maintext_family, font=1, adj=0)
}
# Section 2: tabulating the data
logfile_2 <- function(table_organism1,table_organism2,table_organism3,table_organism4) {
  ### Demographic data: list of organisms
  par(oma=c(0,0,0,0), mar=c(0,0,0,0))
  layout(matrix(c(1), nrow=1, ncol=1, byrow=TRUE))
  plot(c(0,12), c(0,66), ann = F, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
  ## Title
  fun_titletext_format(ypos=62, "Data summary log file")
  # Introduction sentence
  fun_maintext_format_bold(ypos=60, "This a log file for user to verify the data set read by the AMASS application")
  fun_maintext_format_reg(ypos=59, "Please review the following information carefully before interpreting the AMR surveillance report")
  fun_maintext_format_reg(ypos=58, "generated by the AMASS application.")
  fun_maintext_format_bold(ypos=56, "Table S1: List of organisms in the microbiology_data file")
  # List1
  log_tb_organism1 <- tableGrob(table_organism1, row=NULL,
                                theme = ttheme_default(core = list(fg_params=list(cex = maintext_cex))))
  grid.draw(log_tb_organism1)
  ## format the page
  par(oma=c(0,0,0,0), mar=c(0,0,0,0))
  layout(matrix(c(1), nrow=1, ncol=1, byrow=TRUE))
  plot(c(0,12), c(0,66), ann = F, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
  ## Title
  fun_titletext_format(ypos=62, "Data summary log file")
  # Introduction sentence
  fun_maintext_format_bold(ypos=60, "This a log file for user to verify the data set read by the AMASS application")
  fun_maintext_format_reg(ypos=59, "Please review the following information carefully before interpreting the AMR surveillance report")
  fun_maintext_format_reg(ypos=58, "generated by the AMASS application.")
  fun_maintext_format_bold(ypos=56, "Table S1: List of organisms in the microbiology_data file")
  # List 2
  log_tb_organism2 <- tableGrob(table_organism2, row=NULL,
                                theme = ttheme_default(core = list(fg_params=list(cex = maintext_cex))))
  grid.draw(log_tb_organism2)
  ## format the page
  par(oma=c(0,0,0,0), mar=c(0,0,0,0))
  layout(matrix(c(1), nrow=1, ncol=1, byrow=TRUE))
  plot(c(0,12), c(0,66), ann = F, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
  ## Title
  fun_titletext_format(ypos=62, "Data summary log file")
  # Introduction sentence
  fun_maintext_format_bold(ypos=60, "This a log file for user to verify the data set read by the AMASS application")
  fun_maintext_format_reg(ypos=59, "Please review the following information carefully before interpreting the AMR surveillance report")
  fun_maintext_format_reg(ypos=58, "generated by the AMASS application.")
  fun_maintext_format_bold(ypos=56, "Table S1: List of organisms in the microbiology_data file")
  # List 3
  log_tb_organism3 <- tableGrob(table_organism3, row=NULL,
                                theme = ttheme_default(core = list(fg_params=list(cex = maintext_cex))))
  grid.draw(log_tb_organism3)
  ## format the page
  par(oma=c(0,0,0,0), mar=c(0,0,0,0))
  layout(matrix(c(1), nrow=1, ncol=1, byrow=TRUE))
  plot(c(0,12), c(0,66), ann = F, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
  ## Title
  fun_titletext_format(ypos=62, "Data summary log file")
  # Introduction sentence
  fun_maintext_format_bold(ypos=60, "This a log file for user to verify the data set read by the AMASS application")
  fun_maintext_format_reg(ypos=59, "Please review the following information carefully before interpreting the AMR surveillance report")
  fun_maintext_format_reg(ypos=58, "generated by the AMASS application.")
  fun_maintext_format_bold(ypos=56, "Table S1: List of organisms in the microbiology_data file")
  # List 4
  log_tb_organism4 <- tableGrob(table_organism4, row=NULL,
                                theme = ttheme_default(core = list(fg_params=list(cex = maintext_cex))))
  grid.draw(log_tb_organism4)
}
### Demographic data: gender of the population under survey (merged data)
logfile_3 <- function(table_gender,table_age) {
  ## format the page
  par(oma=c(0,0,0,0), mar=c(0,0,0,0))
  layout(matrix(c(1), nrow=1, ncol=1, byrow=TRUE))
  plot(c(0,12),xc(0,66), ann = F, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
  ## Title
  fun_titletext_format(ypos=62, "Data summary log file")
  # Introduction sentence
  fun_maintext_format_bold(ypos=60, "This a log file for user to verify the data set read by the AMASS application")
  fun_maintext_format_reg(ypos=59, "Please review the following information carefully before interpreting the AMR surveillance report")
  fun_maintext_format_reg(ypos=58, "generated by the AMASS application.")
  fun_maintext_format_bold(ypos=56, "Table S2: Gender of the population under survey (merged data)")
  log_tb_gender <- tableGrob(table_gender, row=NULL,
                             theme = ttheme_default(core = list(fg_params=list(cex = maintext_cex))))
  grid.draw(log_tb_gender)
  ### Demographic data: distribution of age in the population under survey (merged data)
  ## format the page
  par(oma=c(0,0,0,0), mar=c(0,0,0,0))
  layout(matrix(c(1), nrow=1, ncol=1, byrow=TRUE))
  plot(c(0,12), c(0,66), ann = F, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
  ## Title
  fun_titletext_format(ypos=62, "Data summary log file")
  # Introduction sentence
  fun_maintext_format_bold(ypos=60, "This a log file for user to verify the data set read by the AMASS application")
  fun_maintext_format_reg(ypos=59, "Please review the following information carefully before interpreting the AMR surveillance report")
  fun_maintext_format_reg(ypos=58, "generated by the AMASS application.")
  fun_maintext_format_bold(ypos=56, "Table S3: Age distribution of the population under survey (merged data)")
  log_tb_age <- tableGrob(table_age, row=NULL,
                          theme = ttheme_default(core = list(fg_params=list(cex = maintext_cex))))
  grid.draw(log_tb_age)
}

