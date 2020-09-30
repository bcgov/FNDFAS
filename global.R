source("concat_list_diagnosis.R")
source("concat_list_prescription.R")

diag_code <- function (element, value){
  T_val <- switch (element,
                # thresholds for diagnosis categories
                
                "N"   = c(1.00, 1.15, 1.35, NA),
                "P"   = c(0.08, 0.10, 0.12, NA),
                "K"   = c(0.30, 0.35, 0.40, NA),
                "Ca"  = c(0.06, 0.08, 0.10, NA),
                "Mg"  = c(0.04, 0.06, 0.08, NA),
                "S"   = c(0.06, 0.08, 0.10, NA),
                "SO4" = c( 40, 60, 80, NA),
                "Cu"  = c(1, 3, NA, NA),
                "Zn"  = c(10, 15, NA, NA),
                "Fe"  = c(20, 30, NA, NA),
                "Mg"  = c(15, 25, NA, NA),
                "B"  = c(3, 6, 12, 15),
                "N:P"  = c(9, 11, 13, NA),
                "N:K"  = c(2.5, 3.5, 4.5, NA),
                "N:Mg"  = c(15, 20, 30, NA),
                "N:S"  = c(14, 20, 25, NA) )
  
  sum(c(value >= T_val[1], value >= T_val[2], value >= T_val[3], value >= T_val[4]), na.rm = TRUE) + 1
  
}




#---------------------------------------------------------------------------------------------------------------
# This procedure is for the diagnosis summary table in the Diagnosis/Prescription
# Note that some interpretation vectors have the inverse order of severity
#---------------------------------------------------------------------------------------------------------------

Int_type1 <- c("Severely deficient",
               "Moderately to severely deficient",
               "Slightly to moderately deficient",
               "Adequate")
Int_type2 <- c("Probable deficiency",
               "Possible deficiency",
               "No deficiency")

Int_type3 <- c("Severely deficient",
               "Probable deficiency",
               "Possible deficiency",
               "Likely not deficient",
               "No deficiency")  
Int_type4 <- c("No",
               "Possible slight",
               "Slight to moderate",
               "Moderate to severe")
Int_type5 <- c("No",
               "Slight to moderate",
               "Moderate to severe",
               "Severe")

Interpret <- function (element, value){
if (is.na(value)) return("N/A") else {
d_code <- diag_code(element, value)
T_val <- switch (element, 
                 "N"   = Int_type1[d_code],
                 "P"   = Int_type1[d_code],
                 "K"   = Int_type1[d_code],
                 "Ca"  = Int_type1[d_code],
                 "Mg"  = Int_type1[d_code],
                 "S"   = Int_type1[d_code],
                 "SO4" = Int_type1[d_code],
                 "Cu"  = Int_type2[d_code],
                 "Zn"  = Int_type2[d_code],
                 "Fe"  = Int_type2[d_code],
                 "Mn"  = Int_type2[d_code],
                 "B"  =  Int_type3[d_code],
                 "N:P"  = Int_type4[d_code],
                 "N:K"  = Int_type4[d_code],
                 "N:Mg"  = Int_type4[d_code],
                 "N:S"  = Int_type5[d_code] )
T_val
}
}


#---------------------------------------------------------------------------------------------------------------
# This procedure is for concatenate phrases for "Comments" section.
# This procedure is basically same with the above procedure.
#---------------------------------------------------------------------------------------------------------------

Int_type11 <- c("severe",
               "moderate to severe",
               "slight to moderate",
               "no")
Int_type21 <- c("probable",
               "possible",
               "no")

Int_type31 <- c("severe",
               "probable",
               "possible",
               "likely no",
               "no")  
Int_type41 <- c("no",
               "possible slight",
               "slight to moderate",
               "moderate to severe")
Int_type51 <- c("no",
               "slight to moderate",
               "moderate to severe",
               "severe")

Intp_comments <- function (element, value){
  if (is.na(value)) return("N/A") else {
    d_code <- diag_code(element, value)
    T_val <- switch (element, 
                     "N"   = Int_type11[d_code],
                     "P"   = Int_type11[d_code],
                     "K"   = Int_type11[d_code],
                     "Ca"  = Int_type11[d_code],
                     "Mg"  = Int_type11[d_code],
                     "S"   = Int_type11[d_code],
                     "SO4" = Int_type11[d_code],
                     "Cu"  = Int_type21[d_code],
                     "Zn"  = Int_type21[d_code],
                     "Fe"  = Int_type21[d_code],
                     "Mn"  = Int_type21[d_code],
                     "B"  =  Int_type31[d_code],
                     "N:P"  = Int_type41[d_code],
                     "N:K"  = Int_type41[d_code],
                     "N:Mg"  = Int_type41[d_code],
                     "N:S"  = Int_type51[d_code] )
    T_val
  }
}




write_super_prescription <- function (Prev_fert, N_value, Lab_type, SO4_value, N_S_value, Cu_value, 
                                      Zn_value, Fe_value, Mn_value, P_value, N_P_value,
                                      K_value, N_K_value, Ca_value, Mg_value, N_Mg_value, B_value){
  comment_list <- list()
  
  if (Prev_fert == "No"){
    if (!is.na(N_value) & Interpret("N", N_value) == "Adequate" ) {
        comment_list[[length(comment_list)+1]] <- prescription_properties$Nitrogen_Fertilization_none  
      } 
    if (!is.na(N_value) & Interpret("N", N_value) != "Adequate" ) {
        comment_list[[length(comment_list)+1]] <- prescription_properties$Nitrogen_Fertilization_deficient  
      } 
    if (Lab_type != "Others"){
        comment_list[[length(comment_list)+1]] <- switch (Interpret("SO4", SO4_value),
                            "Severely deficient"= prescription_properties$Sulphate_severe,
                            "Moderately to severely deficient"= prescription_properties$Sulphate_moderate,
                            "Slightly to moderately deficient"= prescription_properties$Sulphate_slight,
                            "Adequate" =prescription_properties$Sulphate_none)
        
        comment_list[[length(comment_list)+1]] <- switch ( sum(c(N_S_value > 12, N_S_value > 14)) + 1,
                            prescription_properties$N_S_none1,
                            prescription_properties$N_S_none2,
                            prescription_properties$N_S_deficient)
      }
    if (Lab_type == "Others"){
        comment_list[[length(comment_list)+1]] <- prescription_properties$Other_Labs_Sulphur
    }
    if (!is.na(Cu_value) & Interpret("Cu", Cu_value) == "Probable deficiency" ){
      comment_list[[length(comment_list)+1]] <- prescription_properties$Copper_Deficiency_probable
    }
    if (!is.na(Zn_value) & Interpret("Zn", Zn_value) == "Probable deficiency" ){
        comment_list[[length(comment_list)+1]] <- prescription_properties$Zinc_Deficiency_probable
    }
    if (!is.na(Fe_value) & Interpret("Fe", Fe_value) == "Probable deficiency" ){
      comment_list[[length(comment_list)+1]] <- prescription_properties$Iron_Deficiency_probable
    }
    if (!is.na(Mn_value) & Interpret("Mn", Mn_value) == "Probable deficiency" ){
      comment_list[[length(comment_list)+1]] <- prescription_properties$Manganese_Deficiency_probable
    }
    if ( (!is.na(P_value) & (!is.na(N_P_value)) ) & 
        (Interpret("P", P_value) %in% c("Severely deficient", "Moderately to severely deficient") | 
        (Interpret("N:P", N_P_value) %in% c("Slight to moderate", "Moderate to severe") ) ) ) {
      comment_list[[length(comment_list)+1]] <- prescription_properties$Phosphorus_Deficiency_severe_or_moderate
    }
    if ( (!is.na(K_value) & (!is.na(N_K_value)) ) &
        (Interpret("K", K_value) %in% c("Severely deficient", "Moderately to severely deficient") | 
        (Interpret("N:K", N_K_value) %in% c("Slight to moderate", "Moderate to severe") ) ) ) {
      comment_list[[length(comment_list)+1]] <- prescription_properties$Potassium_Deficiency_severe_or_moderate
    }
    
    if (!is.na(Ca_value) & Interpret("Ca", Ca_value) %in% c("Severely deficient", "Moderately to severely deficient") ) {
      comment_list[[length(comment_list)+1]] <- prescription_properties$Calcium_Deficiency_severe_or_moderate
    }
    
    if ( (!is.na(Mg_value) & (!is.na(N_Mg_value)) ) &
        (Interpret("Mg", Mg_value) %in% c("Severely deficient", "Moderately to severely deficient") | 
        (Interpret("N:Mg", N_Mg_value) %in% c("Slight to moderate", "Moderate to severe") ) ) ) {
      comment_list[[length(comment_list)+1]] <- prescription_properties$Magnesium_Deficiency_severe_or_moderate
    }
    if ((!is.na(Cu_value) & Interpret("Cu", Cu_value) == "Possible deficiency" )|
        (!is.na(Zn_value) & Interpret("Zn", Zn_value) == "Possible deficiency" )|
        (!is.na(Fe_value) & Interpret("Fe", Fe_value) == "Possible deficiency" )| 
        (!is.na(Mn_value) & Interpret("Mn", Mn_value) == "Possible deficiency" ) ) {
      comment_list[[length(comment_list)+1]] <- prescription_properties$Micronutrients_possible
    }
    if ((!is.na(P_value) & Interpret("P", P_value) == "Slightly to moderately deficient") | 
        (!is.na(K_value) & Interpret("K", K_value) == "Slightly to moderately deficient") |
        (!is.na(Ca_value) & Interpret("Ca", Ca_value) == "Slightly to moderately deficient") |
        (!is.na(Mg_value) & Interpret("Mg", Mg_value) == "Slightly to moderately deficient") |
        (!is.na(N_P_value) & Interpret("N:P", N_P_value) == "Slightly to moderately deficient") | 
        (!is.na(N_K_value) & Interpret("N:K", N_K_value) == "Slightly to moderately deficient") |
        (!is.na(N_Mg_value) & Interpret("N:Mg", N_Mg_value) == "Slightly to moderately deficient" ) ) {
      comment_list[[length(comment_list)+1]] <- prescription_properties$Macronutrients_and_Ratio_slight
    }
    if (!is.na(B_value) & Interpret("B", B_value) != "No deficiency"){
      comment_list[[length(comment_list)+1]] <- switch (Interpret("B", B_value),
                                                        "Severely deficient"= prescription_properties$Boron_severe,
                                                        "Probable deficiency"= prescription_properties$Boron_likely,
                                                        "Possible deficiency"= prescription_properties$Boron_possible,
                                                        "Likely not deficient"= prescription_properties$Boron_not_likely)
    }
  } else if (Prev_fert == "Yes") {
    comment_list[[length(comment_list)+1]] <- "Since the site was fertilized within the last two years, there is no prescription available."
  }
  
  if (length(comment_list) == 0) comment_list[[1]] <- "There is not enough data for fertilization prescription."
  
  HTML(paste("<li>", unlist(comment_list), collapse ="<br/>"))
}



write_super_comment <- function (Lab_type, Crown, B_value, N_K_value, Cr_pos, samp_date, Diag_base,
                                 Comp_Number, Leaf_age, Pertinent){
  comment_list <- list()

  if (Lab_type != "Others"){
    comment_list[[length(comment_list)+1]] <- site_properties$Laboratory_Laboratory_Output
  } else if (Lab_type == "Others") {
    comment_list[[length(comment_list)+1]] <- site_properties$Laboratory_Other_Labs
  }
  
  if (Crown !="" & Crown < 20) {
    comment_list[[length(comment_list)+1]] <- site_properties$Percent_Live_Crown_Crown
  }

  if (!is.na(B_value) & Interpret("B", B_value) == "Probable deficiency" ){
    comment_list[[length(comment_list)+1]] <- site_properties$Boron_probable
  }
  
  if (!is.na(B_value) & Interpret("B", B_value) == "Severely deficient" ){
    comment_list[[length(comment_list)+1]] <- site_properties$Boron_severe
  } 

  if (!is.na(N_K_value) & Interpret("N:K", N_K_value) == "Slightly to moderately deficient" ){
    comment_list[[length(comment_list)+1]] <- site_properties$Ratio_N_K_Deficiency_Slight
  } 
  
  if (Cr_pos %in% c("Middle", "Lower") ) {
    comment_list[[length(comment_list)+1]] <- site_properties$Crown_Position_Middle_Lower
  } 
  
  if (length(samp_date)!=0) {
    sample_month_day_numeric <- as.numeric(format(as.Date(samp_date), "%m%d"))   # trick for date range filter
    if  (sample_month_day_numeric > 315 & sample_month_day_numeric < 915) {
           # sampling date from March 15 to Sep 15 is not appropriate
    comment_list[[length(comment_list)+1]] <- site_properties$Sampling_Month_incorrect
    }
  }
  
  if (Diag_base == "Indv_sam" ) {
    comment_list[[length(comment_list)+1]] <- site_properties$Diagnosis_is_based_on_ind_sample
  } 
  
  if (!is.na(Comp_Number) & Comp_Number < 10) {
    comment_list[[length(comment_list)+1]] <- site_properties$Composite_Number_Comp_Number
  } 
  
  if (Leaf_age != "Cur_yr") {
    comment_list[[length(comment_list)+1]] <- site_properties$Foliage_Age_Not_Current_Year
  } 
  
  if ("Very wet or dry soil conditions" %in%  Pertinent) {
    comment_list[[length(comment_list)+1]] <- site_properties$Pertinent_site_stand_information_poorly_drained
  } 
  
  if ("Needle cast of blight" %in%  Pertinent) {
    comment_list[[length(comment_list)+1]] <- site_properties$Pertinent_site_stand_information_severe_needle
  } 
  
  if ("Stem cankers or rusts" %in%  Pertinent) {
    comment_list[[length(comment_list)+1]] <- site_properties$Pertinent_site_stand_information_stem_cankers
  } 
  
  if ("Animal damage to crop trees" %in%  Pertinent) {
    comment_list[[length(comment_list)+1]] <- site_properties$Pertinent_site_stand_information_small_damage
  } 
 
  if (length(comment_list) == 0) comment_list[[1]] <- "There is not enough information for comments."
  
  HTML(paste("<li>", unlist(comment_list), collapse ="<br/>"))

  }




