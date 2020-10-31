
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

# note that Int_type4 and 5 have reversed order
Int_type4 <- c("No deficiency",
               "Possible slight",
               "Slight to moderate",
               "Moderate to severe")
Int_type5 <- c("No deficiency",
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

# note that Int_type41 and 51 have reversed order
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


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# This is the function for nutrient diagnosis.
# This will also work as the base comments for nutrient ratio (e.g., "N:P") disgnosis.
#-------------------------------------------------------------------------------------------------------------------------------------------------------

concat_comments_nutrients <- function(element, value, fertilization, lab_type){
  if (is.na(value)) {
  comment <- paste("The analysis result for", element, "was not entered.", collapse =" ")
  }  
  
  else if (element == "N" & fertilization =="No") {
      comment1 <- switch (diag_code(element, value), 
                          'that will likely negatively affect stand performance.',
                          'that will likely negatively affect stand performance.',
                          'that may negatively affect stand performance.',
                          'level of nutrient condition.')
      comment <- paste("Foliar", element, "concentration indicates", Intp_comments(element, value), element, "deficiency", comment1, collapse =" ")
  }
  
  else if (element == "N" & fertilization =="Yes") {
      comment1 <- switch (diag_code(element, value), 
                          'deficiency, which is surprising given the recent fertilization of this stand.  Because the uptake of applied N is apparently poor, growth response following fertilization may be relatively small.',
                          'deficiency, which is surprising given the recent fertilization of this stand.  Because the uptake of applied N is apparently poor, growth response following fertilization may be relatively small.',
                          'deficiency.',
                          'deficiency.')
      comment <- paste("Foliar", element, "concentration indicates", Intp_comments(element, value), element, comment1, collapse =" ")
  }
  
  else if (element %in% c("P", "K", "Ca", "Mg") & fertilization =="No" ) {
      comment_list <- list()  
      comment_list[[1]]  <- "Foliar"
      comment_list[[2]]  <- element
      comment_list[[3]]  <- "concentration indicates"
      comment_list[[4]]  <- Intp_comments(element, value)
      comment_list[[5]]  <- element
      comment_list[[6]]  <- "deficiency."       # this will be appended depending on conditions
      if (diag_code(element, value)==1) {
          comment_list[[6]]  <- "deficiency that will likely negatively affect stand performance and/or growth response following N fertilization.  Severe"
          comment_list[[7]]  <- element
          comment_list[[8]]  <- "deficiencies are uncommon in the B.C. interior, and growth responses to"
          comment_list[[9]]  <- element
          comment_list[[10]]  <- "fertilization are not well documented. Therefore, it is suggested that a tree nutrition specialist be consulted. Deficiency symptoms may, or may not, be visible. Refer to 'Visual Deficiency Symptoms' for common and noticeable symptoms of"
          comment_list[[11]]  <- element
          comment_list[[12]]  <- "deficiency."
          } 
      else if  (diag_code(element, value)==2) {
          comment_list[[6]]  <- "deficiency that may negatively affect stand performance and/or growth response following N fertilization. Because growth responses to"
          comment_list[[7]]  <- element
          comment_list[[8]]  <- "fertilization are not well documented. Therefore, it is suggested that a tree nutrition specialist be consulted. Deficiency symptoms may, or may not, be visible. Refer to 'Visual Deficiency Symptoms' for common and noticeable symptoms of"
          comment_list[[9]]  <- element
          comment_list[[10]]  <- "deficiency."  
          }
      else if  (diag_code(element, value)==3){
          comment_list[[6]]  <- "deficiency. However, results from research trials in the B.C. interior indicate that"
          comment_list[[7]]  <- element
          comment_list[[8]]  <- "status will likely not have a serious negative impact on stand performance and/or growth response following N fertilization."
          }
  comment <- paste(unlist(comment_list), collapse =" ")  
  }
      
  else if (element %in% c("P", "K", "Mg") & fertilization =="Yes" ) {
     comment_list <- list()  
     comment_list[[1]]  <- "Foliar"
     comment_list[[2]]  <- element
     comment_list[[3]]  <- "concentration indicates"
     comment_list[[4]]  <- Intp_comments(element, value)
     comment_list[[5]]  <- element
     comment_list[[6]]  <- "deficiency."       # this will be appended depending on conditions
     if (diag_code(element, value)==1) {
          comment_list[[6]]  <- "deficiency, likely induced or exacerbated by N fertilization, that may negatively affect stand performance and/or growth response following N fertilization.  Deficiency symptoms are likely visible.  Refer to 'Visual Deficiency Symptoms' for common and noticeable symptoms of"
          comment_list[[7]]  <- element
          comment_list[[8]]  <- "deficiency."
          } 
     else if (diag_code(element, value)==2) {
          comment_list[[6]]  <- "deficiency, likely induced or exacerbated by N fertilization, that may negatively affect stand performance and/or growth response following N fertilization.  Deficiency symptoms may, or may not, be visible.  Refer to 'Visual Deficiency Symptoms'for common and noticeable symptoms of"
          comment_list[[7]]  <- element
          comment_list[[8]]  <- "deficiency."
          }
     else if  (diag_code(element, value)==3){
          comment_list[[6]]  <- "deficiency, , likely induced by N fertilization.  However, results from research trials in the B.C. interior indicate that"
          comment_list[[7]]  <- element
          comment_list[[8]]  <- "status will likely not have a serious negative impact on stand performance and/or growth response following N fertilization."
          }
  comment <- paste(unlist(comment_list), collapse =" ")  
  }      

  else if (element =="Ca" & fertilization =="Yes" ) {      
     comment_list <- list()  
     comment_list[[1]]  <- "Foliar"
     comment_list[[2]]  <- element
     comment_list[[3]]  <- "concentration indicates"
     comment_list[[4]]  <- Intp_comments(element, value)
     comment_list[[5]]  <- element
     comment_list[[6]]  <- "deficiency."       # this will be appended depending on conditions
     if (diag_code(element, value)==1) {
         comment_list[[6]]  <- "deficiency, likely induced or exacerbated by N fertilization, that may negatively affect stand performance and/or growth response following N fertilization.  Deficiency symptoms are likely visible.  Refer to 'Visual Deficiency Symptoms' for common and noticeable symptoms of"
         comment_list[[7]]  <- element
         comment_list[[8]]  <- "deficiency."
         } 
     else if (diag_code(element, value)==2) {
         comment_list[[6]]  <- "deficiency, likely induced or exacerbated by N fertilization, that may negatively affect stand performance and/or growth response following N fertilization."
         }
     else if (diag_code(element, value)==3){
         comment_list[[6]]  <- "deficiency, likely induced by N fertilization.  However, results from research trials in the B.C. interior indicate that Ca status will likely not have a serious negative impact on stand performance and/or growth response following N fertilization."
         }
  comment <- paste(unlist(comment_list), collapse =" ")     
  } 
  
  else if (element == "S" & fertilization =="No") {
      comment1 <- switch (diag_code(element, value), 
                          'However, foliar SO4 and N/S ratio have greater diagnostic value than total S.',
                          'However, foliar SO4 and N/S ratio have greater diagnostic value than total S.',
                          'However, foliar SO4 and N/S ratio have greater diagnostic value than total S.',
                          '')
  comment <- paste("Foliar", element, "concentration indicates", Intp_comments(element, value), element, 'deficiency.', comment1, collapse =" ")
  }
  
  else if (element == "S" & fertilization =="Yes") {
      comment1 <- switch (diag_code(element, value), 
                            'deficiency, likely induced or exacerbated by N fertilization.  However, foliar N/S ratio has greater diagnostic value than total S.',
                            'deficiency, likely induced or exacerbated by N fertilization.  However, foliar N/S ratio has greater diagnostic value than total S.',
                            'deficiency, likely induced or exacerbated by N fertilization.  However, foliar N/S ratio has greater diagnostic value than total S.',
                            'deficiency.')
  comment <- paste("Foliar", element, "concentration indicates", Intp_comments(element, value), element, comment1, collapse =" ")
  }      
      
      
  else if (element == "SO4" & fertilization =="No" & lab_type != "Others") {
      comment1 <- switch (diag_code(element, value), 
                          'Results from research trials in the B.C. interior indicate that growth response following N fertilization will be minimal unless S is added to the fertilizer prescription.',
                          'Results from research trials in the B.C. interior indicate that growth response following N fertilization will likely be small unless S is added to the fertilizer prescription.',
                          'Results from research trials in the B.C. interior indicate that growth response following N fertilization may be improved if S is added to the fertilizer prescription.',
                          'Results from research trials in the B.C. interior indicate there is low risk of inducing S deficiency following N fertilization.')
  comment <- paste("Foliar", element, "concentration indicates", Intp_comments(element, value), element, "deficiency.", comment1, collapse =" ")    
  }
  
  else if (element == "SO4" & fertilization =="No" & lab_type == "Others") {
    comment1 <- switch (diag_code(element, value), 
                        'Results from research trials in the B.C. interior indicate that growth response following N fertilization will be minimal unless S is added to the fertilizer prescription.  In the absence of "normalized" foliar S and/or SO4 data, interpretative criteria used for S diagnosis may not apply to analytical methods used by the laboratory to which this foliage sample was submitted.',
                        'Results from research trials in the B.C. interior indicate that growth response following N fertilization will likely be small unless S is added to the fertilizer prescription.  In the absence of "normalized" foliar S and/or SO4 data, interpretative criteria used for S diagnosis may not apply to analytical methods used by the laboratory to which this foliage sample was submitted.',
                        'Results from research trials in the B.C. interior indicate that growth response following N fertilization may be improved if S is added to the fertilizer prescription.  In the absence of "normalized" foliar S and/or SO4 data, interpretative criteria used for S diagnosis may not apply to analytical methods used by the laboratory to which this foliage sample was submitted.',
                        'Results from research trials in the B.C. interior indicate there is low risk of inducing S deficiency following N fertilization.  In the absence of "normalized" foliar S and/or SO4 data, interpretative criteria used for S diagnosis may not apply to analytical methods used by the laboratory to which this foliage sample was submitted.')
  comment <- paste("Foliar", element, "concentration indicates", Intp_comments(element, value), element, "deficiency.", comment1, collapse =" ")    
  }
  else if (element == "SO4" & fertilization == "Yes"){
  comment <- paste("Foliar", element, "concentration indicates", Intp_comments(element, value), element, "deficiency.", "\n", "Foliar SO4 concentration is not a useful measure of S status in fertilized foliage.")  
  }  
      
  else if (element %in% c("Cu", "Zn", "Fe", "Mn") & fertilization =="No"){
      comment_list <- list()  
      comment_list[[1]]  <- "Foliar"
      comment_list[[2]]  <- element
      comment_list[[3]]  <- "concentration indicates"
      comment_list[[4]]  <- Intp_comments(element, value)
      comment_list[[5]]  <- element
      comment_list[[6]]  <- "deficiency." 
      
      if (diag_code(element, value)==1) {
          comment_list[[6]]  <- "deficiency that may negatively affect stand performance and/or growth response following N fertilization. Probable"
          comment_list[[7]]  <- element
          comment_list[[8]]  <- "deficiencies are uncommon in the B.C. interior, and growth responses to"
          comment_list[[9]]  <- "fertilization are not well documented. Therefore, it is suggested that a tree nutrition specialist be consulted. Deficiency symptoms may, or may not, be visible. Refer to 'Visual Deficiency Symptoms' for common and noticeable symptoms of"
          comment_list[[10]]  <- element
          comment_list[[11]]  <- "deficiency."  
          }
      else if (diag_code(element, value)==2) {
          comment_list[[6]]  <- "deficiency. However, results from research trials in the B.C. interior indicate that"
          comment_list[[7]]  <- element
          comment_list[[8]]  <- "status will likely not have a serious negative impact on stand performance and/or growth response following N fertilization."
          }
  comment <- paste(unlist(comment_list), collapse =" ")
  }
    
  else if (element %in% c("Cu", "Zn", "Fe", "Mn") & fertilization =="Yes"){
      comment_list <- list()  
      comment_list[[1]]  <- "Foliar"
      comment_list[[2]]  <- element
      comment_list[[3]]  <- "concentration indicates"
      comment_list[[4]]  <- Intp_comments(element, value)
      comment_list[[5]]  <- element
      comment_list[[6]]  <- "deficiency." 
        
      if (diag_code(element, value)==1) {
          comment_list[[6]]  <- "deficiency that may negatively affect stand performance and/or growth response following N fertilization.  Deficiency symptoms may, or may not, be visible.  Refer to 'Visual Deficiency Symptoms' for common and noticeable symptoms of"
          comment_list[[7]]  <- element
          comment_list[[8]]  <- "deficiency."
          }
      else if (diag_code(element, value)==2) {
          comment_list[[6]]  <- "deficiency. However, results from research trials in the B.C. interior indicate that"
          comment_list[[7]]  <- element
          comment_list[[8]]  <- "status will likely not have a serious negative impact on stand performance and/or growth response following N fertilization."
          }
  comment <- paste(unlist(comment_list), collapse =" ")
  } 

  else if (element == "B" & fertilization =="No") {
    comment_list <- list()  
    comment_list[[1]]  <- "Foliar"
    comment_list[[2]]  <- element
    comment_list[[3]]  <- "concentration indicates"
    comment_list[[4]]  <- Intp_comments(element, value)
    comment_list[[5]]  <- element
    comment_list[[6]]  <- "deficiency."
    
    if (diag_code(element, value)==1){
      comment_list[[6]]  <- "deficiency, likely induced or exacerbated by N fertilization. Results from research trials in the B.C. interior indicate that risk of reduced height increment and/or top dieback is high. Deficiency symptoms are likely visible. Refer to 'Visual Deficiency Symptoms' for common and noticeable symptoms of B deficiency."  
      }
    else if (diag_code(element, value)==2) {
      comment_list[[6]]  <- "deficiency, likely induced or exacerbated by N fertilization.  Results from research trials in the B.C. interior indicate a moderate risk of  reduced height increment and/or top dieback. Deficiency symptoms may, or may not, be visible. Refer to 'Visual Deficiency Symptoms' for common and noticeable symptoms of B deficiency."
      }
    else if (diag_code(element, value)==3) {
      comment_list[[6]]  <- "deficiency. Although B status will likely not have a serious negative impact on stand performance or growth response following N fertilization, there may be a small negative impact on height increment."
      }
  comment <- paste(unlist(comment_list), collapse =" ")
  }

  
  else if (element == "B" & fertilization =="Yes") {
    comment_list <- list()  
    comment_list[[1]]  <- "Foliar"
    comment_list[[2]]  <- element
    comment_list[[3]]  <- "concentration indicates"
    comment_list[[4]]  <- Intp_comments(element, value)
    comment_list[[5]]  <- element
    comment_list[[6]]  <- "deficiency."
    
    if (diag_code(element, value)==1){
      comment_list[[6]]  <- "deficiency, likely induced or exacerbated by N fertilization. Results from research trials in the B.C. interior indicate that risk of reduced height increment and/or top dieback is high. Deficiency symptoms are likely visible. Refer to 'Visual Deficiency Symptoms' for common and noticeable symptoms of B deficiency."  
    }
    else if (diag_code(element, value)==2) {
      comment_list[[6]]  <- "deficiency, likely induced or exacerbated by N fertilization. Results from research trials in the B.C. interior indicate a moderate risk of reduced height increment and/or top dieback.  Deficiency symptoms may, or may not, be visible. Refer to 'Visual Deficiency Symptoms' for common and noticeable symptoms of B deficiency."
    }
    else if (diag_code(element, value)==3) {
      comment_list[[6]]  <- "deficiency. Although B status will likely not have a serious negative impact on stand performance or growth response following N fertilization, there may be a small negative impact on height increment."
    }
  comment <- paste(unlist(comment_list), collapse =" ")
  }  
  
comment
   
}



#-------------------------------------------------------------------------------------------------------------------------------------------------------
# This is the function for nutrient ratio (e.g., "N:P") diagnosis.
# First, return the diagnosis of given nutrient and append additional statements.
#-------------------------------------------------------------------------------------------------------------------------------------------------------


concat_comments_nutrients_ratio <- function (ratio_element, ratio_value, base_value, SO4_value, fertilization){
  
  base_element <- switch (ratio_element, 
                          "N:P"  = "P",
                          "N:K"  = "K",
                          "N:Mg"  = "Mg",
                          "N:S"  = "S")
  
  if (is.na(ratio_value)|is.na(base_value)) {
    paste("The analysis results for", ratio_element, "and/or", base_element, "were not entered.", collapse =" ")} else {
  
       
      comment_list <- list()  
      comment_list[[1]]  <- "Foliar"
      comment_list[[2]]  <- ratio_element
      comment_list[[3]]  <- "ratio indicates"
      comment_list[[4]]  <- Intp_comments(ratio_element, ratio_value)
      comment_list[[5]]  <- base_element  
      comment_list[[6]]  <- "deficiency."     # below code replace this and thereafter
      
      if (base_element %in% c("P", "K", "Mg") & diag_code(ratio_element, ratio_value) == 4 ){       # severe
        if (fertilization == "No"){
        comment_list[[6]]  <- "deficiency that will likely negatively affect stand performance and/or growth response following N fertilization."
        } else {
          comment_list[[6]]  <-  "deficiency, likely induced or exacerbated by N fertilization."
        }
      }
      
      if (base_element %in% c("P", "K", "Mg") & diag_code(ratio_element, ratio_value) == 3 ){       # moderate
        if (fertilization == "No"){
        comment_list[[6]]  <- "deficiency that may negatively affect stand performance and/or growth response following N fertilization."
        } else {
          comment_list[[6]]  <- "deficiency, likely induced or exacerbated by N fertilization."
        }
      }
      if (base_element %in% c("P", "K", "Mg") & diag_code(ratio_element, ratio_value) == 2 & fertilization =="Yes" ){       # slightly
          comment_list[[6]]  <- "deficiency, , likely induced by N fertilization."
        }
      
      if (base_element %in% c("P", "K", "Mg") & diag_code(ratio_element, ratio_value) == 1 ){       # none
        comment_list[[6]]  <- "deficiency. Results from research trials in the B.C. interior indicate there is low risk of inducing"
        comment_list[[7]]  <- base_element
        comment_list[[8]]  <- "deficiency following N fertilization."
      }
      
      if (base_element %in% c("P", "K", "Mg") & diag_code(base_element, base_value) == 3 & diag_code(ratio_element, ratio_value)%in%c(1, 2) ){
        comment_list <- list() 
        comment_list[[1]]  <- "However, foliar"
        comment_list[[2]]  <- ratio_element
        comment_list[[3]]  <-  " of the sample and the results from research trials in the B.C. interior indicate that"
        comment_list[[4]]  <- base_element
        comment_list[[5]]  <- "status will likely not have a serious negative impact on stand performance and/or growth response following N fertilization."
      }
      
      if (ratio_element == "N:S" & diag_code(ratio_element, ratio_value)== 4 ){                    # severe
        comment_list[[6]]  <- ifelse ( fertilization == "No", "deficiency. Foliar N/S ratios of this magnitude have not been documented for unfertilized lodgepole pine in the B.C. interior.  Foliar N and/or S values may be incorrect or N fertilizer may have recently been applied to this stand.", 
                                       "deficiency, likely induced or exacerbated by N fertilization.  Results from research trials in the B.C. interior indicate that growth response following N fertilization will likely be relatively small.")
        
      } else if (ratio_element == "N:S" & diag_code(ratio_element, ratio_value)== 3 ){            # moderate
        comment_list[[6]]  <- ifelse ( fertilization == "No", "deficiency. Foliar N/S ratios of this magnitude are extremely rare in unfertilized lodgepole pine in the B.C. interior.  Foliar N and/or S values may be incorrect or N fertilizer may have recently been applied to this stand.",
                                       "deficiency, likely induced or exacerbated by N fertilization.  Results from research trials in the B.C. interior indicate that growth response following N fertilization may be relatively small.")
        
      } else if (ratio_element == "N:S" & diag_code(ratio_element, ratio_value)== 2 ){           # slight
        comment_list[[6]]  <-  ifelse ( fertilization == "No", "deficiency. Results from research trials in the B.C. interior indicate that growth response following N fertilization will be minimal unless S is added to the fertilizer prescription.",
                                        "deficiency, likely induced by N fertilization")
        
      } else if (ratio_element == "N:S" & diag_code(ratio_element, ratio_value)== 1 & !is.na(SO4_value)){
        comment_list[[6]]  <-  "deficiency. However, research trials in the B.C. interior indicate S deficiency may be induced by N fertilization and that growth response may be enhanced with the addition of S to the fertilizer prescription."
      } else if (ratio_element == "N:S" & diag_code(ratio_element, ratio_value)== 1 & is.na(SO4_value)){  
        comment_list[[6]]  <-  "deficiency. However, foliar SO4 concentration has greater diagnostic value than N/S ratio."
      }
      paste(unlist(comment_list), collapse =" ")  
    }
}


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# This is the function for writing fertilization prescription
#-------------------------------------------------------------------------------------------------------------------------------------------------------

prescription_properties<-list()

prescription_properties[["Boron_not_likely"]] <-
 "In most cases, the addition of B to the fertilizer prescription is likely not needed. However, on sites subject to frequent and prolonged growing season moisture deficits, the inclusion of 2-3 kg B/ha (as granular borate [15% B]) in the fertilizer prescription will eliminate the small risk of N-induced B deficiency."

prescription_properties[["Boron_possible"]]<-
  "Foliar diagnosis indicates B deficiency may be induced by N fertilization, especially on coarse-textured soils subject to growing season moisture deficits. The inclusion of 2-3 kg B/ha (as granular borate [15% B]) in the fertilizer prescription may be warranted."

prescription_properties[["Boron_likely"]]<-
  "Foliar diagnosis indicates B deficiency will likely be induced or exacerbated by N fertilization. It is recommended that 2-3 kg B/ha (as granular borate [15% B]) be included in the fertilizer prescription."

prescription_properties[["Boron_severe"]]<-
  "Foliar diagnosis indicates B is severely deficient. It is strongly recommended that 2-3 kg B/ha (as granular borate [15% B]) be included in the fertilizer prescription."

prescription_properties[["Calcium_Deficiency_severe_or_moderate"]]<-
  "Foliar diagnosis indicates Ca is deficient. Growth response of lodgepole pine following Ca fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including Ca in the fertilizer prescription."

prescription_properties[["Copper_Deficiency_probable"]]<-
  "Foliar diagnosis indicates Cu deficiency is probable. Growth response of lodgepole pine following Cu fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including Cu in the fertilizer prescription."

prescription_properties[["Iron_Deficiency_probable"]]<-
  "Foliar diagnosis indicates Fe deficiency is probable. Growth response of lodgepole pine following Fe fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including Fe in the fertilizer prescription."

prescription_properties[["Macronutrients_and_Ratio_slight"]]<-
  "Available research information indicates that slight to moderate macronutrient (P, K, Ca, Mg) deficiencies do not prevent favourable responses to N fertilization. Therefore, the addition of these nutrients to the fertilizaer prescription is currently not recommended."

prescription_properties[["Magnesium_Deficiency_severe_or_moderate"]]<-
  "Foliar diagnosis indicates Mg is deficient. Growth response of lodgepole pine following Mg fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including Mg in the fertilizer prescription."

prescription_properties[["Magnesium_Ratio_Deficiency_severe_or_moderate"]]<-
  "Foliar diagnosis indicates N/Mg is deficient. Growth response of lodgepole pine following Mg fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including Mg in the fertilizer prescription."

prescription_properties[["Manganese_Deficiency_probable"]]<-
  "Foliar diagnosis indicates Mn deficiency is probable. Growth response of lodgepole pine following Mn fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including Mn in the fertilizer prescription."

prescription_properties[["Micronutrients_possible"]]<-
  "Available research information indicates that possible micronutrient (Cu, Zn, Fe, Mn) deficiencies do not prevent favourable responses to N fertilization. Therefore, the addition of these nutrients to the fertilizer prescription is currently not recommended."

prescription_properties[["Nitrogen_Fertilization_deficient"]]<-
  "Foliar diagnosis indicates N deficiency. Recommended fertilizer prescription to correct N deficiency is 175-225 kg N/ha, using urea (46-0-0) as the primary N source."

prescription_properties[["Nitrogen_Fertilization_none"]]<-
  "Results from research trials in the B.C. interior indicate poor fertilization response potential in stands where pre-fertilization foliar N is > 1.35%. Although other deficiencies may exist, the addition of other nutrients will likely not result in significant growth responses in the absence of N deficiency."

prescription_properties[["N_S_none1"]]<-
  "In the absence of foliar SO4 data, the need to include sulphur in the fertilizer prescription cannot be reliably determined. However, foliar N/S indicates that inclusion of S in the fertilizer prescription is likely not warranted."

prescription_properties[["N_S_none2"]]<-
  "In the absence of foliar SO4 data, the need to include sulphur in the fertilizer prescription cannot be reliably determined. However, foliar N/S indicates that S deficiency may potentially be induced by N fertilization. Results from research trials indicate that growth response may be improved if S is added in combination with N. The inclusion of 50-75 kg S/ha (as ammonium sulphate [21-0-0]) in the fertilizer prescription may be warranted."

prescription_properties[["N_S_none1_other_labs"]]<-
  "In the absence of 'normalized' foliar SO4 data, the need to include sulphur in the fertilizer prescription cannot be reliably determined. However, foliar N/S indicates that S deficiency may potentially be induced by N fertilization. Results from research trials indicate that growth response may be improved if S is added in combination with N. The inclusion of 50-75 kg S/ha (as ammonium sulphate [21-0-0]) in the fertilizer prescription may be warranted."

prescription_properties[["N_S_none2_other_labs"]]<-
  "In the absence of 'normalized' foliar SO4 data, the need to include sulphur in the fertilizer prescription cannot be reliably determined. However, foliar N/S indicates that inclusion of S in the fertilizer prescription is likely not warranted." 

prescription_properties[["N_S_deficient"]]<-
  "Foliar diagnosis indicates S deficiency may be induced or exacerbated by N fertilization. Results from research trials indicate that growth response will likely be improved if S is added in combination with N. The inclusion of 50-75 kg S/ha (as ammonium sulphate [21-0-0]) in the fertilizer prescription may be warranted."

prescription_properties[["Other_Labs_Sulphur"]]<-
  "In the absence of 'normalized' foliar S and SO4 data, the need to include S in the fertilizer prescription cannot be reliably determined."

prescription_properties[["Phosphorus_Deficiency_severe_or_moderate"]]<-
  "Foliar diagnosis indicates P is deficient. Growth response of lodgepole pine following P fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including P in the fertilizer prescription."

prescription_properties[["Phosphorus_Ratio_Deficiency_severe_or_moderate"]]<-
  "Foliar diagnosis indicates N/P is deficient. Growth response of lodgepole pine following P fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including P in the fertilizer prescription."

prescription_properties[["Potassium_Deficiency_severe_or_moderate"]]<-
  "Foliar diagnosis indicates K is deficient. Growth response of lodgepole pine following K fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including K in the fertilizer prescription."

prescription_properties[["Potassium_Ratio_Deficiency_severe_or_moderate"]]<-
  "Foliar diagnosis indicates N/K is deficient. Growth response of lodgepole pine following K fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including K in the fertilizer prescription."

prescription_properties[["Sulphate_severe"]]<-
  "Foliar diagnosis indicates S deficiency will likely be induced or exacerbated by N fertilization. Results from research trials in B.C. indicate growth response will likely be minimal unless S is added in combination with N. It is recommended that 50-75 kg S/ha (as ammonium sulphate [21-0-0]) be included in the fertilizer prescription."

prescription_properties[["Sulphate_moderate"]]<-
  "Foliar diagnosis indicates S deficiency will likely be induced or exacerbated by N fertilization. Results from research trials in the B.C. interior indicate growth response may be significantly improved if S is added in combination with N. It is recommended that 50-75 kg S/ha (as ammonium sulphate [21-0-0]) be included in the fertilizer prescription."

prescription_properties[["Sulphate_slight"]]<-
  "Foliar diagnosis indicates S deficiency may potentially be induced by N fertilization. Results from research trials indicate that growth response may be improved if S is added in combination with N. The inclusion of 50-75 kg S/ha (as ammonium sulphate [21-0-0]) in the fertilizer prescription may be warranted."

prescription_properties[["Sulphate_none"]]<-
  "Foliar diagnosis indicates that inclusion of S in the fertilizer prescription is likely not warranted."

prescription_properties[["Zinc_Deficiency_probable"]]<-
  "Foliar diagnosis indicates Zn deficiency is probable. Growth response of lodgepole pine following Zn fertilization in the B.C. interior is not well documented. It is recommended that a tree nutrition specialist be consulted before including Zn in the fertilizer prescription."






#-------------------------------------------------------------------------------------------------------------------------------------------------------
# This is the function for writing Comments
#-------------------------------------------------------------------------------------------------------------------------------------------------------



site_properties <- list()

site_properties[["Boron_probable"]]<-
  
  "Foliar B concentration indicates a probable B deficiency. Deficiency symptoms may, or may not, be visible. In lodgepole pine, the most common and noticeable visual symptoms of B deficiency are shoot, shoot tip, or bud dieback of the leader and/or lateral branches resulting in multi-leadered, bush crowns. Needles near the affected tips are often short and deformed, either dark green, or discolored. Needle malformations often occur before other external symptoms. It is suggested that the stand be re-visited to confirm whether visual symptoms of B deficiency exist."

site_properties[["Boron_severe"]]<-
  
  "Foliar B concentration indicates a severe B deficiency. Deficiency symptoms are likely visible, or will become visible after N fertilization. In lodgepole pine, the most common and noticeable visual symptoms of B deficiency are shoot, shoot tip, or bud dieback of the leader and/or lateral bracnches resulting in multi-leadered, bush crowns. Needles near the affected tips are often short and deformed, either dark green, or discolored. Needle malformations often occur before other external symptoms. It is suggested that the stand be re-visited to confirm whether visual symptoms of B deficiency exist."

site_properties[["Composite_Number_Comp_Number"]]<-
  
  "Because there may be considerable inter-tree variation in foliar nutrient levels, a composite sample consisting of foliage from less than 10 trees per stand (or stratum) may not provide reliable estimates of mean foliar concentrations for many nutrients."

site_properties[["Diagnosis_is_based_on_ind_sample"]]<-
  
  "Because there may be considerable inter-tree variation in foliar nutrient levels, foliage from 15 to 20 trees per stand (or stratum) is needed to provide reliable estimates of mean foliar concentrations for most nutrients. For routine diagnostic use, a composite sample, prepared by combining foliage collected from 15 to 20 trees per stand (or stratum), is a cost-effective way of assessing stand foliar nutrient status. Refer to <A HREF='http://www.for.gov.bc.ca/hfd/pubs/Docs/En/En52.pdf'>'Foliar sampling guidelines and nutrient interpretative criteria for lodgepole pine'</A> for further information."

site_properties[["Laboratory_Laboratory_Output"]]<-
  
  "Foliar analytical results for some nutrients differ depending on the methodology used for extraction and determination. In some cases, differences may be large enough to affect diagnoses of nutrient sufficiency or deficiency based on available interpretative criteria. Using functions developed from inter-laboratory comparisons, the submitted foliar values for some nutrients have been adjusted to 'normalized' values from which diagnoses and fertilizer prescriptions have been developed. Normalization is necessary for consistent application of diagnostic and prescriptive rules. <I><b>No inferences should be drawn from the normalization requirement regarding the quality or integrity of the submitted foliar data.</b></I> Submitted and 'normalized' foliar nutrient values (and calculated nutrient ratios), are both shown in the above tabular output."

site_properties[["Laboratory_Other_Labs"]]<-
  
  "Foliar analytical results for some nutrients differ depending on the methodology used for extraction and determination. In some cases, differences may be large enough to affect diagnoses of nutrient sufficiency or deficiency based on available interpretative criteria. For some laboratories, inter-laboratory comparisons have been undertaken to develop functions that can be used to adjust foliar levels to 'normalized' values from which interpretations and fertilizer prescriptions are then made. Because functions have not yet been developed for the laboratory to which this sample was submitted, output should be reviewed with caution, especially with respect to S diagnosis and prescription.<I><b> However, no inferences should be drawn regarding the quality or integrity of the submitted foliar data.</b></I> "

site_properties[["Percent_Live_Crown_Crown"]]<-
  
  "The percent live crown of trees may be too small to efficiently utilize added nutrients following fertilization and, as such, growth response potential may be relatively low."

site_properties[["Pertinent_site_stand_information_stem_cankers"]]<-
  
  "The presence of stem cankers or rusts may prevent reliable diagnosis of stand nutrient status if foliage was collected from affected trees. Refer to <A HREF='http://www.for.gov.bc.ca/hfd/pubs/Docs/En/En52.pdf'>'Foliar sampling guidelines and nutrient interpretative criteria for lodgepole pine'</A> for further information."

  
site_properties[["Pertinent_site_stand_information_small_damage"]]<-
  
  "The presence of feeding injuries (i.e., stem de-barking) by red squirrels or other animals may prevent reliable diagnosis of stand nutrient status if foliage was collected from damaged trees. Sharp increases in the abundance and severity of feeding injuries have been documented following fertilization of lodgepole pine in the interior of British Columbia. It is recommended that a forest health specialist be consulted if fertilization of this stand is contemplated. "

site_properties[["Pertinent_site_stand_information_poorly_drained"]]<-
  
  "Foliar nutrient status may be affected by excessively wet or dry soil conditions that influence the transport and availability of soil nutrients and the activity and vigour of tree roots. Because soil moisture may exert the primary limitation on tree growth, fertilization of excessively wet or dry sites is not recommended."

site_properties[["Pertinent_site_stand_information_severe_needle"]]<-
  
  "The presence of needle casts (e.g., <I>Lophodermella concolor</I>) or blights may prevent reliable diagnosis of stand nutrient status. Dead and discoloured foliage may have lower nutrient levels than unaffected foliage. Nutrient levels in green, current-year''s foliage may be affected by the infection and defoliation of older foliage. "

site_properties[["Ratio_N_P_Deficiency_Slight"]]<-
  
  "Phosphorus deficiency may be induced by nitrogen fertilization. "

site_properties[["Ratio_N_K_Deficiency_Slight"]]<-
  
  "Potassium deficiency may be induced by nitrogen fertilization. "

site_properties[["Foliage_Age_Not_Current_Year"]]<-
  
  "Foliage other than current year''s foliage was submitted for analysis and, as such, may prevent reliable diagnosis of stand nutrient status. Foliar nutrient concentrations may be strongly affected by the age of collected foliage. Foliar interpretative criteria are based on analysis of current year''s foliage. Refer to <A HREF='http://www.for.gov.bc.ca/hfd/pubs/Docs/En/En52.pdf'>'Foliar sampling guidelines and nutrient interpretative criteria for lodgepole pine'</A> for further information. "

site_properties[["Sampling_Month_incorrect"]]<-
  
  "Foliage was collected at a time of year not normally recommended for foliage sampling and, as such, may prevent reliable diagnosis of stand nutrient status. Foliar nutrient concentrations may be strongly affected by the time of year in which foliage is collected. Foliar interpretative criteria are based on analysis of foliage collected during the dormant season. Refer to <A HREF='http://www.for.gov.bc.ca/hfd/pubs/Docs/En/En52.pdf'>'Foliar sampling guidelines and nutrient interpretative criteria for lodgepole pine'</A> for further information. "

site_properties[["Site_Information_Poorly_Drained"]]<-
  
  "Poorly drained soils may affect soil nutrient availability and/or nutrient uptake by tree roots. As such foliar nutrient levels may not accurately reflect soil nutrient status."

site_properties[["Visual_Deficiency_Symptoms_top_dieback_none"]]<-
  
  "Top dieback is a common visual symptom of B deficiency in lodgepole pine. Because foliar B levels in this stand suggest B is not deficient, the observed top dieback symptoms are likely not nutritionally related."

site_properties[["Visual_Deficiency_Symptoms_top_dieback_notlikely"]]<-
  
  "A boron deficiency, causing reduced height increment and/or top die-back, may be induced by nitrogen fertilization"

site_properties[["Visual_Deficiency_Symptoms_top_dieback_possible"]]<-
  
  "A boron deficiency, causing reduced height increment and/or top die-back, may be induced by nitrogen fertilization"

site_properties[["Visual_Deficiency_Symptoms_top_dieback_likely"]]<-
  
  "Sub-acute B deficiency, causing reduced height increment, likely exists in the absence of visual deficiency symptoms (i.e., top die-back)."

site_properties[["Visual_Deficiency_Symptoms_top_dieback_severe"]]<-
  
  "Visual symptoms of B deficiency (i.e., top die-back) likely present. "

site_properties[["Visual_Defficiency_Severe_Needle_Cast"]]<-
  
  "Needle casts (e.g., <I>Lophodermella concolor</I>) and blights may affect foliar nutrient levels. Discolored foliage may have lower nutrient levels than unaffected foliage. Also, nutrient levels in green foliage sampled from trees with severe defoliation may not accurately reflect stand nutrient status."

site_properties[["Crown_Position_Middle_Lower"]]<-
  
  "Foliage was collected from a crown position not normally recommended for foliage sampling and, as such, may prevent reliable diagnosis of stand nutrient status. Foliar nutrient concentrations may be strongly affected by crown position. Foliar interpretative criteria are based on foliage collected from between the top one-quarter and bottom one-half of the live crown. Refer to <A HREF='http://www.for.gov.bc.ca/hfd/pubs/Docs/En/En52.pdf'>'Foliar sampling guidelines and nutrient interpretative criteria for lodgepole pine'</A> for further information."







