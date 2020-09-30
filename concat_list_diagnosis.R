#-------------------------------------------------------------------------------------------------------------------------------------------------------
# This is the function for nutrient diagnosis.
# This will also work as the base comments for nutrient ratio (e.g., "N:P") disgnosis.
#-------------------------------------------------------------------------------------------------------------------------------------------------------

concat_comments_nutrients <- function(element, value){
  if (is.na(value)) paste("The analysis result for", element, "was not entered.", collapse =" ") else {
  if (element == "N") {
      comment1 <- switch (diag_code(element, value), 
                          'that will likely negatively affect stand performance.',
                          'that will likely negatively affect stand performance.',
                          'that may negatively affect stand performance.',
                          'level of nutrient condition.')
      paste("Foliar", element, "concentration indicates", Intp_comments(element, value), element, "deficiency", comment1, collapse =" ")
  } else  if (element %in% c("P", "K", "Ca", "Mg")& diag_code(element, value)==1) {
      comment_list <- list()  
      comment_list[[1]]  <- "Foliar"
      comment_list[[2]]  <- element
      comment_list[[3]]  <- "concentration indicates"
      comment_list[[4]]  <- Intp_comments(element, value)
      comment_list[[5]]  <- element
      comment_list[[6]]  <- "deficiency that will likely negatively affect stand performance and/or growth response following N fertilization.  Severe"
      comment_list[[7]]  <- element
      comment_list[[8]]  <- "deficiencies are uncommon in the B.C. interior, and growth responses to"
      comment_list[[9]]  <- element
      comment_list[[10]]  <- "fertilization are not well documented. Therefore, it is suggested that a tree nutrition specialist be consulted. Deficiency symptoms may, or may not, be visible."
      paste(unlist(comment_list), collapse =" ")
  } else  if (element %in% c("P", "K", "Ca", "Mg")& diag_code(element, value)==2) {
      comment_list <- list()  
      comment_list[[1]]  <- "Foliar"
      comment_list[[2]]  <- element
      comment_list[[3]]  <- "concentration indicates"
      comment_list[[4]]  <- Intp_comments(element, value)
      comment_list[[5]]  <- element
      comment_list[[6]]  <- "deficiency that may negatively affect stand performance and/or growth response following N fertilization. Because growth responses to"
      comment_list[[7]]  <- element
      comment_list[[8]]  <- "fertilization are not well documented. Therefore, it is suggested that a tree nutrition specialist be consulted.Deficiency symptoms may, or may not, be visible."
      paste(unlist(comment_list), collapse =" ")
  } else  if (element %in% c("P", "K", "Ca", "Mg")& diag_code(element, value)==3) {
    comment_list <- list()  
    comment_list[[1]]  <- "Foliar"
    comment_list[[2]]  <- element
    comment_list[[3]]  <- "concentration indicates"
    comment_list[[4]]  <- Intp_comments(element, value)
    comment_list[[5]]  <- element
    comment_list[[6]]  <- "deficiency. However, results from research trials in the B.C. interior indicate that"
    comment_list[[7]]  <- element
    comment_list[[8]]  <- "status will likely not have a serious negative impact on stand performance and/or growth response following N fertilization."
    paste(unlist(comment_list), collapse =" ")
  } else  if (element %in% c("P", "K", "Ca", "Mg")& diag_code(element, value)==4) {
    comment_list <- list()  
    comment_list[[1]]  <- "Foliar"
    comment_list[[2]]  <- element
    comment_list[[3]]  <- "concentration indicates"
    comment_list[[4]]  <- Intp_comments(element, value)
    comment_list[[5]]  <- element
    comment_list[[6]]  <- "deficiency."
    paste(unlist(comment_list), collapse =" ")
  } else if (element == "S") {
    comment1 <- switch (diag_code(element, value), 
                        'However, foliar SO4 and N/S ratio have greater diagnostic value than total S.',
                        'However, foliar SO4 and N/S ratio have greater diagnostic value than total S.',
                        'However, foliar SO4 and N/S ratio have greater diagnostic value than total S.',
                        '')
    paste("Foliar", element, "concentration indicates", Intp_comments(element, value), element, "deficiency.", comment1, collapse =" ")
  } else if (element == "SO4") {
    comment1 <- switch (diag_code(element, value), 
                        'Results from research trials in the B.C. interior indicate that growth response following N fertilization will be minimal unless S is added to the fertilizer prescription.',
                        'Results from research trials in the B.C. interior indicate that growth response following N fertilization will likely be small unless S is added to the fertilizer prescription.',
                        'Results from research trials in the B.C. interior indicate that growth response following N fertilization may be improved if S is added to the fertilizer prescription.',
                        '')
    paste("Foliar", element, "concentration indicates", Intp_comments(element, value), element, "deficiency.", comment1, collapse =" ")    
    
  } else  if (element %in% c("Cu", "Zn", "Fe", "Mn")& diag_code(element, value)==1) {
    comment_list <- list()  
    comment_list[[1]]  <- "Foliar"
    comment_list[[2]]  <- element
    comment_list[[3]]  <- "concentration indicates"
    comment_list[[4]]  <- Intp_comments(element, value)
    comment_list[[5]]  <- element
    comment_list[[6]]  <- "deficiency that may negatively affect stand performance and/or growth response following N fertilization. Probable"
    comment_list[[7]]  <- element
    comment_list[[8]]  <- "deficiencies are uncommon in the B.C. interior, and growth responses to"
    comment_list[[9]]  <- "fertilization are not well documented. Therefore, it is suggested that a tree nutrition specialist be consulted. Deficiency symptoms may, or may not, be visible."
    paste(unlist(comment_list), collapse =" ")
  } else  if (element %in% c("Cu", "Zn", "Fe", "Mn")& diag_code(element, value)==2) {
    comment_list <- list()  
    comment_list[[1]]  <- "Foliar"
    comment_list[[2]]  <- element
    comment_list[[3]]  <- "concentration indicates"
    comment_list[[4]]  <- Intp_comments(element, value)
    comment_list[[5]]  <- element
    comment_list[[6]]  <- "deficiency. However, results from research trials in the B.C. interior indicate that"
    comment_list[[7]]  <- element
    comment_list[[8]]  <- "status will likely not have a serious negative impact on stand performance and/or growth response following N fertilization."
    paste(unlist(comment_list), collapse =" ")
  }  else  if (element %in% c("Cu", "Zn", "Fe", "Mn") & diag_code(element, value)==3) {
    comment_list <- list()  
    comment_list[[1]]  <- "Foliar"
    comment_list[[2]]  <- element
    comment_list[[3]]  <- "concentration indicates"
    comment_list[[4]]  <- Intp_comments(element, value)
    comment_list[[5]]  <- element
    comment_list[[6]]  <- "deficiency." 
    paste(unlist(comment_list), collapse =" ")
  } else  if (element == "B" & diag_code(element, value)==1) {
    comment_list <- list()  
    comment_list[[1]]  <- "Foliar"
    comment_list[[2]]  <- element
    comment_list[[3]]  <- "concentration indicates"
    comment_list[[4]]  <- Intp_comments(element, value)
    comment_list[[5]]  <- element
    comment_list[[6]]  <- "deficiency, likely induced or exacerbated by N fertilization. Results from research trials in the B.C. interior indicate that risk of reduced height increment and/or top dieback is high. Deficiency symptoms are likely visible."
    paste(unlist(comment_list), collapse =" ")
  } else  if (element == "B" & diag_code(element, value)==2) {
    comment_list <- list()  
    comment_list[[1]]  <- "Foliar"
    comment_list[[2]]  <- element
    comment_list[[3]]  <- "concentration indicates"
    comment_list[[4]]  <- Intp_comments(element, value)
    comment_list[[5]]  <- element
    comment_list[[6]]  <- "deficiency, likely induced or exacerbated by N fertilization.  Results from research trials in the B.C. interior indicate a moderate risk of  reduced height increment and/or top dieback. Deficiency symptoms may, or may not, be visible."
    paste(unlist(comment_list), collapse =" ")
  } else  if (element == "B" & diag_code(element, value)==3) {
    comment_list <- list()  
    comment_list[[1]]  <- "Foliar"
    comment_list[[2]]  <- element
    comment_list[[3]]  <- "concentration indicates"
    comment_list[[4]]  <- Intp_comments(element, value)
    comment_list[[5]]  <- element
    comment_list[[6]]  <- "deficiency. Although B status will likely not have a serious negative impact on stand performance or growth response following N fertilization, there may be a small negative impact on height increment."
    paste(unlist(comment_list), collapse =" ")
  } else  if (element == "B" & diag_code(element, value)%in%c(4,5)) {
    comment_list <- list()  
    comment_list[[1]]  <- "Foliar"
    comment_list[[2]]  <- element
    comment_list[[3]]  <- "concentration indicates"
    comment_list[[4]]  <- Intp_comments(element, value)
    comment_list[[5]]  <- element
    comment_list[[6]]  <- "deficiency."
    paste(unlist(comment_list), collapse =" ")
  }
}
}


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# This is the function for nutrient ratio (e.g., "N:P") diagnosis.
# First, return the diagnosis of given nutrient and append additional statements.
#-------------------------------------------------------------------------------------------------------------------------------------------------------


concat_comments_nutrients_ratio <- function (ratio_element, ratio_value, base_value){
  
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

    if (base_element %in% c("P", "K", "Mg") & diag_code(ratio_element, ratio_value) == 1 ){
      comment_list[[6]]  <- "deficiency that will likely negatively affect stand performance and/or growth response following N fertilization."
    }
    
    if (base_element %in% c("P", "K", "Mg") & diag_code(ratio_element, ratio_value) == 2 ){
      comment_list[[6]]  <- "deficiency that may negatively affect stand performance and/or growth response following N fertilization."
    }
    
    if (base_element %in% c("P", "K", "Mg") & diag_code(ratio_element, ratio_value) == 4 ){
      comment_list[[6]]  <- "deficiency. Results from research trials in the B.C. interior indicate there is low risk of inducing"
      comment_list[[7]]  <- base_element
      comment_list[[8]]  <- "deficiency following N fertilization."
    }
    
    if (base_element %in% c("P", "K", "Mg") & diag_code(base_element, base_value) == 3 & diag_code(ratio_element, ratio_value)%in%c(3, 4) ){
      comment_list[[6]]  <- "deficiency. However, results from research trials in the B.C. interior indicate that"
      comment_list[[7]]  <- base_element
      comment_list[[8]]  <- "status will likely not have a serious negative impact on stand performance and/or growth response following N fertilization."
    }
  
    if (ratio_element == c("N:S") ){
      comment_list[[6]]  <- switch(diag_code(ratio_element, ratio_value),
                                   "deficiency, likely induced or exacerbated by N fertilization. Results from research trials in the B.C. interior indicate that growth response following N fertilization will likely be relatively small.",
                                   "deficiency, likely induced or exacerbated by N fertilization. Results from research trials in the B.C. interior indicate that growth response following N fertilization may be relatively small.",
                                   "deficiency, likely induced by N fertilization.",
                                   "deficiency, likely induced by N fertilization.")
    }
    paste(unlist(comment_list), collapse =" ")  
}
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------
# This is the function specific for sulphate diagnosis.
#-------------------------------------------------------------------------------------------------------------------------------------------------------


concat_comments_sulphate <- function (value, fertilization){
  if (is.na(value)) paste("The analysis result for SO4 was not entered.", collapse =" ") else {
  
  comment_list <- list()  
  comment_list[[1]]  <- "Foliar"
  comment_list[[2]]  <- "SO4"
  comment_list[[3]]  <- "concentration indicates"
  comment_list[[4]]  <- Intp_comments("SO4", value)
  comment_list[[5]]  <- "S"  
  
  if (fertilization == "Yes"){
    comment_list[[6]]  <- paste("deficiency.", "\n", "Foliar SO4 concentration is not a useful measure of S status in fertilized foliage.  However, in the absence of 'normalized' foliar S and/or SO4 data, interpretative criteria used for S diagnosis may not apply to analytical methods used by the laboratory to which this foliage sample was submitted.")  
    
  } else {
    comment_list[[6]]  <- switch(diag_code("SO4", value),
                                   "deficiency. Results from research trials in the B.C. interior indicate that growth response following N fertilization will be minimal unless S is added to the fertilizer prescription. In the absence of 'normalized' foliar S and/or SO4 data, interpretative criteria used for S diagnosis may not apply to analytical methods used by the laboratory to which this foliage sample was submitted.",
                                   "deficiency. Results from research trials in the B.C. interior indicate that growth response following N fertilization will likely be small unless S is added to the fertilizer prescription. In the absence of 'normalized' foliar S and/or SO4 data, interpretative criteria used for S diagnosis may not apply to analytical methods used by the laboratory to which this foliage sample was submitted.",
                                   "deficiency. Results from research trials in the B.C. interior indicate that growth response following N fertilization may be improved if S is added to the fertilizer prescription. In the absence of 'normalized' foliar S and/or SO4 data, interpretative criteria used for S diagnosis may not apply to analytical methods used by the laboratory to which this foliage sample was submitted.",
                                   "deficiency. Results from research trials in the B.C. interior indicate there is low risk of inducing S deficiency following N fertilization. In the absence of 'normalized' foliar S and/or SO4 data, interpretative criteria used for S diagnosis may not apply to analytical methods used by the laboratory to which this foliage sample was submitted.")
          }
   paste(unlist(comment_list), collapse =" ")  
    
}
}
  
  
  






