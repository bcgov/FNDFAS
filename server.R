library(shiny)
library(DT)
library(knitr)
library(rmarkdown)
library(leaflet)
library(ggmap)
library(dplyr)

function(input, output, session) {

N_norm <- reactive(ifelse (input$Lab_type=="Pacific Soil Analysis Inc.", input$N_cont, 0.9584*input$N_cont)  )
P_norm <- reactive(ifelse (input$Lab_type=="Pacific Soil Analysis Inc.", 0.9492*input$P_cont, input$P_cont)  )
K_norm <- reactive(ifelse (input$Lab_type=="Pacific Soil Analysis Inc.", 0.1714*(input$K_cont*input$K_cont)+(0.8405*input$K_cont), input$K_cont))
Ca_norm <- reactive(ifelse (input$Lab_type=="Pacific Soil Analysis Inc.", (0.3592*(input$Ca_cont*input$Ca_cont)+0.7346*input$Ca_cont), input$Ca_cont))
Mg_norm <- reactive(ifelse (input$Lab_type=="Pacific Soil Analysis Inc.",(1.0249*input$Mg_cont), input$Mg_cont))
S_norm <- reactive(ifelse (input$Lab_type=="Pacific Soil Analysis Inc.", input$S_cont, (0.9558*input$S_cont)-(0.6267*(input$S_cont*input$S_cont))))
SO4_norm <- reactive(ifelse (input$Lab_type=="Pacific Soil Analysis Inc.", input$SO4_cont, (1.4164*input$SO4_cont)-(0.0008*(input$SO4_cont*input$SO4_cont))))
B_norm <- reactive(ifelse (input$Lab_type=="Pacific Soil Analysis Inc.", input$B_cont, (0.8732*input$B_cont)-(0.0012*(input$B_cont*input$B_cont))))
Cu_norm <- reactive(input$Cu_cont)
Zn_norm <- reactive(input$Zn_cont)
Fe_norm <- reactive(ifelse (input$Lab_type=="Pacific Soil Analysis Inc.", (1.1477*input$Fe_cont), input$Fe_cont))
Mn_norm <- reactive(input$Mn_cont)
SampID <- reactive(input$SampID)
BECID <- reactive(input$BECID)
SPP <- reactive(input$SPP)
Age <- reactive(input$Age)

observeEvent(input$example1, {
  updateNavbarPage(session, "FERT", selected = "Example1")
     })
observeEvent(input$example2, {
  updateNavbarPage(session, "FERT", selected = "Example2")
})
observeEvent(input$Vis_sym, {
  updateNavbarPage(session, "FERT", selected = "Visual Symptoms")
})




write_site_info <- function (){
  comment_list <- list()
  comment_list[[1]] <- paste("Sample ID:", input$SampID)
  comment_list[[2]] <- paste("Species:", input$spp)
  comment_list[[3]] <- paste("Sampling Date:", input$samp_date)
  comment_list[[4]] <- paste("Analytical Laboratory:", input$Lab_type)
  comment_list[[5]] <- paste("Site ID:", input$SiteID)
  comment_list[[6]] <- paste("BEC Zone: ", input$BEC_zone, input$BEC_subz, " ", input$BEC_site, sep = "")
  comment_list[[7]] <- paste("Elevation:", input$Elev)
  comment_list[[8]] <- paste("Mapsheet:", input$Mapsheet)
  comment_list[[9]] <- paste("Opening ID:", input$OpenID)
  comment_list[[10]] <- paste("Stand Age (yr):", input$Age)
  comment_list[[11]] <- paste("Stand Origin: ", switch(input$Origin, "Plantation" = "Plantation", "Natural_F"="Natural (fire origin)", "Natural_H"= "Natural (harvest origin)"))
  comment_list[[12]] <- paste("Site Index (m):", input$SI)
  comment_list[[13]] <- paste("Percent Live Crown:", input$Crown)
  comment_list[[14]] <- paste("Recently (past two years) fertilized (Yes/No):", input$Prev_fert)
                             
  HTML(paste("<li>", unlist(comment_list), collapse ="<br/>"))
  
}

output$Site_Info <- renderUI(write_site_info())


# Add interactive location map

lng <- reactive(as.numeric(input$Long_deg) + as.numeric(input$Long_min)/60)
lat <- reactive(as.numeric(input$Lat_deg) + as.numeric(input$Lat_min)/60)

output$Loc_map <- renderLeaflet({
  leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    setView(lng = ifelse(is.na(lng()), -123.3666667, lng()), 
            lat = ifelse(is.na(lat()), 48.4166667, lat()), 
            zoom = ifelse(is.na(lat()), 12, 8)) %>%
    addMarkers(lng=lng(), 
               lat=lat(), 
               popup="The sampling location")
})


output.table <- function (){
  Element = c("N", "P", "K", "Ca", "Mg", "S", "SO4", "B", "Cu", "Zn", "Fe", "Mn", "N:S", "N:P", "N:K", "N:Mg")
  Raw.value = as.numeric(round(c(input$N_cont, input$P_cont, input$K_cont, input$Ca_cont, input$Mg_cont, input$S_cont, input$SO4_cont, 
                input$B_cont, input$Cu_cont, input$Zn_cont, input$Fe_cont, input$Mn_cont,
                input$N_cont/input$S_cont, input$N_cont/input$P_cont, input$N_cont/input$K_cont, input$N_cont/input$Mg_cont), 3))
  Norm.values <- as.numeric(round(c(N_norm(), P_norm(), K_norm(), Ca_norm(), Mg_norm(), S_norm(), SO4_norm(), 
                                B_norm(), Cu_norm(), Zn_norm(), Fe_norm(), Mn_norm(),  
                               N_norm()/S_norm(), N_norm()/P_norm(), N_norm()/K_norm(), N_norm()/Mg_norm()), 3))
  Dev.suff <- rep(NA, 16)
  Dev.suff[1:12] <- round(Norm.values - apply(mapply(T_func, input$spp, Element), 2, max, na.rm=TRUE), 3)[1:12]
  Dev.suff[13:16] <- round(Norm.values - apply(mapply(T_func, input$spp, Element), 2, min, na.rm=TRUE), 3)[13:16]
  Interpretation <- mapply (Interpret, input$spp, Element, Norm.values)
  Desirability <- c(rep("High", 12), rep("Low", 4))
  Out_Table <- data.frame (cbind(Element, Raw.value, Norm.values, Dev.suff, Interpretation, Desirability))
  Out_Table
}

output$OutTb <- renderDT({

  pct.header = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        lapply(c("Elements", "Measured Values", "Normalized Values", "Deviation from Adequate Level", "Interpretation", "Desirable Condition"), th)
      )  
    )
  )
  )
  datatable(output.table(),
            container =  pct.header , rownames = FALSE, 
            options = list(paging = FALSE, ordering=F, sDom  = '<"top">rt<"bottom">',
                           columnDefs=list(list(targets= '_all', class="dt-right"))               
            ))          
})

output$diag_N <- renderText(concat_comments_nutrients(input$spp, "N", N_norm(), input$Prev_fert, input$Lab_type))
output$diag_P <- renderText(concat_comments_nutrients(input$spp, "P", P_norm(), input$Prev_fert, input$Lab_type))
output$diag_K <- renderText(concat_comments_nutrients(input$spp, "K", K_norm(), input$Prev_fert, input$Lab_type))
output$diag_Ca <- renderText(concat_comments_nutrients(input$spp, "Ca", Ca_norm(), input$Prev_fert, input$Lab_type))
output$diag_Mg <- renderText(concat_comments_nutrients(input$spp, "Mg", Mg_norm(), input$Prev_fert, input$Lab_type))
output$diag_S <- renderText(concat_comments_nutrients(input$spp, "S", S_norm(), input$Prev_fert, input$Lab_type))
output$diag_Cu <- renderText(concat_comments_nutrients(input$spp,"Cu", Cu_norm(), input$Prev_fert, input$Lab_type))
output$diag_Zn <- renderText(concat_comments_nutrients(input$spp,"Zn", Zn_norm(), input$Prev_fert, input$Lab_type))
output$diag_Fe <- renderText(concat_comments_nutrients(input$spp,"Fe", Fe_norm(), input$Prev_fert, input$Lab_type))
output$diag_Mn <- renderText(concat_comments_nutrients(input$spp,"Mn", Mn_norm(), input$Prev_fert, input$Lab_type))
output$diag_B <- renderText(concat_comments_nutrients(input$spp,"B", B_norm(), input$Prev_fert, input$Lab_type))

output$diag_SO4 <- renderText(concat_comments_nutrients(input$spp, "SO4", SO4_norm(), input$Prev_fert, input$Lab_type))

output$diag_N_P <- renderText(concat_comments_nutrients_ratio(input$spp, "N:P", N_norm()/P_norm(), P_norm(), SO4_norm(), input$Prev_fert))
output$diag_N_K <- renderText(concat_comments_nutrients_ratio(input$spp, "N:K", N_norm()/K_norm(), K_norm(), SO4_norm(), input$Prev_fert))
output$diag_N_Mg <- renderText(concat_comments_nutrients_ratio(input$spp, "N:Mg", N_norm()/Mg_norm(), Mg_norm(), SO4_norm(), input$Prev_fert))
output$diag_N_S <- renderText(concat_comments_nutrients_ratio(input$spp, "N:S", N_norm()/S_norm(), S_norm(), SO4_norm(), input$Prev_fert))

output$prescription <- renderUI(write_super_prescription (spp = input$spp, Prev_fert = input$Prev_fert, N_value = N_norm(), 
                                                            Lab_type = input$Lab_type, SO4_value = SO4_norm(),
                                                            N_S_value = N_norm()/S_norm(), Cu_value = Cu_norm(),
                                                            Zn_value = Zn_norm(), Fe_value = Fe_norm(), 
                                                            Mn_value = Mn_norm(), P_value = P_norm(), 
                                                            N_P_value = N_norm()/P_norm(), K_value = K_norm(),
                                                            N_K_value = N_norm()/K_norm(), Ca_value = Ca_norm(), 
                                                            Mg_value = Mg_norm(), N_Mg_value = N_norm()/Mg_norm(),
                                                            B_value = B_norm())
                                  )

output$Comments <- renderUI(write_super_comment(spp = input$spp, Lab_type = input$Lab_type, Crown = input$Crown,
                                                    B_value = B_norm(), N_K_value = N_norm()/K_norm(),
                                                    Cr_pos = input$Cr_pos, samp_date = input$samp_date, 
                                                    Diag_base = input$Diag_base, 
                                                    Comp_Number = as.numeric(input$Comp_Number),
                                                    Leaf_age = input$Leaf_age, Pertinent = input$Pertinent))



output$downloadReport <- downloadHandler(
  filename = function() {
    paste('Foliar_Nutrient_Report', sep = '.', switch(
      input$Repform, PDF = 'pdf', HTML = 'html', Word = 'docx')
    )
  },
  
  content = function(file) {
    src <- normalizePath('report.Rmd')
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'report.Rmd', overwrite = TRUE)
    
    out <- render('report.Rmd', switch(
      input$Repform,
      PDF = pdf_document(), HTML = html_document(), Word = word_document()
    ))
    file.rename(out, file)
  }
)



}
  
