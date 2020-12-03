library(shiny)
library(DT)
library(knitr)
library(rmarkdown)





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
  updateNavbarPage(session, "PL_FERT", selected = "Example1")
     })
observeEvent(input$example2, {
  updateNavbarPage(session, "PL_FERT", selected = "Example2")
})
observeEvent(input$Vis_sym, {
  updateNavbarPage(session, "PL_FERT", selected = "Visual Symptoms")
})


output.table <- function (){
  Element = c("N", "P", "K", "Ca", "Mg", "S","SO4", "B", "Cu", "Zn", "Fe", "Mn", "N:S", "N:P", "N:K", "N:Mg")
  Raw.value = as.numeric(round(c(input$N_cont, input$P_cont, input$K_cont, input$Ca_cont, input$Mg_cont, input$S_cont, input$SO4_cont, 
                input$B_cont, input$Cu_cont, input$Zn_cont, input$Fe_cont, input$Mn_cont,
                input$N_cont/input$S_cont, input$N_cont/input$P_cont, input$N_cont/input$K_cont, input$N_cont/input$Mg_cont), 3))
  Norm.values <- as.numeric(round(c(N_norm(), P_norm(), K_norm(), Ca_norm(), Mg_norm(), S_norm(), SO4_norm(), 
                                B_norm(), Cu_norm(), Zn_norm(), Fe_norm(), Mn_norm(),  
                               N_norm()/S_norm(), N_norm()/P_norm(), N_norm()/K_norm(), N_norm()/Mg_norm()), 3))
  
  Interpretation <- mapply (Interpret, Element, Norm.values)
  Out_Table <- data.frame (cbind(Element, Raw.value, Norm.values, Interpretation))
  Out_Table
}

output$OutTb <- renderDT({

  pct.header = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        lapply(c("Elements", "Measured Values", "Normalized Values", "Interpretation"), th)
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

output$diag_N <- renderText(concat_comments_nutrients("N", N_norm(), input$Prev_fert, input$Lab_type))
output$diag_P <- renderText(concat_comments_nutrients("P", P_norm(), input$Prev_fert, input$Lab_type))
output$diag_K <- renderText(concat_comments_nutrients("K", K_norm(), input$Prev_fert, input$Lab_type))
output$diag_Ca <- renderText(concat_comments_nutrients("Ca", Ca_norm(), input$Prev_fert, input$Lab_type))
output$diag_Mg <- renderText(concat_comments_nutrients("Mg", Mg_norm(), input$Prev_fert, input$Lab_type))
output$diag_S <- renderText(concat_comments_nutrients("S", S_norm(), input$Prev_fert, input$Lab_type))
output$diag_Cu <- renderText(concat_comments_nutrients("Cu", Cu_norm(), input$Prev_fert, input$Lab_type))
output$diag_Zn <- renderText(concat_comments_nutrients("Zn", Zn_norm(), input$Prev_fert, input$Lab_type))
output$diag_Fe <- renderText(concat_comments_nutrients("Fe", Fe_norm(), input$Prev_fert, input$Lab_type))
output$diag_Mn <- renderText(concat_comments_nutrients("Mn", Mn_norm(), input$Prev_fert, input$Lab_type))
output$diag_B <- renderText(concat_comments_nutrients("B", B_norm(), input$Prev_fert, input$Lab_type))

output$diag_SO4 <- renderText(concat_comments_nutrients("SO4", SO4_norm(), input$Prev_fert, input$Lab_type))

output$diag_N_P <- renderText(concat_comments_nutrients_ratio("N:P", N_norm()/P_norm(), P_norm(), SO4_norm(), input$Prev_fert))
output$diag_N_K <- renderText(concat_comments_nutrients_ratio("N:K", N_norm()/K_norm(), K_norm(), SO4_norm(), input$Prev_fert))
output$diag_N_Mg <- renderText(concat_comments_nutrients_ratio("N:Mg", N_norm()/Mg_norm(), Mg_norm(), SO4_norm(), input$Prev_fert))
output$diag_N_S <- renderText(concat_comments_nutrients_ratio("N:S", N_norm()/S_norm(), S_norm(), SO4_norm(), input$Prev_fert))

output$prescription <- renderUI(write_super_prescription (Prev_fert = input$Prev_fert, N_value = N_norm(), 
                                                            Lab_type = input$Lab_type, SO4_value = SO4_norm(),
                                                            N_S_value = N_norm()/S_norm(), Cu_value = Cu_norm(),
                                                            Zn_value = Zn_norm(), Fe_value = Fe_norm(), 
                                                            Mn_value = Mn_norm(), P_value = P_norm(), 
                                                            N_P_value = N_norm()/P_norm(), K_value = K_norm(),
                                                            N_K_value = N_norm()/K_norm(), Ca_value = Ca_norm(), 
                                                            Mg_value = Mg_norm(), N_Mg_value = N_norm()/Mg_norm(),
                                                            B_value = B_norm())
                                  )

output$Comments <- renderUI(write_super_comment(Lab_type = input$Lab_type, Crown = input$Crown,
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
  
