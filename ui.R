library(shiny)
library(DT)
library(rmarkdown)
#library(shinythemes)

navbarPage(title = "Lodgepole Pine Foliar Nutrient Diagnosis and Fertilizer Advisory System",
#           theme = shinytheme("sandstone"),
           tabPanel(icon("home"),
                    fluidRow(
                      column(8, offset = 0, style='position:absolute; top:10%; left:15%',
                             includeMarkdown("README.md")
                      )
                    )
           ),
           
           tabPanel("Data Input",
                    
                    fluidRow(
                      column(3,
                             wellPanel(
                               h3("Personal Information"),
                               
                               
                                tags$head(
                                 tags$style(type="text/css", 
                                      "label{ display: table-cell; text-align: left; vertical-align: right; margin: 0 0 1em 0;
                                              width: 50%} .form-group { display: table-row;}")
                                ),
                               
                               tags$div(class = "inline",
                               
                               textInput("Org","Organization:", placeholder = "Your orgnaization.."),
                               textInput("FirstName","First Name:", placeholder="Your name.."),
                               textInput("LastName","Last Name:", placeholder="Your last name.."),
                               textInput("Addr_1","Current Address: ", NA),
                               textInput("Addr_2","", NA),
                               textInput("Addr_3","", NA),
                               textInput("Addr_4","", NA)
                                       ))
                      ),
                      
                      column(3, 
                             wellPanel(
                               h3("Stand Information"),
                               textInput("SiteID","Site Location:",NA),
                               textInput("SampID","Sample ID:",NA),
                               textInput("Elev","Elevation (m):",NA),
                               textInput("Age","Stand Age (yrs):",NA),
                               textInput("Map","Mapsheet ID:",NA),
                               textInput("Open","Opening ID:",NA),
                               textInput("SI","Estimated Site Index:",NA),
                               textInput("Crown","Percent Live Crown:",NA),
                               textInput("Lat_deg","Latitude (Deg.):",NA),
                               textInput("Lat_min","Latitude (Min.):",NA),
                               textInput("Long_deg","Longitude (Deg.):",NA),
                               textInput("Long_min","Longitude (Min.)",NA),
                               textInput("BEC_zone","BEC (zone):",NA),
                               textInput("BEC_subz","BEC (subzone):",NA),
                               textInput("BEC_site","BEC (site code):",NA),
                               helpText("Has this stand been fertilized with a nitrogenous fertilizer within the past two years?"),
                               radioButtons("Prev_fert", "",
                                            c("Yes" = "Yes","No" = "No"), select="No")
                              )
                      ),
                      column(3, 
                             wellPanel(
                               h3("Foliar Sampling Information"),
                               dateInput('samp_date',
                                         label = 'Date: yyyy-mm-dd',
                                         value = NA ),
                               radioButtons("Cr_pos", "Crown Position:",
                                            c("Upper" = "Upper","Middle" = "Middle", 
                                              "Lower" = "Lower"), select="Upper"),
                               radioButtons("Origin", "Stand Origin",
                                            c("Plantation" = "Plantation","Natural (fire origin)" = "Natural_F", 
                                              "Natural (harvest origin)" = "Natural_H"), select="Plantation"),
                               radioButtons("Leaf_age", "Foliar Age",
                                            c("Current year" = "Cur_yr","1-year-old" = "1_yr", 
                                              "Greater than 1-year-old" = "1-yr-more", "Mixed ages" = "Mixed ages"), select="Cur_yr"),
                               radioButtons("Diag_base", "Diagnosis is based on:",
                                            c("Individual Sample" = "Indv_sam","Composite Sample" = "Comp_sam"), select="Indv_sam"),
                               numericInput("Comp_Number", "Number of trees per composite:", NA),
                               checkboxGroupInput("Pertinent", "Pertinent site/stand information:",
                                            c("Very wet or dry soil conditions", "Needle cast of blight", "Stem cankers or rusts", 
                                              "Animal damage to crop trees"))
                             )
                      ),
                      column(3, 
                             wellPanel(
                               h3("Foliar Sampling Lab Results"),
                               selectInput(inputId="Lab_type",label="Select Laboratory",
                                           choices = c("Pacific Soil Analysis Inc." = "Pacific Soil Analysis Inc.", 
                                                       "Ministry of Environment" = "Ministry of Environment",
                                                       "Others" = "Others"),
                                           selected = "Pacific Soil Analysis Inc."),
                               numericInput("N_cont", "N (%)", NA, min = 0),
                               numericInput("P_cont", "P (%)", NA, min = 0),
                               numericInput("K_cont", "K (%)", NA, min = 0),
                               numericInput("Ca_cont", "Ca (%)", NA, min = 0),
                               numericInput("Mg_cont", "Mg (%)", NA, min = 0),
                               numericInput("S_cont", "S (%)", NA, min = 0),
                               numericInput("SO4_cont", "SO4 (ppm)", NA, min = 0),
                               numericInput("B_cont", "B (ppm)", NA, min = 0),
                               numericInput("Cu_cont", "Cu (ppm)", NA, min = 0),
                               numericInput("Zn_cont", "Zn (ppm)", NA, min = 0),
                               numericInput("Fe_cont", "Fe (ppm)", NA, min = 0),
                               numericInput("Mn_cont", "Mn (ppm)", NA, min = 0)
                             )
                      )
                    )),
           tabPanel("Diagnosis/Prescription",
                    fluidRow(
                      column(2,
                        wellPanel(
                          h4(icon("file-alt"), "Export Report"),
                          hr(),
                          radioButtons("Repform", "Document format:", c("PDF", "HTML", "Word"), inline=TRUE),
                          downloadButton("downloadReport","Create Report")
                        )
                      ),
                      column(6,
                         h3(icon("tree"), "LODGEPOLE PINE FOLIAR NUTRIENT DIAGNOSIS") ,
                         hr(),
                         tags$li("Query date/time: ", Sys.time()),
                         hr(),
                         h4(span("Caution!", style = "color:red"), "Please note that this report was made through the proto-version of the", tags$em("LODGEPOLE PINE FOLIAR NUTRIENT DIAGNOSIS,"), "so the diagnosis and fertilizer prescription might contain inaccurate information."),
                         hr(),
                         h4(icon("pagelines"), "Foliar Nutrient Data"),
                         DTOutput("OutTb"),

                         hr(),
                         h4(icon("sticky-note"), "Comments"),
                         htmlOutput("Comments"),
                         
                         hr(),
                         h4(icon("stethoscope"), "DIAGNOSIS"),
                         h5(tags$b('see "Comments" above for factors that may affect the foliar nutrient diagnosis')),
                         h5(tags$li(tags$em(tags$b("Nitrogen (N):")))),
                         textOutput("diag_N"),
                         
                         h5(tags$li(tags$em(tags$b("Phosphorus (P):")))),
                         textOutput("diag_P"),
                         textOutput("diag_N_P"),
                         
                         h5(tags$li(tags$em(tags$b("Potassium (K):")))),
                         textOutput("diag_K"),
                         textOutput("diag_N_K"),
                         
                         h5(tags$li(tags$em(tags$b("Calcium (Ca):")))),
                         textOutput("diag_Ca"),
                         
                         h5(tags$li(tags$em(tags$b("Magnesium (Mg):")))),
                         textOutput("diag_Mg"),
                         textOutput("diag_N_Mg"),
                         
                         h5(tags$li(tags$em(tags$b("Sulphur (S):")))),
                         textOutput("diag_S"),
                         textOutput("diag_N_S"),
                         textOutput("diag_SO4"),
                         
                         h5(tags$li(tags$em(tags$b("Copper (Cu):")))),
                         textOutput("diag_Cu"),
                         
                         h5(tags$li(tags$em(tags$b("Zinc (Zn):")))),
                         textOutput("diag_Zn"),
                         
                         h5(tags$li(tags$em(tags$b("Iron (Fe):")))),
                         textOutput("diag_Fe"),
                         
                         h5(tags$li(tags$em(tags$b("Manganese (Mn):")))),
                         textOutput("diag_Mn"),
                         
                         h5(tags$li(tags$em(tags$b("Boron (B):")))),
                         textOutput("diag_B"),
                         
                         hr(),
                         h4(icon("file-prescription"), "FERTILIZER PRESCRIPTION"),
                         htmlOutput("prescription"),
                         hr()
                    )
           )


))
                      