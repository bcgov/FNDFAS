library(shiny)
library(DT)
library(rmarkdown)
library(leaflet)


navbarPage(title = "Foliar Nutrient Diagnosis System", id = "FERT",
           theme = "bcgov.css",

           tabPanel(icon("home"),
                    fluidRow(

                      column(6, offset = 0, style='position:relative; left:10%',
                             includeMarkdown("README.md")
                      ),
                      column(5, style='position:relative; right:-10%',
                             sidebarPanel(
                               h4("Example Reports"),
                               actionButton("example1", "Example 1"),
                               actionButton("example2", "Example 2"),
                               
                               h4("Deficiency Symptoms"),
                               actionButton("Vis_sym", "Visual Symptoms"),
                             )
                      ))),

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
                               helpText("Note: The information will be used only for generating the header of the report.",
                                        "We will not collect any of your personal information."),          
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
                               textInput("BEC_zone","BEC (zone):",NA),
                               textInput("BEC_subz","BEC (subzone):",NA),
                               textInput("BEC_site","BEC (site series):",NA),
                               textInput("Lat_deg","Latitude (Deg.):",NA),
                               textInput("Lat_min","Latitude (Min.):",NA),
                               textInput("Long_deg","Longitude (Deg.):",NA),
                               textInput("Long_min","Longitude (Min.)",NA),
                               helpText("Has this stand been fertilized with a nitrogenous fertilizer within the past two years?"),
                               radioButtons("Prev_fert", "",
                                            c("Yes" = "Yes","No" = "No"), select="No")
                              )
                      ),
                      column(3, 
                             wellPanel(
                               h3("Foliar Sampling Information"),
                               dateInput('samp_date',
                                         label = 'Sampling Date: yyyy-mm-dd',
                                         value = date()),
                               radioButtons("Cr_pos", "Crown Position:",
                                            c("Upper" = "Upper","Middle" = "Middle", 
                                              "Lower" = "Lower"), select="Upper"),
                               radioButtons("Origin", "Stand Origin",
                                            c("Plantation" = "Plantation","Natural (fire origin)" = "Natural_F", 
                                              "Natural (harvest origin)" = "Natural_H"), select="Plantation"),
                               radioButtons("Leaf_age", "Foliar Age",
                                            c("Current year" = "Cur_yr","1-year-old" = "1_yr", 
                                              "Greater than 1-year-old" = "1-yr-more", "Mixed ages" = "Mixed ages"), select="Cur_yr"),
                               radioButtons("Diag_base", "Diagnosis is Based on:",
                                            c("Individual Sample" = "Indv_sam","Composite Sample" = "Comp_sam"), select="Indv_sam"),
                               numericInput("Comp_Number", "Number of Trees per Composite:", NA),
                               checkboxGroupInput("Pertinent", HTML("Pertinent Site/Stand <br/>Information:"),
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
                               selectInput(inputId="spp",label="Species",
                                           choices = c("Select Species" = NA,
                                                       "Lodgepole pine" = "Lodgepole pine", 
                                                       "Interior spruce" = "Interior spruce",
                                                       "Interior Douglas-fir" = "Interior Douglas-fir")),

                               numericInput("N_cont", "N (%)", NA, min = 0, step = 0.01),
                               numericInput("P_cont", "P (%)", NA, min = 0, step = 0.01),
                               numericInput("K_cont", "K (%)", NA, min = 0, step = 0.01),
                               numericInput("Ca_cont", "Ca (%)", NA, min = 0, step = 0.01),
                               numericInput("Mg_cont", "Mg (%)", NA, min = 0, step = 0.001),
                               numericInput("S_cont", "S (%)", NA, min = 0, step = 0.01),
                               numericInput("SO4_cont", HTML(paste0("SO",tags$sub("4"), " (ppm)")), NA, min = 0),
                               numericInput("B_cont", "B (ppm)", NA, min = 0, step = 0.1),
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
                          radioButtons("Repform", "Document format:", c("PDF", "HTML", "Word")),
                          downloadButton("downloadReport","Create Report")
                        )
                      ),
                      column(6,
                         h3(icon("tree"), "FOLIAR NUTRIENT DIAGNOSIS REPORT") ,

                         hr(),
                         h4(span("Caution!", style = "color:red"), "Please note that this report was made through the proto-version of the", tags$em("FOLIAR NUTRIENT DIAGNOSIS,"), "so the diagnosis and fertilizer prescription might contain inaccurate information."),

                         
                         hr(),
                         h4(icon("info-circle"), "Site infomation"),
                         htmlOutput("Site_Info"),
                         
                         hr(),
                         h4(icon("map-marked-alt"), "Sample Location"),
                         
                         leafletOutput("Loc_map", width = "100%", height = 500),
                         
                         
                         hr(),
                         h4(icon("pagelines"), "Foliar Nutrient Status"),
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
                    
               ),

                tabPanel("Visual Deficiency Symptoms", value= "Visual Symptoms",
                    fluidRow(
                      column(6, offset = 0, style='position:relative; left:15%',
                            includeMarkdown("visual_deficiency.md")
                      )
                )
                ),

                tabPanel("Example #1", value= "Example1",
                    fluidRow(
                      column(6, offset = 0, style='position:relative; left:15%',
                            includeHTML("Example_1.html")
                     )
                )
                ),
           
               tabPanel("Example #2", value= "Example2",
                    fluidRow(
                      column(6, offset = 0, style='position:relative; left:15%',
                             includeHTML("Example_2.html")
                      )
                    )
           ),
           
           column(width = 12,
                  style = "background-color:#003366; border-top:2px solid #fcba19;",
                  
                  tags$footer(class="footer",
                              tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                       tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                               tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                               tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                               tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                               tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                               tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                               tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                       )
                              )
                  )
           )
           
            
)


                      