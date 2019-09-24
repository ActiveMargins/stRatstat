
#Code last updated by D.Coutts on Sept.20, 2019

ui <- navbarPage("stRat stat",
navbarMenu("Digitize stratigraphic data", 
  #--------------------------------------First panel for Loading and Saving Data
  tabPanel("1. Save/Load Data" ,
    sidebarLayout(
      sidebarPanel(width = 3,
        h4("Save Data"),
        h6("To save inputs prior to processing: save then download the file"),
        actionButton("save_data", "Save Data"),
        downloadLink("downloadSaveData", "Download saved section data"),
        hr(),
        h4("Load Data"),
        h6("To load points prior to processing: select file then click load data"),
        fileInput("load_data_file", "Choose .csv save file"),
        actionButton("load_data", "Load Data")
      ), # end sidebarPanel
      mainPanel(
        p("Welcome to stRat stat, an R-based digitizer that converts drawn stratigraphic sections/core logs to a numeric format. Please refer to the GitHub repository and reference the documentation on the use of stRat stat."),
        p("GitHub repo: https://github.com/ActiveMargins/stRatstat")
      )
    ) #end sidebarLayout         
  ), #end first panel
  #--------------------------------------Second panel for digitizing data
  tabPanel("2. Digitize beds and grain sizes", 
    sidebarLayout(
      sidebarPanel( width = 2,
        fileInput(inputId = 'files', 
          label = 'Select an Image',
          accept=c('image/png', 'image/jpeg')
        ),
        hr(),
        numericInput("sectthick_top", "Top of section thickness", 50.5),
        numericInput("sectthick_base", "Base of section thickness", 0),
        hr(),
        checkboxGroupInput("GS_checkGroup", label = h5("What Grain Sizes are in the section?"), 
                           choices = list("Mud" = 1, 
                                          "Silt" = 2, 
                                          "Very fine sand" = 3, 
                                          "Fine sand" = 4, 
                                          "Medium sand" = 5, 
                                          "Coarse sand" = 6, 
                                          "Very coarse sand" = 7, 
                                          "Granule" = 8, 
                                          "Pebble" = 9, 
                                          "Cobble" = 10, 
                                          "Boulder" = 11),
                           selected = c(1,2,3,4,5,6,7,8,9,10)),
        hr(),
        radioButtons("pt_type", "Types of points to record:",
                     choices = list("Bottom and top of section" = 1,
                                    "Grain size divisions" = 2,
                                    "Bed tops" = 3,
                                    "Grain size profiles" = 4)),
        checkboxInput("recordpts", "Record points ON/OFF", value = FALSE, width = NULL)
      ), # end of sidebarPanel
      
      mainPanel(
        fluidRow(
          column(width = 12, height = 500,
                 h4("Drag window to zoom on LEFT plot. Digitize points on the RIGHT plot."),
                 fluidRow(
                   column(width = 5,
                          plotOutput("plot2", height = 500,
                                     brush = brushOpts(
                                       id = "plot2_brush",
                                       resetOnNew = TRUE
                                     )
                          )
                    ), # end of column
                   column(width = 5, height = 500,
                          plotOutput("plot3", height = 500, click="plotclick")
                    ) # end of column
                ) # end of fluidRow
          ) #end column
        ), #end fluid row
        
        fluidRow(
          actionButton("delete_pt", "Delete last point"),
          actionButton("delete_row", "Delete selected row(s) in table"),
          actionButton("updateplot3","UpdatePlot"),
          hr(),
          DT::dataTableOutput('pttable')
        ) #end fluid row
      ) #end mainPanel
    ) #end sidebarLayout
  ), #end tabPanel
  #--------------------------------------Second panel for digitizing sedimentary structures
  tabPanel("3. Digitize sedimentary structures and features",
    sidebarLayout(
      sidebarPanel(width = 4,
                   textInput("sedstruct_text", "Sedimentary Structure name", value = "(e.g., ripple cross lamination)"),
                   actionButton("create_sedstruct_name", "Create sedimentary structure category"),
                   actionButton("delete_sedstruct_name", "Delete sedimentary structure category"),
                   uiOutput("sedstructchoices"),
                   actionButton("rec_sed_brush", "Record sedimentary structure interval")
      ), #end of sidebarPanel
      mainPanel(
        fluidRow(
          column(width = 12, height = 500,
          h4("Drag window to zoom on LEFT plot. Digitize points on the RIGHT plot."),
            fluidRow(
              column(width = 5, height = 500,
                plotOutput("plot4", height= "500px", width = "100%",brush = brushOpts(id = "plot4_brush"))
              ),
              column(width = 5,height = 500,
                plotOutput("plot5", height= "500px", width = "100%", brush = brushOpts(id = "plot5_brush"))
              )
            ) # end of fluidRow
          ) # end of column
        ), # end of fluidRow
        fluidRow(
          actionButton("delete_pt_sed", "Delete last interval"),
          actionButton("delete_row_sed", "Delete selected row(s) in table"),
          hr(),
          DT::dataTableOutput('sedstrattable')
        ) #end fluid row
      ) # end of main panel
    ) # end of sidebarLayout
  ), # end of tabPanel
  #--------------------------------------Third panel for digitizing facies intervals
  tabPanel("4. Digitize lithofacies intervals",
    sidebarLayout(
      sidebarPanel(width = 4,
                   textInput("facies_text", "Facies name", value = "Facies name...."),
                   actionButton("create_fac_name", "Create facies category"),
                   actionButton("delete_fac_name", "Delete facies category"),
                   uiOutput("facieschoices"),
                   actionButton("update_fac", "Record facies interval"),
                   checkboxInput("modifyfacies", "Clip facies boundaries to closest beds?", value = TRUE, width = NULL)
      ), #end of sidebarPanel
              
      mainPanel(
        fluidRow(
          column(width = 12, height = 500,
                  h4("Drag window to zoom on LEFT plot. Digitize points on the RIGHT plot."),
                  fluidRow(
                    column(width = 5, height = 500,
                            plotOutput("plot6", height= "500px", width = "100%",brush = brushOpts(id = "plot6_brush"))
                    ),
                    column(width = 5,height = 500,
                            plotOutput("plot7", height= "500px", width = "100%", brush = brushOpts(id = "plot7_brush" ))
                    )
                  ) # end of fluid row
          ) # end of column
        ), # end of fluid row
        fluidRow(
          actionButton("delete_pt_fac", "Delete last interval"),
          actionButton("delete_row_fac", "Delete selected row(s) in table"),
          hr(),
          DT::dataTableOutput('factable')
        ) # end fluid row
      ) # end of main panel
    ) # end of sidebar layout            
  ), # end tabPanel
  #--------------------------------------Fourth panel for digitizing element intervals
  tabPanel("5. Digitize architectural elements",
    sidebarLayout(
      sidebarPanel( width = 4,
                    textInput("element_text", "Element Name", value = "Element name...."),
                    actionButton("create_element_name", "Create element cateogry"),
                    actionButton("delete_element_name", "Delete element category"),
                    uiOutput("elementchoices"),
                    actionButton("update_element", "Record element interval"),
                    checkboxInput("modifyelements", "Clip element boundaries to closest beds?", value = TRUE, width = NULL)
      ), #end sidebarPanel
              
      mainPanel(
        fluidRow(
          column(width = 12, height = 500,
                 h4("Drag window to zoom on LEFT plot. Digitize points on the RIGHT plot."),
                 fluidRow(
                   column(width = 5, height = 500,
                            plotOutput("plot8", height= "500px", width = "100%",brush = brushOpts(id = "plot8_brush"))
                          ),
                   column(width = 5,height = 500,
                          plotOutput("plot9", height= "500px", width = "100%", brush = brushOpts(id = "plot9_brush" ))
                          )
                 )
          )
        ), #end fluid row
        fluidRow(
          actionButton("delete_pt_element", "Delete last interval"),
          actionButton("delete_row_element", "Delete selected row(s) in table"),
          hr(),
          DT::dataTableOutput('elementtable')
        ) # end of fluid row
      ) #end main panel
    ) #end sidebarLayout
  ), #end tab panel
  #--------------------------------------Fifth panel for digitizing element set/intervals
  tabPanel("6. Digitize architectural element sets or stratigraphic intervals",
    sidebarLayout(
      sidebarPanel(width = 4,
                  textInput("element_set_text", "Element set name", value = "Element Set name...."),
                  actionButton("create_element_set_name", "Create element set"),
                  actionButton("delete_elementset_name", "Delete element set"),
                  uiOutput("elementsetchoices"),
                  actionButton("update_elementset", "Update element set selection"),
                  checkboxInput("modifyelementsets", "Clip element set boundaries to closest beds?", value = TRUE, width = NULL)
      ),
            
      mainPanel(
        fluidRow(
          column(width = 12, height = 500,
            h4("Drag window to zoom on LEFT plot. Digitize points on the RIGHT plot."),
            fluidRow(
              column(width = 5, height = 500,
                     plotOutput("plot10", height= "500px", width = "100%",brush = brushOpts(id = "plot10_brush"))
              ),
              column(width = 5,height = 500,
                     plotOutput("plot11", height= "500px", width = "100%", brush = brushOpts(id = "plot11_brush"))
              )
            )
          ) #end of column
        ), # end of fluid row
        fluidRow(
          actionButton("delete_element_set", "Delete Last Interval"),
          actionButton("delete_row_elementset", "Delete Selected Row(s) in Table"),
          hr(),
          DT::dataTableOutput('elementsettable')
        ) # end of fluidrow
      ) # end of mainPanel
    ) # end of sidebar layout            
  ), #end of tab panel
  #--------------------------------------Seventh panel for setting up award numeric things
  tabPanel("7. Numeric Settings",
    fluidPage(
      fluidRow(
        textInput("MudSize", "Mud/Clay Size", value = "0.001"),
        textInput("SiltSize", "Silt Size", value = "0.0332"),
        textInput("VeryFineSize", "Very Fine Sand Size", value = "0.09375"),
        textInput("FineSize", "Fine Sand Size", value = "0.1875"),
        textInput("MediumSize", "Medium Sand Size", value = "0.35"),
        textInput("CoarseSize", "Coarse Sand Size", value = "0.75"),
        textInput("VeryCoarseSize", "Very Coarse Sand Size", value = "1.5"),
        textInput("GranuleSize", "Granule Size", value = "3"),
        textInput("PebbleSize", "Pebble Size", value = "34"),
        textInput("CobbleSize", "Cobble Size", value = "160"),
        textInput("BoulderSize", "Boulder Size", value = "256"),
        hr(),
        textInput("ResGS","Reservoir Grain Size Cutoff", value = "0.063475"),
        hr(),
        textInput("StratInc","Incriment to discretize data at", value = "0.01")
      ) # end of fluidRow
    ) # end of fluidPage
  ), #end of panel
  #--------------------------------------Eight panel for final naming prior to hitting go and process the data
  tabPanel("8. Process digitized data",
    sidebarLayout(
      sidebarPanel(
        textInput("section_name", "Section/Core Name", value = ""),
        textInput("section_loc", "Section/Core Location", value=""),
        textInput("UTM_E", "UTM Easting", value=""),
        textInput("UTM_N", "UTM Northing", value=""),
        hr(),
        actionButton("process_pts", "Process Selected Points"),
        hr(),
        downloadLink("downloadSectData", "Download processed section data")
      ), #end of side bar
          
      mainPanel(
        tableOutput('SectDataTable')
      ) #end of main pannel
    ) # end of page
  ) # end of panel
), # end of navbarMenu

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------Appending Discrete Data to Digitized Data-------------------------------------------------------------------------------------------  
#Panel for adding discrete measurements that we don't "digitize" they have already been measured (fractures, porosity, permeability, paleocurrent, core orientation)
   
navbarMenu("Append Additional Measurements",
  tabPanel("Import discrete data",
    sidebarLayout(
      sidebarPanel(
        h4("Import discrete measurement data"),
        p("This page will append discrete measurement data (e.g., prosity/permeability samples, fracture orientations/dimensions) to a already created digitized file. If summarized, all imported measurements will be summarized."),
        fileInput("sectfile", "Choose DIGITIZED SECTION/CORE data .csv file of data to add additional measurements too",
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")
        ), # end of fileInput
        fileInput("measurementfile", "Choose .csv file of MEASUREMENT data to append to digitized section/core data",
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")
        ), # end of fileInput
        checkboxInput("header", "Header in additional measurement data?", TRUE),
        hr(),
        numericInput("ThickColumn", label = "Which column is depth/thickness", value=1, min=1, max=100),
        hr(),
        checkboxGroupInput("SummarizeMeasurementChoices", "Summarize measurements by", choices = c("Bed" = "summBed"),
                                                                                                           #"Facies" = "summFac", this is not currently programmed in
                                                                                                           #"Element" = "summElement", this is not currently programmed in
                                                                                                           #"Element Set" = "summElementSet"),this is not currently programmed in
                                   selected = NULL, inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL),
        p("stRat stat currently summarizes continuous data by bed - to be changed soon"),
                
        actionButton("ImportMeasurments_Discrete", "Join measurements to section data"),
        hr(),
        downloadLink("downloadMergedMeasurementData", "Download merged data")
      ), #end of sidebar Panel
      mainPanel(
        tableOutput("MeasurementContents")
      ) # end of main panel
    ) # end of sidebarLayout
  ), # end of tab for discrete measurements
   
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------Appending Continuous Data to Digitized Data-------------------------------------------------------------------------------------------  
#Panel for adding discrete measurements that we don't "digitize" they have already been measured (fractures, porosity, permeability, paleocurrent, core orientation)
     
  tabPanel("Import continuous data",
    sidebarLayout(
      sidebarPanel(
        h4("Import continuous/log data"),
        p("This page will append continuous measurement data (e.g., petrophysical well log data) to a already created digitized file. If summarized, all imported measurements will be summarized."),
        fileInput("sectfile2", "Choose DIGITIZED SECTION/CORE data .csv file of data to add additional measurements too",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")
        ),
        fileInput("measurementfile2", "Choose .csv file of MEASUREMENT data to append to digitized section/core data",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")
        ),
        checkboxInput("header2", "Header in additional measurement data?", TRUE),
        hr(),
        numericInput("ThickColumn2", label = "Which column is depth/thickness", value = 1, min=1, max=100),
        hr(),
        checkboxGroupInput("SummarizeMeasurementChoices2", "Summarize measurements by", choices = c("Bed" = "summBed2"),
                                                                                                    #"Facies" = "summFac2", - to be implemented soon
                                                                                                    #"Element" = "summElement2", - to be implemented soon
                                                                                                    #"Element Set" = "summElementSet2"),  - to be implemented soon
                                         selected = NULL, inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL),
        p("stRat stat currently summarizes continuous data by bed - to be changed soon"),
        actionButton("ImportMeasurments_Continuous", "Join measurements to section data"),
        hr(),
        downloadLink("downloadMergedMeasurementData2", "Download merged data")
        ), #end of sidebar Panel
        mainPanel(
          tableOutput("MeasurementContents2")
        ) # end of main panel
      ) # end of side bar layout
    ), # end of tab panel for continuous data
   
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------Creating stratigraphic libarary-------------------------------------------------------------------------------------------  
  tabPanel("Create Stratigraphic Data Library",
    sidebarLayout(
      sidebarPanel(
        fileInput("Sectfile", "Choose pre-digitzed stratigraphic .csv file",
                        accept = c(".csv")
        ),
        actionButton("ImportSectionData", "Import selected file"),
        actionButton("DeleteSectionData", "Delete last uploaded file"),
        hr(),
        downloadLink("downloadTotalData", "Download merged data")
      ),
        mainPanel(
          fluidRow(
            tableOutput('SectDataToImport')
          ) # end of fluidRow
        ) # end of mainPanel
    ) # end of sidebarLayout 
  ) # end of tabPanel
) # end of Navbar
) # end of UI
