#' @title Launch stRat stat
#' @description This function launches the stRat stat application in a R Shiny window. See the GitHub repository for more detailed information.
#' @import shiny imager ggplot2 dplyr zoo DT
#' @return This function launches the stRat stat program in a shiny window
#' @export


launchstRatstat <- function(){

# User interface function for stRat stat
ui <- navbarPage("stRat stat",
                 navbarMenu("Digitize stratigraphic data",
                            
                            ##################################################################
                            #First panel for Loading and Saving Data
                            ##################################################################
                            tabPanel("1. Save/Load Data" ,
                                     sidebarLayout(
                                       sidebarPanel(width = 3,
                                                    h4("Save Data"),
                                                    h6("To save inputs prior to processing: save then download the file"),
                                                    actionButton("save_data", "Compile save file"),
                                                    downloadButton("downloadSaveData", "Download save file"),
                                                    hr(),
                                                    h4("Load Data"),
                                                    h6("To load points prior to processing: select file then click load data"),
                                                    fileInput("load_data_file", "Choose .csv save file"),
                                                    actionButton("load_data", "Load Data")
                                       ), # end sidebarPanel
                                       mainPanel(
                                         p("Welcome to stRat stat, an R-based digitizer that converts drawn stratigraphic sections/core logs to a numeric format."),
                                         p("Please refer to the GitHub repository for documentation and example datasets"),
                                         p("GitHub repo: https://github.com/ActiveMargins/stRatstat")
                                       ) # end main panel
                                     ) #end sidebarLayout
                            ), #end first panel
                            
                            ##################################################################
                            #Second panel for digitizing data
                            ##################################################################
                            tabPanel("2. Digitize beds and grain sizes",
                                     sidebarLayout(
                                       sidebarPanel( width = 2,
                                                     fileInput(inputId = 'files',
                                                               label = 'Select an Image',
                                                               accept=c('image/png', 'image/jpeg')
                                                     ),
                                                     hr(),
                                                     numericInput("sectthick_top", "Top of section thickness", 1),
                                                     numericInput("sectthick_base", "Base of section thickness", 0),
                                                     hr(),
                                                     checkboxGroupInput("GS_checkGroup", label = h5("What grain sizes divisions are included in the section?"),
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
                                                                                       "Boulder" = 11)),
                                                     hr(),
                                                     radioButtons("pt_type", "Types of points to record:",
                                                                  choices = list("Bottom and top of section (picked bottom then top)" = 1,
                                                                                 "Grain size divisions (picked finest to coarsest)" = 2,
                                                                                 "Bed tops" = 3,
                                                                                 "Grain size profile" = 4)),
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
                                                                        resetOnNew = TRUE)
                                                           )
                                                    ), # end of column
                                                    column(width = 5, height = 500, plotOutput("plot3", height = 500, click="plotclick")
                                                    ) # end of column
                                                  ) # end of fluidRow
                                           ) #end column
                                         ), #end fluid row
                                         
                                         fluidRow(
                                           actionButton("delete_pt", "Delete last point"),
                                           actionButton("delete_row", "Delete selected row(s) in table"),
                                           actionButton("updateplot3","Update right-hand plot"),
                                           hr(),
                                           DT::dataTableOutput('pttable')
                                         ) #end fluid row
                                       ) #end mainPanel
                                     ) #end sidebarLayout
                            ), #end tabPanel
                            
                            ##################################################################
                            #Second panel for digitizing sedimentary structures
                            ##################################################################
                            tabPanel("3. Digitize sedimentary structures and features",
                                     sidebarLayout(
                                       sidebarPanel(width = 4,
                                                    textInput("sedstruct_text", "Sedimentary structure name", value = "(e.g., ripple cross lamination)"), #text input for the name of the sedimentary structure
                                                    actionButton("create_sedstruct_name", "Create sedimentary structure category"), #button to create the sedimentary structure category
                                                    actionButton("delete_sedstruct_name", "Delete sedimentary structure category"), #button to delete the last created sedimentary structure category
                                                    uiOutput("sedstructchoices"), #dynamic radio button list of created sedimentary structures
                                                    checkboxInput("modifysedstructs", "Clip sedimentary structure boundaries to closest beds?", value = FALSE, width = NULL), #check box to clip the logged facies interval to the nearest beds
                                                    actionButton("rec_sed_brush", "Record sedimentary structure interval") #button to log/record the brushed
                                       ), #end of sidebarPanel
                                       mainPanel(
                                         fluidRow(
                                           column(width = 12, height = 500,
                                                  h4("Drag window to zoom on LEFT plot. Digitize points on the RIGHT plot."),
                                                  fluidRow(
                                                    column(width = 5, height = 500,
                                                           plotOutput("plot4", height= "500px", width = "100%",brush = brushOpts(id = "plot4_brush")) #plot to navigate (left)
                                                    ),
                                                    column(width = 5,height = 500,
                                                           plotOutput("plot5", height= "500px", width = "100%", brush = brushOpts(id = "plot5_brush")) #plot to brush selected interval (right)
                                                    )
                                                  ) # end of fluidRow
                                           ) # end of column
                                         ), # end of fluidRow
                                         fluidRow(
                                           actionButton("delete_pt_sed", "Delete last interval"), #button to delete last logged sedimentary structure interval
                                           actionButton("delete_row_sed", "Delete selected row(s) in table"), #button to delete all selected rows in the table
                                           hr(),
                                           DT::dataTableOutput('sedstrattable') #table to display the logged sedimentary structure data
                                         ) #end fluid row
                                       ) # end of main panel
                                     ) # end of sidebarLayout
                            ), # end of tabPanel
                            
                            ##################################################################
                            #Third panel for digitizing facies intervals
                            ##################################################################
                            tabPanel("4. Digitize lithofacies intervals",
                                     sidebarLayout(
                                       sidebarPanel(width = 4,
                                                    textInput("facies_text", "Facies name", value = "(e.g., channel axis)"), #text input for the facies name
                                                    actionButton("create_fac_name", "Create facies category"), #button to create the facies category
                                                    actionButton("delete_fac_name", "Delete facies category"), #button to delete the last created facies category
                                                    uiOutput("facieschoices"), #dynamic radio button list of created facies categories
                                                    checkboxInput("modifyfacies", "Clip facies boundaries to closest beds?", value = TRUE, width = NULL), #check box to clip the logged facies interval to the nearest beds
                                                    actionButton("update_fac", "Record facies interval") #button to log/record the brushed interval on the right
                                                    
                                       ), #end of sidebarPanel
                                       
                                       mainPanel(
                                         fluidRow(
                                           column(width = 12, height = 500,
                                                  h4("Drag window to zoom on LEFT plot. Digitize points on the RIGHT plot."),
                                                  fluidRow(
                                                    column(width = 5, height = 500,
                                                           plotOutput("plot6", height= "500px", width = "100%",brush = brushOpts(id = "plot6_brush")) #left plot to navigate on the right plot
                                                    ),
                                                    column(width = 5,height = 500,
                                                           plotOutput("plot7", height= "500px", width = "100%", brush = brushOpts(id = "plot7_brush" )) #right plot to brush the desired interval
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
                            
                            ##################################################################
                            #Fourth panel for digitizing element intervals
                            ##################################################################
                            tabPanel("5. Digitize architectural elements",
                                     sidebarLayout(
                                       sidebarPanel(width = 4,
                                                    textInput("element_text", "Element Name", value = "(e.g., Channel 1)"), #text input for the element name
                                                    actionButton("create_element_name", "Create element cateogry"), #button to create a new element category
                                                    actionButton("delete_element_name", "Delete element category"), #button to delete the last created element category
                                                    uiOutput("elementchoices"), #dynamic radio button list of created element categories
                                                    actionButton("update_element", "Record element interval"), #button to log/record the brushed interval as the selected category
                                                    checkboxInput("modifyelements", "Clip element boundaries to closest beds?", value = TRUE, width = NULL) #check box to clip the selected element interval to the nearest bed boundaries
                                       ), #end sidebarPanel
                                       
                                       mainPanel(
                                         fluidRow(
                                           column(width = 12, height = 500,
                                                  h4("Drag window to zoom on LEFT plot. Digitize points on the RIGHT plot."),
                                                  fluidRow(
                                                    column(width = 5, height = 500,
                                                           plotOutput("plot8", height= "500px", width = "100%",brush = brushOpts(id = "plot8_brush")) #left plot for navigating
                                                    ),
                                                    column(width = 5,height = 500,
                                                           plotOutput("plot9", height= "500px", width = "100%", brush = brushOpts(id = "plot9_brush" )) #right plot for brushing the desired interval
                                                    )
                                                  )#end fluid row
                                           )#end column
                                         ), #end fluid row
                                         fluidRow(
                                           actionButton("delete_pt_element", "Delete last interval"), #button to delete the last logged interval from the recorded data
                                           actionButton("delete_row_element", "Delete selected row(s) in table"), #button to delete all the selected rows from the table
                                           hr(),
                                           DT::dataTableOutput('elementtable') #table to display logged element intervals
                                         ) # end of fluid row
                                       ) #end main panel
                                     ) #end sidebarLayout
                            ), #end tab panel
                            
                            ##################################################################
                            #Fifth panel for digitizing element set/intervals
                            ##################################################################
                            tabPanel("6. Digitize architectural element sets or stratigraphic intervals",
                                     sidebarLayout(
                                       sidebarPanel(width = 4,
                                                    textInput("element_set_text", "Element set/interval name", value = "(e.g., Channel complex 1)"), #text input for the element set/interval
                                                    actionButton("create_element_set_name", "Create element set"), #button to create a new element set/interval category
                                                    actionButton("delete_elementset_name", "Delete element set"), #button to delete the last created element set/interval category
                                                    uiOutput("elementsetchoices"), #dynamic radio button list of created element set/interval category
                                                    checkboxInput("modifyelementsets", "Clip element set boundaries to closest beds?", value = TRUE, width = NULL), #check box to clip the selected element set interval to the nearest bed boundaries
                                                    actionButton("update_elementset", "Update element set selection") #button to log/record the brushed itnerval as the selected category
                                                    
                                       ), # end of sidebar
                                       
                                       mainPanel(
                                         fluidRow(
                                           column(width = 12, height = 500,
                                                  h4("Drag window to zoom on LEFT plot. Digitize points on the RIGHT plot."),
                                                  fluidRow(
                                                    column(width = 5, height = 500,
                                                           plotOutput("plot10", height= "500px", width = "100%",brush = brushOpts(id = "plot10_brush")) #left plot for navigating
                                                    ),
                                                    column(width = 5,height = 500,
                                                           plotOutput("plot11", height= "500px", width = "100%", brush = brushOpts(id = "plot11_brush")) #right plot for brushing the desired interval
                                                    )
                                                  ) #end of fluid row
                                           ) #end of column
                                         ), # end of fluid row
                                         fluidRow(
                                           actionButton("delete_element_set", "Delete Last Interval"), #button to delete the last logged interval from the recorded element set data
                                           actionButton("delete_row_elementset", "Delete Selected Row(s) in Table"), #button to delete all selected rows from the table
                                           hr(),
                                           DT::dataTableOutput('elementsettable') #table to display logged element set intervals
                                         ) # end of fluidrow
                                       ) # end of mainPanel
                                     ) # end of sidebar layout
                            ), #end of tab panel
                            
                            ##################################################################
                            #Seventh panel for setting up award numeric things
                            ##################################################################
                            tabPanel("7. Numeric Settings",
                                     fluidPage(
                                       fluidRow(
                                         textInput("MudSize", "Mud/clay size", value = "0.001"), #text inputs for each grain size deviation
                                         textInput("SiltSize", "Silt size", value = "0.0332"),
                                         textInput("VeryFineSize", "Very fine sand size", value = "0.09375"),
                                         textInput("FineSize", "Fine sand size", value = "0.1875"),
                                         textInput("MediumSize", "Medium sand size", value = "0.35"),
                                         textInput("CoarseSize", "Coarse sand size", value = "0.75"),
                                         textInput("VeryCoarseSize", "Very coarse sand size", value = "1.5"),
                                         textInput("GranuleSize", "Granule size", value = "3"),
                                         textInput("PebbleSize", "Pebble size", value = "34"),
                                         textInput("CobbleSize", "Cobble size", value = "160"),
                                         textInput("BoulderSize", "Boulder size", value = "256"),
                                         hr(),
                                         textInput("ResGS","Reservoir grain size cutoff", value = "0.063475"), #resrvoir cuttoff to designate reservoir (1) from non-reservoir (0)
                                         hr(),
                                         numericInput("StratInc","Increment  to discretize data at", value = "0.01") #the incriment used to create the stratigraphic section ##SHOULD BE A NUMERIC INPUT!
                                       ) # end of fluidRow
                                     ) # end of fluidPage
                            ), #end of panel
                            
                            ##################################################################
                            #Eight panel for final naming prior to hitting go and process the data
                            ##################################################################
                            tabPanel("8. Process digitized data",
                                     #sidebarLayout(
                                     fluidPage(
                                       fluidRow(
                                         h4("Section name location options"),
                                         column(3,
                                                textInput("section_name", "Section/Core Name", value = ""), #text inputs for the location of the stratigraphic sections
                                                textInput("section_loc", "Section/Core Location", value=""),
                                                textInput("UTM_E", "UTM Easting", value=""),
                                                textInput("UTM_N", "UTM Northing", value="")
                                         ), #end of column
                                         
                                         h4("Which statistics do you want to compute?"),
                                         column(5,
                                                checkboxInput("BedStats", "Compute bed-scale statistics"), #check box inputs to run the different levels of statistics
                                                checkboxInput("FaciesStats", "Compute facies-scale statistics"),
                                                checkboxInput("ElementStats", "Compute element-scale statistics"),
                                                checkboxInput("ElementSetStats", "Compute element-set/strat. interval statistics")
                                         ) #end of column
                                       ), #end fluidRow
                                       hr(),
                                       fluidRow(actionButton("process_pts", "Process selected points")), #button to start the descretization process given the selected inputs),
                                       hr(),
                                       fluidRow(downloadButton("downloadSectData", "Download processed data in stRat stat format")), #download the processed data once complete),
                                       fluidRow(downloadButton("downloadStripLog", "Download processed data in graphiclog format")) #download the processed data once complete)
                                     ) # end of page
                            ) # end of panel
                 ), # end of navbarMenu
                 
                 ###################################################################
                 #Appending Discrete Data to Digitized Data
                 ##################################################################
                 #Panel for adding discrete measurements that we don't "digitize" they have already been measured (fractures, porosity, permeability, paleocurrent, core orientation)
                 navbarMenu("Append Additional Measurements",
                            tabPanel("Import discrete data",
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4("Import discrete measurement data"),
                                         p("This page will append discrete measurement data (e.g., prosity/permeability samples, fracture orientations/dimensions) to a already created digitized file. If summarized, all imported measurements will be summarized."),
                                         fileInput("sectfile", "Choose DIGITIZED STRATIGRAPHIC SECTION data .csv file of data to add additional measurements too", #file input for the already digitized stratigraphic section data file
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                                         ), # end of fileInput
                                         fileInput("measurementfile", "Choose .csv file of MEASUREMENT data to append to digitized stratigraphic section data", #file input for the measurements that are to be joined
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                                         ), # end of fileInput
                                         checkboxInput("header", "Header in additional measurement data?", TRUE),
                                         hr(),
                                         numericInput("ThickColumn", label = "Which column is depth/thickness", value=1, min=1, max=100), #numeric input to specify which column within the input stratigraphic section file is the thickness
                                         hr(),
                                         h5("Summarize continuous data by:"),
                                         checkboxInput("Discrete_BedStat", "Bed"), #check boxes to specify which level of heirarchy statistics should be computed on
                                         checkboxInput("Discrete_FaciesStat", "Facies"),
                                         checkboxInput("Discrete_ElementStat", "Element"),
                                         checkboxInput("Discrete_ElementSetStat", "Element set/Interval"),
                                         actionButton("ImportMeasurments_Discrete", "Join measurements to section data"), #button to run the joining process
                                         downloadButton("downloadMergedMeasurementData", "Download merged data") #button to download the joined and summarized data
                                       ), #end of sidebar Panel
                                       mainPanel(
                                         tableOutput("MeasurementContents") #table to display the measurement file, once it is selected in the file input
                                       ) # end of main panel
                                     ) # end of sidebarLayout
                            ), # end of tab for discrete measurements
                            
                            ##################################################################
                            #Appending Continuous Data to Digitized Data
                            ##################################################################
                            #Panel for adding discrete measurements that we don't "digitize" they have already been measured (fractures, porosity, permeability, paleocurrent, core orientation)
                            tabPanel("Import continuous data",
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4("Import continuous/log data"),
                                         p("This page will append continuous measurement data (e.g., petrophysical well log data) to a already created digitized file. If summarized, all imported measurements will be summarized."),
                                         fileInput("sectfile2", "Choose DIGITIZED STRATIGRAPHIC SECTION data .csv file of data to add additional measurements too", #file input for the already digitized stratigraphic section data file
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                                         ),
                                         fileInput("measurementfile2", "Choose .csv file of MEASUREMENT data to append to digitized section/core data", #file input for the measurements that are to be joined
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                                         ),
                                         checkboxInput("header2", "Header in additional measurement data?", TRUE),
                                         hr(),
                                         numericInput("ThickColumn2", label = "Which column is depth/thickness", value = 1, min=1, max=100), #numeric input to specify which column within the input stratigraphic section file is the thickness
                                         hr(),
                                         h5("Interpolate continuous data"),
                                         checkboxInput("Interpolate_Continuous", "Interpolate data"), #check boxes to specify if continuous data should be interpolated across all descritized intervals
                                         hr(),
                                         h5("Summarize continuous data by:"),
                                         checkboxInput("Continuous_BedStat", "Bed"), #check boxes to specify which level of heirarchy statistics should be computed on
                                         checkboxInput("Continuous_FaciesStat", "Facies"),
                                         checkboxInput("Continuous_ElementStat", "Element"),
                                         checkboxInput("Continuous_ElementSetStat", "Element set/Interval"),
                                         p("stRat stat currently summarizes continuous data by bed - to be changed soon"),
                                         actionButton("ImportMeasurments_Continuous", "Join measurements to section data"),
                                         downloadButton("downloadMergedMeasurementData2", "Download merged data") #button to download the joined and summarized data
                                       ), #end of sidebar Panel
                                       mainPanel(
                                         tableOutput("MeasurementContents2") #table to display the measurement file, once it is selected in the file input
                                       ) # end of main panel
                                     ) # end of side bar layout
                            ), # end of tab panel for continuous data
                            
                            ##################################################################
                            #Creating stratigraphic libarary
                            ##################################################################
                            tabPanel("Create Stratigraphic Data Library",
                                     sidebarLayout(
                                       sidebarPanel(
                                         fileInput("Sectfile", "Choose pre-digitzed stratigraphic .csv file", #file input for already-made stratigraphic section files to be appened into a stratigraphic library
                                                   accept = c(".csv")
                                         ),
                                         actionButton("ImportSectionData", "Import selected file"), #button to append the selected stratigraphic section
                                         actionButton("DeleteSectionData", "Delete last uploaded file"), #button to remove the last selected stratigraphic section file
                                         downloadButton("downloadTotalData", "Download merged data") #button to download the compiled stratigraphic library
                                       ), #end sidebarPanel
                                       mainPanel(
                                         fluidRow(
                                           tableOutput('SectDataToImport') #reactive table to display information regarding the selected stratigraphic section files
                                         ) # end of fluidRow
                                       ) # end of mainPanel
                                     ) # end of sidebarLayout
                            ) # end of tabPanel
                 ) # end of Navbar
) # end of UI

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Reactive variables related to the image
  LoadedImage <- reactive({load.image(gsub("\\\\", "/", input$files$datapath))}) #Loaded image
  ImageWidth <- reactive(width(LoadedImage())) #Wdith of the image, used to scale the interactive plots
  ImageHeight <- reactive(height(LoadedImage())) #height of the image, used to scale the interactive plots
  null <-  reactiveValues(df=tibble("nullx"=c(-10,-9),"nully"=c(-10,-9))) #a "null" dataframe used when there is no portion of the stratigraphic section selected on the left hand plots
  
  #Reactive dataframes related to logging and processing the data
  values <- reactiveValues(df=tibble(x=-999,y=-999, pt=1)) #points to represent the grain size profile values x=position on x-axis of image, y=position on y-axis of image, pt=point number for the logged points
  topbottom <- reactiveValues(df=tibble(x=-999,y=-999,PtLabel="Example")) #points picked to locate the top and bottom of the section. x=position on the x-axis of the image, y=position on the y-axis of the image, label=string variable to lable as top and bottom
  gsmarkers <- reactiveValues(df=tibble(x=-999,y=-999,pt=1)) #points to represent the grain size division at the base of the section, x=position on the x-axis of the image, y=position on the y-axis of the image, pt=pt number for the logged points
  bedtops_raw <- reactiveValues(df=tibble(x=-999,y=-999, pt=1, BedTop=1)) #points picked to represent bed boundaries, x=position on the x-axis of the image, y=position on the y-axis of the image, pt=the number of the picked points, bedtop=numeric value of 1, which is used later to signify a the top of a bed in a discretized layer
  sedstrat_raw <- reactiveValues(df=tibble(ymin_raw=-999,ymax_raw=-998, SedStruct="Example", pt=0)) #recorded brushes of logged sedimentary structure intervals, y-min_raw and y-max_raw=the y-axis locations of brushes on the ggplot, SedStruct=the selected sedimnentary structure category from the list
  facies_raw <- reactiveValues(df=tibble(ymin_raw=-999,ymax_raw=-998, FacName="Example", FacBlockNum=0)) #recorded brushes of logged facies intervals, y-min_raw and y-max_raw=the y-axis locations of brushes on the ggplot, FacName=the selected facies category from the list, FaciesBlockNum=the incrementing number of different instances of logged facies - used to summarize data
  element_raw <- reactiveValues(df=tibble(ymin_raw=-999,ymax_raw=-998, ElementName="Example", ElementBlockNum=0)) #recorded brushes of logged element intervals, y-min_raw and y-max_raw=the y-axis locations of brushes on the ggplot, ElementName=the selected element category from the list, ElementBlockNum=the incrementing number of different instances of logged elements - used to summarize data
  elementset_raw <- reactiveValues(df=tibble(ymin_raw=-999,ymax_raw=-998, ElementSetName="Example", ElementSetBlockNum=0)) #recorded brushes of logged element set intervals, y-min_raw and y-max_raw=the y-axis locations of brushes on the ggplot, ElementSetName=the selected element set category from the list, ElementSetBlockNum=the incrementing number of different instances of logged element sets - used to summarize data
  

  values_plot <- reactiveValues(df=NULL) #points to represent the grain size profile values x=position on x-axis of image, y=position on y-axis of image, pt=point number for the logged points
  topbottom_plot <- reactiveValues(df=NULL) #points picked to locate the top and bottom of the section. x=position on the x-axis of the image, y=position on the y-axis of the image, label=string variable to lable as top and bottom
  gsmarkers_plot <- reactiveValues(df=NULL) #points to represent the grain size division at the base of the section, x=position on the x-axis of the image, y=position on the y-axis of the image, pt=pt number for the logged points
  bedtops_raw_plot <- reactiveValues(df=NULL) #points picked to represent bed boundaries, x=position on the x-axis of the image, y=position on the y-axis of the image, pt=the number of the picked points, bedtop=numeric value of 1, which is used later to signify a the top of a bed in a discretized layer
    
  sedstructnames <- reactiveValues(df=tibble(name = "Example Sed. Structure")) #a reactive list of names for sedimentary stucture categories
  faciesnames <- reactiveValues(df=tibble(name = "Example Facies")) #a reactive list of names for sedimentary stucture categories
  elementnames <- reactiveValues(df=tibble(name = "Example Element")) #a reactive list of names for sedimentary stucture categories
  elementsetnames <- reactiveValues(df=tibble(name = "Example Element Set")) #a reactive list of names for sedimentary stucture categories
  
  NumSectData <- reactiveValues(df=tibble(FileName="Example", NumRows = 1)) #Number of sections appeneded together in the stratigraphic library
  TotalSectionData <- reactiveValues(df=tibble(Thickness=0.1)) #Dataframe used in creating the stratigraphic library
  SectDataProcessed <- reactiveValues(df=tibble(Thickness=0.1)) #Dataframe for section data at the end processing the data
  SectDataJoined_Disc <- reactiveValues(df=tibble(Thickness=0.1))
  SectDataJoined_Cont <- reactiveValues(df=tibble(Thickness=0.1))
  StripLogData <- reactiveValues(df=tibble(BedNumber=1.0,name="Example",collection="Example",tops=1.0, th=1.0,gs_tops=1.0,sand_shl=1.0, mean_gs=1.0, max_gs=1.0, grain_size_dia="Example",grain_size_thick="Example"))
  
  #Reactive dataframe for saving the data
  SaveData <- reactiveValues(df=tibble(Save="Temp"))
  
  #Blank theme for the all the plots that are used to interact with the image of the section
  BlankTheme <- theme(axis.line=element_blank(),
                      axis.text.x=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank(),
                      legend.position="none",
                      panel.background=element_blank(),
                      panel.border=element_blank(),
                      panel.grid.major=element_blank(),
                      panel.grid.minor=element_blank(),
                      plot.background=element_blank())
  
  ############################################################################################################
  # Saving and loading data points
  ############################################################################################################
  #Saving the data
  observeEvent(input$save_data,{
    #Remove all data that is currently in the SaveData$df (except the first line - a place holder) so users can't save multiple copies of the same data to one dataframe.
    SaveData$df <- SaveData$df[1,]
    
    #Convert all the reactive dataframes in their current state into static dataframes with a new column that says their function
    topbottompts <- topbottom$df
    bedtops_raw <- bedtops_raw$df
    values_raw <- values$df
    gsmarkers_raw <- gsmarkers$df
    sedstrat_raw <- sedstrat_raw$df
    facies_raw <- facies_raw$df
    element_raw <- element_raw$df
    elementset_raw <- elementset_raw$df
    
    #Add a keyword to the save column to each used for parsing the data when its loaded
    topbottompts$Save <- "TopBottom"
    bedtops_raw$Save <- "BedTops"
    values_raw$Save <- "GSValues"
    gsmarkers_raw$Save <- "GSMarkers"
    sedstrat_raw$Save <- "SedStrut"
    facies_raw$Save <- "Facies"
    element_raw$Save <- "Elements"
    elementset_raw$Save<- "ElementSets"
    
    #Bind them all together to make the save file
    SaveData$df <- bind_rows(SaveData$df,topbottompts, bedtops_raw,values_raw,gsmarkers_raw,sedstrat_raw,facies_raw,element_raw,elementset_raw)
  })
  
  #Downloading Saved Data - use download handler to create a .csv with a name and system date
  output$downloadSaveData <- downloadHandler(
    filename = function() {
      paste("SavedStratgraphicData", Sys.Date(), ".csv", sep="")
    },
    content = function(con) {
      write.csv(SaveData$df, con, na = "", row.names=FALSE)
    }
  )
  
  #Loading data
  observeEvent(input$load_data, {
    
    inFile <- input$load_data_file
    if (is.null(inFile))
      return(showNotification("No file selected. No file loaded", type = "error"))
    
    LoadData <- read.csv(inFile$datapath, header=TRUE, stringsAsFactors=FALSE) #if there is a file we want to read the .csv
    if(c("Save") %in% colnames(LoadData)) { #if there is no inFile then alert the user and do nothing 
      #Get rid of anything already in reactive dataframes. This stops the user from loading in multiple datasets into stRat stat, it will only keep the most recently loaded data
      topbottom$df <- topbottom$df %>% filter(x<0)
      bedtops_raw$df <- bedtops_raw$df %>% filter(x<0)
      values$df <- values$df %>% filter(x<0)
      gsmarkers$df <- gsmarkers$df %>% filter(x<0)
      sedstrat_raw$df <- sedstrat_raw$df %>% filter(ymin_raw<0)
      facies_raw$df <- facies_raw$df %>% filter(ymin_raw<0)
      element_raw$df <- element_raw$df %>% filter(ymin_raw<0)
      elementset_raw$df <- elementset_raw$df %>% filter(ymin_raw<0)
      sedstructnames$df <- sedstructnames$df %>% filter(name=="Example Sed. Structure")
      faciesnames$df <- faciesnames$df %>% filter(name=="Example Facies")
      elementnames$df <- elementnames$df %>% filter(name=="Example Element")
      elementsetnames$df <- elementsetnames$df %>% filter(name=="Example Element Set")
      
      #Parse/filter the logged points data in the load file into differentdataframes. Filter each save type (e.g., BedTops) and select only the relevant columns and data 
      topbottompts_load <- LoadData %>% filter (Save=="TopBottom") %>% select(x,y,PtLabel) %>% filter (x>0)
      bedtops_load <- LoadData %>% filter(Save=="BedTops") %>% select(x, y, pt, BedTop) %>% filter (x>0)
      values_load<- LoadData %>% filter(Save=="GSValues") %>% select(x,y,pt) %>% filter (x>0)
      gsmarkers_load <- LoadData %>% filter(Save=="GSMarkers") %>% select(x,y,pt) %>% filter (x>0)
      sedstrat_load <- LoadData %>% filter(Save=="SedStrut") %>% select(ymin_raw, ymax_raw, SedStruct, pt) %>% filter (ymin_raw>0)
      facies_load <- LoadData %>% filter(Save=="Facies") %>% select(ymin_raw, ymax_raw, FacName, FacBlockNum) %>% filter (ymin_raw>0)
      element_load <- LoadData %>% filter(Save=="Elements") %>% select(ymin_raw, ymax_raw, ElementName, ElementBlockNum) %>% filter (ymin_raw>0)
      elementset_load <- LoadData %>% filter(Save=="ElementSets") %>% select(ymin_raw, ymax_raw, ElementSetName, ElementSetBlockNum) %>% filter (ymin_raw>0)
      
      #Parse the columns that have the names of sedimentary structures, facies, etc. from the loaded data. These are single strings, so we treat them a bit differently so we can bind it into the reactive dataframe. Also when we make the dataframe, it turns them into factors. So we need to turn them back into strings.
      sedstrat_name_load <- data.frame(name=unique(sedstrat_load$SedStruct))
      facies_name_load <- data.frame(name=unique(facies_load$FacName))
      element_name_load <- data.frame(name=unique(element_load$ElementName))
      elementset_name_load <- data.frame(name=unique(elementset_load$ElementSetName))
      sedstrat_name_load[] <- lapply(sedstrat_name_load, as.character)
      facies_name_load[] <- lapply(facies_name_load, as.character)
      element_name_load[] <- lapply(element_name_load, as.character)
      elementset_name_load[] <- lapply(elementset_name_load, as.character)
      
      #Bind each loading dataframe into a reactive dataframe that the rest of the software can recognize
      topbottom$df <- bind_rows(topbottom$df, topbottompts_load)
      bedtops_raw$df <- bind_rows(bedtops_raw$df, bedtops_load)
      values$df <- bind_rows(values$df, values_load)
      gsmarkers$df <- bind_rows(gsmarkers$df,gsmarkers_load)
      sedstrat_raw$df <- bind_rows(sedstrat_raw$df,sedstrat_load)
      facies_raw$df <- bind_rows(facies_raw$df,facies_load)
      element_raw$df <- bind_rows(element_raw$df,element_load)
      elementset_raw$df <- bind_rows(elementset_raw$df,elementset_load)
      sedstructnames$df <-  bind_rows(sedstructnames$df,sedstrat_name_load)
      faciesnames$df <- bind_rows(faciesnames$df,facies_name_load)
      elementnames$df <- bind_rows(elementnames$df,element_name_load)
      elementsetnames$df <- bind_rows(elementsetnames$df,elementset_name_load)
      
      showNotification("Data loaded successfully ", type = "message") #Send message to user
      
    } else {
      showNotification("Wrong file format. No File Loaded", type = "error")
    }
  })
  
  ############################################################################################################
  # CODE FOR DIGITIZING STRATIGRAPHIC DATA
  ############################################################################################################
  # -------------------------------------------------------------------Server code related to the page "2. Digitze beds and grain sizes"
  # Linked plots (middle and right)
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot2 <- renderPlot({ #left hand plot for manipulating the view on the right hand plot
    if(is.null(input$files)){} # if there's no file input don't plot anything
    else{ # if there is a file input then plot the interactive ggplot
      ggplot(null$df,aes(x=nullx,y=nully)) + #null data required to view the ggplot
        annotation_raster(LoadedImage(),ymin=0,ymax = height(LoadedImage()), xmin=0, xmax=width(LoadedImage())) + #loaded image in the background
        geom_point() +
        BlankTheme + #blank theme to remove axis and labels
        coord_fixed() + #disable changes in aspect ratio, or scales
        xlim(0, width(LoadedImage())) + #change the x-axis to fit the width of the image
        ylim(0, height(LoadedImage())) #change the y-axis to fit the height of the image
    }
  })
  
  output$plot3 <- renderCachedPlot({
    if(is.null(input$files)){} # if there's no file input don't plot anything
    else{ # if there is a file input then plot the interactive ggplot
      ggplot(data=null$df, aes(x=nullx,y=nully,color="NA"))+geom_point() +
        BlankTheme +
        annotation_raster(LoadedImage(),ymin=0,ymax=height(LoadedImage()), xmin=0, xmax=width(LoadedImage())) + #add the loaded image, but scale it so its the correct height and width of the image
        coord_cartesian(xlim=ranges2$x,ylim=ranges2$y) + #change the x and y axis to fit the box selected on plot 2
        geom_point(data=values_plot$df, aes(x=x,y=y),inherit.aes = FALSE, color="green") + #grain size profile poitns are displayed as green dots
        geom_point(data=gsmarkers_plot$df, aes(x=x,y=y), color="blue") + #grain size division along the bottom are displayed as blue dots
        geom_point(data=topbottom_plot$df, aes(x=x,y=y), color="red") + #top and bottom of section are red dots
        geom_hline(yintercept=bedtops_raw_plot$df$y, linetype="dashed", color = "red", size=1) + #bed boundaries are red dashed lines
        geom_label(data=topbottom_plot$df, aes(x=x, y=y, label=ifelse(x>1, as.character(PtLabel),''),hjust=1.25)) + #label the top and bottom
        geom_label(data=values_plot$df, aes(x=x, y=y, label=ifelse(x>1, as.character(pt),''),hjust=1.25)) + #label the grains size profile points
        geom_label(data=bedtops_raw_plot$df, aes(x=3, y=y, label=ifelse(x>1, as.character(pt),''))) #label the bed tops
    }
  },
  cacheKeyExpr = {list(input$updateplot3,input$plot2_brush, input$delete_pt, input$delete_row)} #only update the right hand plot, when the left plot is brushed, a points are deleted, or the update button is pushed
  )
  
  # When a double-click happens on the left plot, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
      #filter the points that fall within the y bounds of the brush on plot 2 + grab the place holder row the reactive dataframe. Use these filtered dataframes to create plot 3. This decreases the plotting time on plot 3 by roughty ~50%   
      values_plot$df <- rbind(filter(values$df, y>brush$ymin & y<brush$ymax),values$df[1,]) 
      bedtops_raw_plot$df <- rbind(filter(bedtops_raw$df, y>brush$ymin & y<brush$ymax),bedtops_raw$df[1,])
      gsmarkers_plot$df <- rbind(filter(gsmarkers$df, y>brush$ymin & y<brush$ymax),gsmarkers$df[1,])
      topbottom_plot$df <- rbind(filter(topbottom$df, y>brush$ymin & y<brush$ymax),topbottom$df[1,])
     
    } else {
      ranges2$x <- null$df$nullx
      ranges2$y <- null$df$nully
    }
  })
  
  #observe clicks on the right plot to get xy coordinates - only keep them if(recordpts == TRUE)
  observeEvent(input$plotclick,{
    if(input$recordpts == TRUE){
      if(input$pt_type == 1){ #sort the point by radio button input$pt_type
        if(nrow(topbottom$df)==1) {
          topbottom$df <- bind_rows(topbottom$df,tibble(x=input$plotclick$x, y=input$plotclick$y, PtLabel="Bottom")) #once sorted, bind_rows into the dataframe
        } else if (nrow(topbottom$df)==2) {
          topbottom$df <- bind_rows(topbottom$df,tibble(x=input$plotclick$x, y=input$plotclick$y, PtLabel="Top"))
        } else if (nrow(topbottom$df)>2) {
          NULL
        }
      }
      
      if(input$pt_type == 2){
        gsmarkers$df <- bind_rows(gsmarkers$df,tibble(x=input$plotclick$x, y=input$plotclick$y, pt=nrow(gsmarkers$df)+1))
      }
      if(input$pt_type == 3){
        bedtops_raw$df <- bind_rows(bedtops_raw$df,tibble(x=input$plotclick$x, y=input$plotclick$y, pt=nrow(bedtops_raw$df), BedTop=as.numeric(1)))
      }
      if(input$pt_type == 4){
        values$df <- bind_rows(values$df,tibble(x=input$plotclick$x, y=input$plotclick$y, pt=(nrow(values$df))))
      }
    }
  })
  
  output$pttable <- DT::renderDT( #Data table at the base of page 2. Display only the collected points that are selected in the radio buttons
    if(input$pt_type==1){
      datatable(filter(topbottom$df, y>0), rownames=FALSE, filter=c('none'))
    }else if(input$pt_type==2){
      datatable(filter(gsmarkers$df, y>0), rownames=FALSE,filter=c("none"))
    }else if(input$pt_type==3){
      datatable(filter(bedtops_raw$df, y>0), rownames=FALSE,filter=c("none"))
    }else if(input$pt_type==4){
      datatable(filter(values$df, y>0), rownames=FALSE,filter=c("none"))
    }
  )
  
  #function for delete last point action button
  observeEvent(input$delete_pt, { #delete the last row from the dataframe selected by the radio buttons
    if(input$pt_type == 1 & nrow(topbottom$df)>1){
      topbottom$df <- topbottom$df[-nrow(topbottom$df),]
      if(!is.null(input$plot2_brush)){
        topbottom_plot$df <- rbind(filter(topbottom$df, y>input$plot2_brush$range$top & y<input$plot2_brush$range$bottom),topbottom$df[1,])
      }
    }
    if(input$pt_type == 2 & nrow(gsmarkers$df)>1){
      gsmarkers$df <- gsmarkers$df[-nrow(gsmarkers$df),]
      if(!is.null(input$plot2_brush)){
        gsmarkers_plot$df <- rbind(filter(gsmarkers$df, y>input$plot2_brush$range$top & y<input$plot2_brush$range$bottom),gsmarkers$df[1,])
      }
    }
    if(input$pt_type == 3 & nrow(bedtops_raw$df)>1){
      bedtops_raw$df <- bedtops_raw$df[-nrow(bedtops_raw$df),]
      if(!is.null(input$plot2_brush)){
        bedtops_raw_plot$df <- rbind(filter(bedtops_raw$df, y>input$plot2_brush$range$top & y<input$plot2_brush$range$bottom),bedtops_raw$df[1,])
      }
    }
    if(input$pt_type == 4 & nrow(values$df)>1){
      values$df <- values$df[-nrow(values$df),]
      if(!is.null(input$plot2_brush)){
        values_plot$df <- rbind(filter(values$df, y>input$plot2_brush$range$top & y<input$plot2_brush$range$bottom),values$df[1,])
      }
    }
  })
  
  #function for deleting the selected row(s) from the dataframe selected by the radio buttons
  observeEvent(input$delete_row, {
    if(!is.null(input$pttable_rows_selected)){
      delete_row <- input$pttable_rows_selected + 1
      if(input$pt_type == 1){
        topbottom$df <- topbottom$df[-(delete_row),]
        if(!is.null(input$plot2_brush)){
          topbottom_plot$df <- rbind(filter(topbottom$df, y>input$plot2_brush$range$top & y<input$plot2_brush$range$bottom),topbottom$df[1,])
        }
      }
      if(input$pt_type == 2){
        gsmarkers$df <- gsmarkers$df[-(delete_row),]
        gsmarkers$df <- gsmarkers$df %>% mutate(pt=row_number()-1)
        if(!is.null(input$plot2_brush)){
          gsmarkers_plot$df <- rbind(filter(gsmarkers$df, y>input$plot2_brush$range$top & y<input$plot2_brush$range$bottom),gsmarkers$df[1,])
        }
      }
      if(input$pt_type == 3){
        bedtops_raw$df <- bedtops_raw$df[-(delete_row),]
        bedtops_raw$df <- bedtops_raw$df %>% mutate(pt = row_number()-1)
        if(!is.null(input$plot2_brush)){
          bedtops_raw_plot$df <- rbind(filter(bedtops_raw$df, y>input$plot2_brush$range$top & y<input$plot2_brush$range$bottom),bedtops_raw$df[1,])
        }
      }
      if(input$pt_type == 4){
        values$df <- values$df[-(delete_row),]
        values$df <- values$df %>% mutate(pt = row_number()-1)
        if(!is.null(input$plot2_brush)){
          values_plot$df <- rbind(filter(values$df, y>input$plot2_brush$range$top & y<input$plot2_brush$range$bottom),values$df[1,])
        }
      }
    }
  })
  
  # -------------------------------------------------------------------Server code related to the page "3. Digitize sedimentary structures and features"
  ranges2_sed <- reactiveValues(x = NULL, y = NULL)
  
  #Brush on plot 4 for changing zoom on plot 5
  output$plot4 <- renderPlot({
    if(is.null(input$files)){} # if there's no file input don't plot anything
    else{ # if there is a file input then plot the interactive ggplot
      ggplot(null$df,aes(x=nullx,y=nully))+
        annotation_raster(LoadedImage(),ymin=0,ymax =height(LoadedImage()), xmin=0, xmax=width(LoadedImage()))+
        BlankTheme+geom_point() +
        coord_fixed() +
        xlim(0, width(LoadedImage()))+
        ylim(0, height(LoadedImage()))
    }
  })
  
  output$plot5 <- renderPlot({
    if(is.null(input$files)){} # if there's no file input don't plot anything
    else{ # if there is a file input then plot the interactive ggplot
      ggplot(data=null$df, aes(x=nullx,y=nully)) +
        geom_point() +
        BlankTheme +
        annotation_raster(LoadedImage(),ymin=0,ymax =height(LoadedImage()), xmin=0, xmax=width(LoadedImage())) +
        coord_cartesian(xlim=ranges2_sed$x,ylim=ranges2_sed$y) +
        geom_hline(yintercept=bedtops_raw$df$y, linetype="dashed", color = "red", size=1)+
        geom_rect(data=sedstrat_raw$df, mapping=aes(xmin=0, xmax=width(LoadedImage()), ymin=sedstrat_raw$df$ymax_raw, ymax=sedstrat_raw$df$ymin_raw), color="blue", inherit.aes = FALSE, alpha = 0.1) +
        geom_label(data=sedstrat_raw$df, aes(x=(width(LoadedImage())/2), y=sedstrat_raw$df$ymax_raw+((sedstrat_raw$df$ymin_raw-sedstrat_raw$df$ymax_raw)/2), label=ifelse(sedstrat_raw$df$pt>0, as.character(sedstrat_raw$df$pt),''),hjust=1.25)) #label the logged sedimentary structures
    }
  })
  
  # If plot 4 is brushed change the range of the zoom on the GG plot to the brush range (x,y); if not, reset the zoom.
  observe({
    plot4brush <- input$plot4_brush
    if (!is.null(plot4brush)) {
      ranges2_sed$x <- c(plot4brush$xmin, plot4brush$xmax)
      ranges2_sed$y <- c(plot4brush$ymin, plot4brush$ymax)
    } else {
      ranges2_sed$x <- null$df$nullx
      ranges2_sed$y <- null$df$nully
    }
  })
  
  #Record brush on plot 5 as two y-coordinates and the sed structure value
  observeEvent(input$rec_sed_brush, {
    if(is.null(input$plot5_brush)){
    }else{
      #if the brushes are outside of the limits of the section then clip them back to the top or bottom of the image
      plot5brush <- input$plot5_brush
      topbottompts <- topbottom$df %>% filter(x>0)
      bedtops_raw <- bedtops_raw$df %>% filter(x>0)
      
      if(plot5brush$ymax[1]>(max(topbottompts$y))){
        plot5brush$ymax[1] <-(max(topbottompts$y))
      }
      if(plot5brush$ymin[1]<(min(topbottompts$y))){
        plot5brush$ymin[1] <-(min(topbottompts$y))
      }
      
      #if the user wants the Element boundaries to be clipped to bed boundaries then we'll do that
      if(input$modifysedstructs == TRUE){
        bedtops_raw <- bind_rows(topbottompts,bedtops_raw)
        
        #Top of the Element selection
        bedindex <-which(abs(bedtops_raw$y-plot5brush$ymax)==min(abs(bedtops_raw$y-plot5brush$ymax)))
        brushymax <- bedtops_raw[[bedindex,2]]
        
        #Bottom of the Element selection
        bedindex <- which(abs(bedtops_raw$y-plot5brush$ymin)==min(abs(bedtops_raw$y-plot5brush$ymin)))
        brushymin <- bedtops_raw[[bedindex,2]]
        
      }else{ #If the user doesn't want their selected interval to be modified, we wont...unless it falls outside the bounds of the section
        #If the top of the brush and/or the bottom of the brush are above/below of the  highest bed top and lowest bed top then make them the same as the top and bottom locations?
        #Reset the top
        if (plot5brush$ymax>max(bedtops_raw$y)){
          brushymax <- max(topbottom$df$y)
        } else {
          brushymax <- plot5brush$ymax
        }
        
        #Reset the base
        if (plot5brush$ymin<min(bedtops_raw$y)){
          brushymin <- min(topbottompts$y)
        } else {
          brushymin <- plot5brush$ymin
        }
      }

      sedstrat_raw$df <- bind_rows(sedstrat_raw$df,tibble(ymin_raw=brushymax,ymax_raw=brushymin, SedStruct=input$sedstruct_input, pt=nrow(sedstrat_raw$df)-1))
    }
  })
  
  #Creating sedimentary structure name
  observeEvent(input$create_sedstruct_name, {
    sedstructnames$df <- bind_rows(sedstructnames$df,tibble(name=input$sedstruct_text))
  })
  
  #Output radio UI
  output$sedstructchoices <- renderUI({
    radioButtons("sedstruct_input", "Sedimentary Structures",choices = sedstructnames$df$name)
  })
  
  #Delete sedimentary structure name from sedimentary structure list
  observeEvent(input$delete_sedstruct_name, {
    if(nrow(sedstructnames$df)>1){
      sedstructnames$df <- sedstructnames$df[-nrow(sedstructnames$df),]
    }
  })
  
  #Table for logged sedimentary structure data
  output$sedstrattable <- DT::renderDT(
    datatable(filter(sedstrat_raw$df, ymin_raw>0), rownames=FALSE, filter=c('none'))
  )
  
  #Delete last logged sed structure interval
  observeEvent(input$delete_pt_sed, {
    if(nrow(sedstrat_raw$df)>1){
      sedstrat_raw$df <- sedstrat_raw$df[-nrow(sedstrat_raw$df),]
    }
  })
  
  #function for deleting the rows selected in the sed strat table
  observeEvent(input$delete_row_sed, {
    if(!is.null(input$sedstrattable_rows_selected)){
      delete_row <- input$sedstrattable_rows_selected + 1
      sedstrat_raw$df <- sedstrat_raw$df[-(delete_row),]
      sedstrat_raw$df <- sedstrat_raw$df %>% mutate(pt=row_number()-1)
    }
  })
  
  # ------------------------------------------------------------------- Server code related to the page "4. Digitize lithofacies intervals"
  #Standard Plot Stuff
  ranges2_fac <- reactiveValues(x = NULL, y = NULL)
  
  output$plot6 <- renderPlot({
    if(is.null(input$files)){} # if there's no file input don't plot anything
    else{ # if there is a file input then plot the interactive ggplot
      ggplot(null$df,aes(x=nullx,y=nully)) +
        annotation_raster(LoadedImage(),ymin=0,ymax =height(LoadedImage()), xmin=0, xmax=width(LoadedImage())) +
        BlankTheme+geom_point() +
        coord_fixed() +
        xlim(0, width(LoadedImage())) +
        ylim(0, height(LoadedImage()))
    }
  })
  
  output$plot7 <- renderPlot({
    if(is.null(input$files)){} # if there's no file input don't plot anything
    else{ # if there is a file input then plot the interactive ggplot
      ggplot(data=null$df, aes(x=nullx,y=nully)) +
        geom_point() +
        BlankTheme +
        annotation_raster(LoadedImage(),ymin=0,ymax =height(LoadedImage()), xmin=0, xmax=width(LoadedImage())) +
        coord_cartesian(xlim=ranges2_fac$x,ylim=ranges2_fac$y) +
        geom_hline(yintercept=bedtops_raw$df$y, linetype="dashed", color = "red", size=1) +
        geom_rect(data=facies_raw$df, mapping=aes(xmin=0, xmax=width(LoadedImage()), ymin=facies_raw$df$ymax_raw, ymax=facies_raw$df$ymin_raw), color="blue", inherit.aes = FALSE, alpha = 0.1) +
        geom_label(data=facies_raw$df, aes(x=(width(LoadedImage())/2), y=facies_raw$df$ymax_raw+((facies_raw$df$ymin_raw-facies_raw$df$ymax_raw)/2), label=ifelse(facies_raw$df$FacBlockNum>0, as.character(facies_raw$df$FacBlockNum),''),hjust=1.25)) #label the logged facies blocks
    }
  })
  
  # If plot 6 is brushed chnage the range of the zoom on the GG plot to the brush range (x,y); if not, reset the zoom.
  observe({
    plot6brush <- input$plot6_brush
    if (!is.null(plot6brush)) {
      ranges2_fac$x <- c(plot6brush$xmin, plot6brush$xmax)
      ranges2_fac$y <- c(plot6brush$ymin, plot6brush$ymax)
    } else {
      ranges2_fac$x <- null$df$nullx
      ranges2_fac$y <- null$df$nully
    }
  })
  
  #Creating facies
  observeEvent(input$create_fac_name, {
    faciesnames$df <- bind_rows(faciesnames$df,tibble(name=input$facies_text))
  })
  
  #Delete facies from facies list
  observeEvent(input$delete_fac_name, {
    if(nrow(faciesnames$df)>1){
      faciesnames$df <- faciesnames$df[-nrow(faciesnames$df),]
    }
  })
  
  #Output radio UI
  output$facieschoices <- renderUI({
    radioButtons("facies_input", "Facies",choices = faciesnames$df$name)
  })
  
  #Table for logged facies data
  output$factable <- DT::renderDT(
    datatable(filter(facies_raw$df, ymin_raw>0), rownames=FALSE, filter=c('none'))
  )
  
  #Log facies interval from brush when clicking the update facies button
  #Record brush on plot 7 as two y-coordinates and the facies name from radio buttons
  observeEvent(input$update_fac, {
    #if there is no brush on the plot then do nothing, otherwise
    if(is.null(input$plot7_brush)){
      print("No brush, no data")
    }else{
      #isolate the brush locations
      plot7brush <- input$plot7_brush
      topbottompts<- topbottom$df %>% filter(x>0)
      bedtops_raw <- bedtops_raw$df %>% filter(x>0)
      
      #if the user wants the facies boundaries to be clipped to bed boundaries then do that
      if(input$modifyfacies == TRUE){
        
        bedtops_raw <- bind_rows(topbottompts,bedtops_raw)
        
        #top of the facies selection      # right now the which() statement is grabbing the index of the closet bed?
        bedindex <-which(abs(bedtops_raw$y-plot7brush$ymax)==min(abs(bedtops_raw$y-plot7brush$ymax)))
        brushymax <- bedtops_raw[[bedindex,2]]
        
        #bottom of the facies selection   # right now the which() statement is grabbing the index of the closet bed?
        bedindex <- which(abs(bedtops_raw$y-plot7brush$ymin)==min(abs(bedtops_raw$y-plot7brush$ymin)))
        brushymin <- bedtops_raw[[bedindex,2]]
        
      }else{ #otherwise we won't modify them at all
        if(plot7brush$ymax[1]>(max(topbottompts$y))){
          brushymax <-(max(topbottompts$y))
        } else {
          brushymax <- plot7brush$ymax[1]
        }
        
        if(plot7brush$ymin[1]<(min(topbottompts$y))){
          brushymin <-(min(topbottompts$y))
        } else {
          brushymin <- plot7brush$ymin[1]
        }
      }
      #Now that we have the optimal position for our facies boundaries we'll bind them into the facies dataframe
      facies_raw$df <- bind_rows(facies_raw$df,tibble(ymin_raw=brushymax,ymax_raw=brushymin, FacName=as.character(input$facies_input), FacBlockNum=nrow(facies_raw$df)-1))
    }
  })
  
  #Delete last logged facies interval
  observeEvent(input$delete_pt_fac, {
    if(nrow(facies_raw$df)>1){
      facies_raw$df <- facies_raw$df[-nrow(facies_raw$df),]
    }
  })
  
  #function for deleting the rows selected in the facies table
  observeEvent(input$delete_row_fac, {
    if(!is.null(input$factable_rows_selected)){
      delete_row <- input$factable_rows_selected + 1
      facies_raw$df <- facies_raw$df[-(delete_row),]
      facies_raw$df <- facies_raw$df %>% mutate(FacBlockNum=row_number()-1)
    }
  })
  
  #-------------------------------------------------------------------Server code related to the page "5. Digitizing architectural elements"
  
  #Standard Plot Stuff
  ranges2_element <- reactiveValues(x = NULL, y = NULL)
  
  output$plot8 <- renderPlot({
    if(is.null(input$files)){} # if there's no file input don't plot anything
    else{ # if there is a file input then plot the interactive ggplot
      ggplot(null$df,aes(x=nullx,y=nully))+
        annotation_raster(LoadedImage(), ymin=0, ymax=height(LoadedImage()), xmin=0, xmax=width(LoadedImage())) +
        BlankTheme+geom_point() +
        coord_fixed() +
        xlim(0, width(LoadedImage())) +
        ylim(0, height(LoadedImage()))
    }
  })
  
  output$plot9 <- renderPlot({
    if(is.null(input$files)){} # if there's no file input don't plot anything
    else{ # if there is a file input then plot the interactive ggplot
      ggplot(data=null$df, aes(x=nullx,y=nully))+
        geom_point()  +
        annotation_raster(LoadedImage(), ymin=0, ymax=height(LoadedImage()), xmin=0, xmax=width(LoadedImage())) +
        coord_cartesian(xlim=ranges2_element$x,ylim=ranges2_element$y) +
        BlankTheme +
        geom_hline(yintercept=bedtops_raw$df$y, linetype="dashed", color = "red", size=1)+
        geom_rect(data=element_raw$df, mapping=aes(xmin=0, xmax=width(LoadedImage()), ymin=element_raw$df$ymax_raw, ymax=element_raw$df$ymin_raw), color="blue", inherit.aes = FALSE, alpha = 0.1) +
        geom_label(data=element_raw$df, aes(x=(width(LoadedImage())/2), y=element_raw$df$ymax_raw+((element_raw$df$ymin_raw-element_raw$df$ymax_raw)/2), label=ifelse(element_raw$df$ElementBlockNum>0, as.character(element_raw$df$ElementBlockNum),''),hjust=1.25)) #label the logged element blocks
    }
  })
  
  # If plot 8 is brushed chnage the range of the zoom on the GG plot to the brush range (x,y); if not, reset the zoom.
  observe({
    plot8brush <- input$plot8_brush
    if (!is.null(plot8brush)) {
      ranges2_element$x <- c(plot8brush$xmin, plot8brush$xmax)
      ranges2_element$y <- c(plot8brush$ymin, plot8brush$ymax)
      
    } else {
      ranges2_element$x <- null$df$nullx
      ranges2_element$y <- null$df$nully
    }
  })
  
  #Create element name for list
  observeEvent(input$create_element_name, {
    elementnames$df <- bind_rows(elementnames$df,tibble(name=input$element_text))
  })
  
  #Delete element name from list
  observeEvent(input$delete_element_name, {
    if(nrow(elementnames$df)>1){
      elementnames$df <- elementnames$df[-nrow(elementnames$df),]
    }
  })
  
  #Output radio UI
  output$elementchoices <- renderUI({
    radioButtons("element_input", "Elements",choices = elementnames$df$name)
  })
  
  #Table for logged element data
  output$elementtable <- DT::renderDT(
    datatable(filter(element_raw$df, ymin_raw>0), rownames=FALSE, filter=c('none'))
  )
  
  #log Element interval from brush when clicking the update Element button
  #Record brush on plot 7 as two y-coordinates and the Element name from radio buttons
  observeEvent(input$update_element, {
    
    if(!is.null(input$plot9_brush)){
      plot9brush <- input$plot9_brush
      topbottompts<- topbottom$df %>% filter(x>0)
      bedtops_raw <- bedtops_raw$df %>% filter(x>0)
      
      #if the user wants the Element boundaries to be clipped to bed boundaries then we'll do that
      if(input$modifyelements == TRUE){
        bedtops_raw <- bind_rows(topbottompts,bedtops_raw)
        
        #Top of the Element selection
        bedindex <-which(abs(bedtops_raw$y-plot9brush$ymax)==min(abs(bedtops_raw$y-plot9brush$ymax)))
        brushymax <- bedtops_raw[[bedindex,2]]
        
        #Bottom of the Element selection
        bedindex <- which(abs(bedtops_raw$y-plot9brush$ymin)==min(abs(bedtops_raw$y-plot9brush$ymin)))
        brushymin <- bedtops_raw[[bedindex,2]]
        
        #If the user doesn't want their selected interval to be modified, we wont...unless it falls outside the bounds of the section
      }else{
        #If the top of the brush and/or the bottom of the brush are above/below of the  highest bed top and lowest bed top then make them the same as the top and bottom locations?
        #Reset the top
        if (plot9brush$ymax>max(bedtops_raw$y)){
          brushymax <- max(topbottom$df$y)
        } else {
          brushymax <- plot9brush$ymax
        }
        
        #Reset the base
        if (plot9brush$ymin<min(bedtops_raw$y)){
          brushymin <- min(topbottompts$y)
        } else {
          brushymin <- plot9brush$ymin
        }
      }
      #Now that we have the optimal place for th element boundary we need to bind it into the new data
      element_raw$df <- bind_rows(element_raw$df,tibble(ymin_raw=brushymax,ymax_raw=brushymin, ElementName=as.character(input$element_input), ElementBlockNum=nrow(element_raw$df)-1))
    }
  })
  
  #Delete last logged element interval
  observeEvent(input$delete_pt_element, {
    if(nrow(element_raw$df)>1){
      element_raw$df <- element_raw$df[-nrow(element_raw$df),]
    }
  })
  
  #function for deleting the rows selected in the facies table
  observeEvent(input$delete_row_element, {
    if(!is.null(input$elementtable_rows_selected)){
      delete_row <- input$elementtable_rows_selected + 1
      element_raw$df <- element_raw$df[-(delete_row),]
      element_raw$df <- element_raw$df %>% mutate(ElementBlockNum=row_number()-1)
    }
  })
  
  #-------------------------------------------------------------------Server code related to the page "Digitize architectural element sets or stratigraphic intervals"
  
  #Standard Plot Stuff
  ranges2_elementset <- reactiveValues(x = NULL, y = NULL)
  
  output$plot10 <- renderPlot({
    if(is.null(input$files)){} # if there's no file input don't plot anything
    else{ # if there is a file input then plot the interactive ggplot
      ggplot(null$df,aes(x=nullx,y=nully)) +
        geom_point() +
        BlankTheme +
        coord_fixed() +
        annotation_raster(LoadedImage() ,ymin=0, ymax=height(LoadedImage()), xmin=0, xmax=width(LoadedImage()))+
        xlim(0, width(LoadedImage())) +
        ylim(0, height(LoadedImage()))
    }
  })
  
  output$plot11 <- renderPlot({
    if(is.null(input$files)){} # if there's no file input don't plot anything
    else{ # if there is a file input then plot the interactive ggplot
      ggplot(data=null$df, aes(x=nullx,y=nully))+
        geom_point() +
        BlankTheme +
        annotation_raster(LoadedImage(),ymin=0, ymax=height(LoadedImage()), xmin=0, xmax=width(LoadedImage())) +
        coord_cartesian(xlim=ranges2_elementset$x,ylim=ranges2_elementset$y) +
        geom_hline(yintercept=bedtops_raw$df$y, linetype="dashed", color = "red", size=1)+
        geom_rect(data=elementset_raw$df, mapping=aes(xmin=0, xmax=width(LoadedImage()), ymin=elementset_raw$df$ymax_raw, ymax=elementset_raw$df$ymin_raw), color="blue", inherit.aes = FALSE, alpha = 0.1) +
        geom_label(data=elementset_raw$df, aes(x=(width(LoadedImage())/2), y=elementset_raw$df$ymax_raw+((elementset_raw$df$ymin_raw-elementset_raw$df$ymax_raw)/2), label=ifelse(elementset_raw$df$ElementSetBlockNum>0, as.character(elementset_raw$df$ElementSetBlockNum),''),hjust=1.25)) #label the logged elementset blocks
    }
  })
  
  # If plot 10 is brushed chnage the range of the zoom on the GG plot to the brush range (x,y); if not, reset the zoom.
  observe({
    plot10brush <- input$plot10_brush
    if (!is.null(plot10brush)) {
      ranges2_elementset$x <- c(plot10brush$xmin, plot10brush$xmax)
      ranges2_elementset$y <- c(plot10brush$ymin, plot10brush$ymax)
      
    } else {
      ranges2_elementset$x <- null$df$nullx
      ranges2_elementset$y <- null$df$nully
    }
  })
  
  #Creating element set name
  observeEvent(input$create_element_set_name, {
    elementsetnames$df <- bind_rows(elementsetnames$df,tibble(name=input$element_set_text))
  })
  
  #Delete element sets name from facies list
  observeEvent(input$delete_elementset_name, {
    if(nrow(elementsetnames$df)>1){
      elementsetnames$df <- elementsetnames$df[-nrow(elementsetnames$df),]
    }
  })
  
  #Output radio UI
  output$elementsetchoices <- renderUI({
    radioButtons("elementset_input", "Element Sets",choices = elementsetnames$df$name)
  })
  
  #Table for logged element set data
  output$elementsettable <- DT::renderDT(
    datatable(filter(elementset_raw$df, ymin_raw>0), rownames=FALSE, filter=c('none'))
  )
  
  #log element set interval from brush when clicking the "Update Element Set" button
  #Record brush on plot 11 as two y-coordinates and the element set name from radio buttons
  observeEvent(input$update_elementset, {
    
    if(!is.null(input$plot11_brush)){
      plot11brush <- input$plot11_brush
      topbottompts<- topbottom$df %>% filter(x>0)
      bedtops_raw <- bedtops_raw$df %>% filter(x>0)
      
      #if the user wants the element set boundaries to be clipped to bed boundaries then do that
      if(input$modifyelementsets == TRUE){
        
        bedtops_raw <- bind_rows(topbottompts,bedtops_raw)
        
        #top of the element set selection      # right now the which() statement is grabbing the index of the closet bed?
        bedindex <-which(abs(bedtops_raw$y-plot11brush$ymax)==min(abs(bedtops_raw$y-plot11brush$ymax)))
        brushymax <- bedtops_raw[[bedindex,2]]
        
        #bottom of the element set selection   # right now the which() statement is grabbing the index of the closet bed?
        bedindex <- which(abs(bedtops_raw$y-plot11brush$ymin)==min(abs(bedtops_raw$y-plot11brush$ymin)))
        brushymin <- bedtops_raw[[bedindex,2]]
        
        #otherwise we won't modify them, except if they fall outside of the top and bottom of the section
      } else {
        
        if(plot11brush$ymax[1]>(max(topbottompts$y))){
          brushymax <-(max(topbottompts$y))
        } else {
          brushymax <- plot11brush$ymax[1]
        }
        
        if(plot11brush$ymin[1]<(min(topbottompts$y))){
          brushymin <-(min(topbottompts$y))
        } else {
          brushymin <- plot11brush$ymin[1]
        }
      }
      #Now we we have the best boundaires of our element set/interval figured out we can bind them into the existing element set dataframe
      elementset_raw$df <- bind_rows(elementset_raw$df,tibble(ymin_raw=brushymax,ymax_raw=brushymin, ElementSetName=as.character(input$elementset_input), ElementSetBlockNum=nrow(elementset_raw$df)-1))
    }
  })
  
  #Delete last logged element set interval
  observeEvent(input$delete_element_set, {
    if(nrow(elementset_raw$df)>1){
      elementset_raw$df <- elementset_raw$df[-nrow(elementset_raw$df),]
    }
  })
  
  #function for deleting the rows selected in the elementset table
  observeEvent(input$delete_row_elementset, {
    if(!is.null(input$elementsettable_rows_selected)){
      delete_row <- input$elementsettable_rows_selected + 1
      elementset_raw$df <- elementset_raw$df[-(delete_row),]
      elementset_raw$df <- elementset_raw$df %>% mutate(ElementSetBlockNum=row_number()-1)
    }
  })
  
  ############################################################################################################
  # Code related to processing the collected data points
  ############################################################################################################
  
  observeEvent(input$process_pts, {
    ####There is a value of -1 that is initialized in the dataframe (you can see this at the top of the server code), filter them out and sort based on y-value. Lastly, all reactive dataframe are convereted to non-reactive dataframe
    topbottom <- topbottom$df %>% filter(x>0) %>% arrange(y)
    gsmarkers <- gsmarkers$df %>% filter(x>0)
    df.bedtops <- bedtops_raw$df %>% filter(x>0) %>% arrange(y)
    df.GSpts <- values$df %>% filter(x>0)%>% arrange(y)
    df.sedstrat <- sedstrat_raw$df %>% filter(ymin_raw>0) %>% arrange(ymin_raw)
    df.facies <- facies_raw$df %>% filter(ymin_raw>0)%>% arrange(ymin_raw)
    df.element <- element_raw$df %>% filter(ymin_raw>0)%>% arrange(ymin_raw)
    df.elementset <- elementset_raw$df %>% filter(ymin_raw>0)%>% arrange(ymin_raw)
    StratInc <- as.numeric(isolate(input$StratInc))
    
    ####Compute the grain size function for mutation - use the x position of the grain size divisions to create a function, so the x-position of the grain size profile points can be transformed into sediment diameter
    GScheckGroupIndex <- as.numeric(input$GS_checkGroup)
    #Isolate the input values for the gain size divisions and form into a vector
    diameter <- c(isolate(input$MudSize),isolate(input$SiltSize),isolate(input$VeryFineSize),isolate(input$FineSize),isolate(input$MediumSize),isolate(input$CoarseSize),isolate(input$VeryCoarseSize),isolate(input$GranuleSize),isolate(input$PebbleSize),isolate(input$CobbleSize),isolate(input$BoulderSize)) # make a vector out of all the grain sizes in the process page
    #Use the check group numbers to select/subset the appropriate values from the diameter vector (above)
    diameter <- as.numeric(diameter[GScheckGroupIndex])
    #Bind the diameter vector to the picked grain size division dataframe
    df.GS1 <- cbind(gsmarkers,diameter)
    
    #Compute the grain size that is used to turn x coordinates of grain size points (df.GSpts) in to numeric grain size. "rule=2" allow for points picked outside that fall outside of the function to return the closest value.
    fun.GS1 <-approxfun(df.GS1$x, df.GS1$diameter, method="linear", rule=2)
    
    ####Compute the thickness function for mutation - use the y position of the top-bottom points to create a function, so the y-position of the grain size profile points (as well as the sedimentary structures, facies, etc.) can be converted to thickness.
    #Get values for the top and bottom of the core (sometimes in thickness, sometimes in depth) and join them to the picked points for top and bottom in df.Thick
    thicknesses  <-  c(isolate(input$sectthick_base),isolate(input$sectthick_top))
    df.Thick <- bind_cols(topbottom, tibble(sectthick=thicknesses))
    
    #Make the thickness function - because approxfun works for functions increasing values of x and mulitple values of y (not increasing in y and multiple values of x), we put y/thickness in as the x value.
    fun.Thick <- approxfun(df.Thick$y, df.Thick$sectthick, method="linear")
    
    #Take the grain size profile points and mutate them with the fucntions (fun.GS1 and fun.Thick) to get the thickness and grain size of each point
    df.GSpts <- df.GSpts %>% mutate(GS = fun.GS1(df.GSpts$x), Thick = (fun.Thick(df.GSpts$y)))
    
    #Create a sequence for the thickness of the strat section that spans from the thickness a the bottom to the thickness at the top, by the increment set by the user.
    NewY <- seq(from = isolate(input$sectthick_base), to = isolate(input$sectthick_top), by = StratInc)
    NewY <- NewY[-1] # the first entry into NewY is removed to start the section 
    SectData <- data.frame(Thickness = NewY)
    
    #Mutate in the bedtops using the thickness fucntion (fun.Thick), then round them to the nearest StratInc. Set the bedtops to a new dataframe, "MatchBedTops", so they can be joined to the section data
    MatchBedTops <- df.bedtops %>%  mutate(Thick = (fun.Thick(df.bedtops$y)))
    MatchBedTops <- MatchBedTops %>% mutate(Thickness = round(MatchBedTops$Thick/StratInc)*StratInc) %>%
      select(Thickness,BedTop) %>%
      arrange(Thickness)
    
    #Join in the Bed Top locations in to the SectData by Thickness (the rounded thickness)
    SectData <- left_join(SectData, MatchBedTops, by = c("Thickness"))
    SectData$BedTop <-  ifelse(is.na(SectData$BedTop), 0, 1)
    
    #Compute the grain size diameter of each point of the sequence. For that, we need a new grain size function. Previously Grain size was a function of x-position on the image, based on the grain size division. Now we need on that is based on the y-position within the grain size profile.
    #Round the grain size points to the nearest thickness/descritized sequence
    df.GSpts <- df.GSpts %>% mutate(Thick = round(df.GSpts$Thick/StratInc)*StratInc)
    
    #The picked grain size profile points all have grain sizes after the first function (fun.GS1). We can now make a function out of those points (fun.GS2) and run the entire new sequence (SeqY/SectData$Thickness) through it to yield the interpolated grain size at any point. Again we need to flip the x and y axis to do this.
    fun.GS2 <-approxfun(df.GSpts$Thick, df.GSpts$GS, method="linear")
    GSInterp <- fun.GS2(NewY)
    
    #Make a dataframe out of the sequence, and the grain size and join it to the section data
    df.MatchGS <- data.frame(NewY,GSInterp)
    df.MatchGS <- df.MatchGS %>% rename(Thickness=NewY, GS=GSInterp)
    SectData <- full_join(SectData, df.MatchGS, by = c("Thickness"))
    
    #Fill in GS data at the start of the log and at the end of the log
    #Beacuse the sediment diameter is only interpolated between the pick points of the function, we need to add grain size data to between the start of the section and the first picked grain size profile point, as well as between the end of the section last picked grain size point
    if(is.na(SectData[1,3])){
      NonNAindex <- which(!is.na(SectData$GS))
      firstNonNA <- min(NonNAindex)
      SectData[1:(firstNonNA-1),3]=SectData[firstNonNA,3]
    }
    
    if(is.na(SectData[nrow(SectData),3])){
      NonNAindex <- which(!is.na(SectData$GS))
      lastNonNA <- max(NonNAindex)
      SectData[(lastNonNA+1):nrow(SectData),3]=SectData[lastNonNA,3]
    }
    
    #Modify the the section data based on the number of input GS points between the bed tops
    for (i in 1:(nrow(MatchBedTops)-1)){
      minbedtop <- as.numeric(MatchBedTops[i,1])
      maxbedtop <- as.numeric(MatchBedTops[i+1,1])
      
      gspts <-  sum((df.GSpts$Thick > minbedtop) & (df.GSpts$Thick <= maxbedtop)) # count the number of digitized grain size points are inbetween those two bedtops
      
      if(gspts==0){ #if we have no points inbetween two bed tops delete the grain size data and call it cover. Separate if's works better than one long if/else if
        SectData$GS <- ifelse(SectData$Thickness > minbedtop & SectData$Thickness <= maxbedtop, NA, SectData$GS)
      }
      
      if(gspts==1){ #if we have only 1 point inbetween two bed tops find the original mutated GS and use that for the entire bed.=
        gsindex <-  which(df.GSpts$Thick > minbedtop & df.GSpts$Thick <= maxbedtop)
        SectData$GS <-  ifelse(SectData$Thickness > minbedtop & SectData$Thickness<= maxbedtop, df.GSpts[gsindex,4],SectData$GS)
      }
      
      if(gspts >= 2){ # if we have 2 or more grain size profile points between the bed tops, filter the original GS points to get all the GS control points that are betwen two bed tops in question
        GS_filter <- df.GSpts %>% filter(between(Thick,minbedtop,maxbedtop))
        Min_GS_Thick <- as.numeric(GS_filter[1,5])
        Min_GS_GS <- as.numeric(GS_filter[1,4])
        Max_GS_Thick <- as.numeric(GS_filter[nrow(GS_filter),5])
        Max_GS_GS <- as.numeric(GS_filter[nrow(GS_filter),4])
        
        SectData$GS <- ifelse(SectData$Thickness <= Min_GS_Thick & SectData$Thickness > minbedtop, Min_GS_GS, SectData$GS)
        SectData$GS <- ifelse(SectData$Thickness > Max_GS_Thick & SectData$Thickness<= maxbedtop, Max_GS_GS, SectData$GS)
      }
    }
    SectData <- transform(SectData, GS = as.numeric(GS))
    
    ####Append in the section name and location data from the processing page, regardless of the inputs
    SectData$SectName <- rep(input$section_name,nrow(SectData)) #Append Section Name
    SectData$SectLoc <- rep(input$section_loc,nrow(SectData)) #Append Section Location
    SectData$UTM_E <- rep(input$UTM_E,nrow(SectData)) #Append UTM E
    SectData$UTM_N <- rep(input$UTM_N,nrow(SectData)) #Append UTM N
    
    ####For the statistics to be computed on we are going to create a few different columns (bednumber, reservoir/nonreservoir, amalgmation of beds)
    SectData$BedNumber <- rep(NA,nrow(SectData))
    
    #Fill in bed numbers then enter loop to fill in the bednum column
    bednumber <- 1
    for (i in 1:nrow(SectData)){ #For loop through the section data and increase the bed number by 1 every time a bed top is found
      SectData[i,8] <- bednumber
      if(SectData[i,2]==1){
        bednumber <- bednumber+1
      }
    } #End bed number for loop
    
    #Reservoir indicator (0 = non-reservoir, 1 = reeservoir) - mutate in a column that represents the presence of reservoir
    SectData <- SectData %>% mutate(Reservoir = ifelse(SectData$GS < input$ResGS | is.na(SectData$GS), 0, 1)) #if the new GS is less than the reservoir cut-off or is NA then call it not mud (=0)
    print("reservoir/non-reservoir column added")
    
    #Add an amalgmation indicator at the bed boundaries (0 = no amalgamation, 1 = amalgamation)
    SectData$Amalg <- rep(0,nrow(SectData)) #Create column for the binary classification as to if the bed contact is amalgamated or not
    
    #Loop through the bed tops and see if there is sand above and below the bedding contact
    for (i in 1:(nrow(MatchBedTops))){
      beddingcontact_depth <- as.numeric(MatchBedTops[i,1])
      beddingcontact_index <- match(beddingcontact_depth, SectData$Thickness)
      
      if(SectData[beddingcontact_index,9]==1 && SectData[beddingcontact_index+1,9]==1){
        SectData[beddingcontact_index,10] <- 1
      } else {
        SectData[beddingcontact_index,10] <- 0
      }
    } #End amalgmation indicator for loop
    
    ####Process the sedimentary structure data
    #Mutate in the thicknesses for the top and bottom of the brushed/recorded intervals
    df.sedstrat <- df.sedstrat %>% mutate(ymin_thick = round((fun.Thick(df.sedstrat$ymax_raw))/StratInc)*StratInc, ymax_thick = round((fun.Thick(df.sedstrat$ymin_raw))/StratInc)*StratInc)
    
    if(nrow(df.sedstrat)>0){  #If df.sedstrat has any data in it, then process it
      #Create the new columns based on the user inputs
      sedstratcolumns <- unique(df.sedstrat$SedStruct)
      
      for (i in 1:length(sedstratcolumns)){
        colname <- sedstratcolumns[i]
        SectData[, colname] <- as.character(NA)
      }
      
      #Populate the columns with
      for (i in 1:nrow(df.sedstrat)){
        #Get nessicary values, the name of sedimentary structure, as well as the min-max bounds of the interval
        colname <- df.sedstrat[[i,3]]
        sedstratmin_y <- df.sedstrat[[i,4]]
        sedstratmax_y <- df.sedstrat[[i,5]]
        
        #Return the column index that that sedimentary structure belongs to
        colindex <- which(colnames(SectData) %in% colname)
        
        #Return the row numbers that fall within the min-max bounds of the interval and set those indexes to the column name
        rowindex <- which(SectData$Thickness >= sedstratmin_y & SectData$Thickness <= sedstratmax_y)
        SectData[rowindex,colindex] <- colname
      }
    }#End if(nrow(df.sedstrat)>0)
    
    ####Process the Facies data
    df.facies <- df.facies %>% mutate(ymin_thick = (round((fun.Thick(df.facies$ymax_raw))/StratInc)*StratInc), ymax_thick = round((fun.Thick(df.facies$ymin_raw))/StratInc)*StratInc)
    
    if(nrow(df.facies)>0){ #If the df.facies has any logged data inside it then processes it
      SectData$Facies <- rep(NA,nrow(SectData))
      SectData$FaciesBlockNum <- rep(NA,nrow(SectData))
      
      #Increment through the facies dataframe and if else each row. Try to get the ifelse to not replace the cells if it fails the test.
      for (i in 1:nrow(df.facies)){
        faciesmin_y <- as.numeric(df.facies[i,5]) ## Set the minimum and maximum y-location (from the brush) to variables 
        faciesmax_y <- as.numeric(df.facies[i,6])
        
        SectData$Facies <-  ifelse(SectData$Thickness>faciesmin_y & SectData$Thickness<=faciesmax_y, df.facies[i,3], SectData$Facies)
        SectData$FaciesBlockNum <- ifelse(SectData$Thickness>faciesmin_y & SectData$Thickness<=faciesmax_y, df.facies[i,4], SectData$FaciesBlockNum)
      } #End for loop
      
      SectData <- transform(SectData, GS = as.numeric(GS),
                            Facies = as.character(Facies),
                            FaciesBlockNum = as.numeric(FaciesBlockNum)
      )
    }#End if(nrow(df.facies)>0)
    
    ####Process the Element data
    df.element <- df.element %>% mutate(ymin_thick = (round((fun.Thick(df.element$ymax_raw))/StratInc)*StratInc), ymax_thick = round((fun.Thick(df.element$ymin_raw))/StratInc)*StratInc)
    
    if(nrow(df.element)>0){ #If df.element has any recorded intervals in it, then process it
      SectData$Element <- rep(NA,nrow(SectData))
      SectData$ElementBlockNum <- rep(NA,nrow(SectData))
      
      #Increment through the element dataframe and if else each row. Try to get the ifelse to not replace the cells if it fails the test.
      for (i in 1:nrow(df.element)){
        elementmin_y<- as.numeric(df.element[i,5]) ## Do as numeric ahead outside of the for loop
        elementmax_y <- as.numeric(df.element[i,6])
        
        SectData$Element <-  ifelse(SectData$Thickness>elementmin_y & SectData$Thickness<=elementmax_y, df.element[i,3], SectData$Element)
        SectData$ElementBlockNum <- ifelse(SectData$Thickness>elementmin_y & SectData$Thickness<=elementmax_y, df.element[i,4], SectData$ElementBlockNum)
      } #End for loop
      
      SectData <- transform(SectData, Element = as.character(Element),
                            ElementBlockNum = as.numeric(ElementBlockNum)
      )
    } #End if(nrow(element)>0)
    
    ####Process the Element Set data
    df.elementset <- df.elementset %>% mutate(ymin_thick = round((fun.Thick(df.elementset$ymax_raw))/StratInc)*StratInc, ymax_thick = round((fun.Thick(df.elementset$ymin_raw))/StratInc)*StratInc)
    
    if(nrow(df.elementset)>0){ #If df.elementset has any recorded intervals in it, then process it
      SectData$ElementSet <- rep(NA,nrow(SectData))
      SectData$ElementSetBlockNum <- rep(NA,nrow(SectData))
      
      #increment through the element set dataframe and if else each row. Try to get the ifelse to not replace the cells if it fails the test.
      for (i in 1:nrow(df.elementset)){
        elementsetmin_y <- as.numeric(df.elementset[i,5]) ## Do as numeric ahead outside of the for loop
        elementsetmax_y <- as.numeric(df.elementset[i,6])
        SectData$ElementSet <- ifelse(SectData$Thickness>elementsetmin_y & SectData$Thickness<=elementsetmax_y, df.elementset[i,3], SectData$ElementSet)
        SectData$ElementSetBlockNum <- ifelse(SectData$Thickness>elementsetmin_y & SectData$Thickness<=elementsetmax_y, df.elementset[i,4], SectData$ElementSetBlockNum)
      }
      
      SectData <- transform(SectData, ElementSet = as.character(ElementSet),
                            ElementSetBlockNum = as.numeric(ElementSetBlockNum)
      )
    } #End if(nrow(elementset)>0)
    
    ####Start bed/facies/element/elementset statistics
    ##In this portion if the check box is checked then statistics will be calculated for each portion of the heirarchy
    #Bed Statistics
    if(input$BedStats==TRUE){ #if bed-scales statistics are desired then do the following
      df_bedsummarize_striplog <- SectData %>%
        dplyr::group_by(BedNumber) %>% #group by bed number
        dplyr::summarise(Thickness = max(Thickness), BedThick=n()*StratInc, BedMeanGS = mean(GS, na.rm=TRUE), BedMaxGS=max(GS, na.rm=TRUE)) #great a summary table, the first column (thickenss) will be used to joint the data back to the top of each bed
      df_bedsummarize <- df_bedsummarize_striplog %>% select(-BedNumber) #we drop the bed number row of the summary table so we can just join by the "Thickness column"
      SectData <- left_join(SectData,df_bedsummarize,by = "Thickness") #Join back
    }
    
    #Facies stats
    if(input$FaciesStats==TRUE & nrow(df.facies)>0){ #if facies-scale statistics are desired and there is facies data provided (i.e., "nrow(df.facies)>0") the do the following
      df_facsummarize_striplog <- SectData %>%
        dplyr::group_by(FaciesBlockNum) %>% #group by facies block number
        dplyr::summarise(Thickness = max(Thickness), FaciesThick=n()*(StratInc), FaciesMeanGS = mean(GS, na.rm=TRUE), FaciesNetGross=sum(Reservoir)/n()) #calculate a variety of statistics, and a thickness row like above
      df_facsummarize <- df_facsummarize_striplog %>% select(-FaciesBlockNum) #drop facies block number so just join by Thickness
      SectData <- left_join(SectData,df_facsummarize,by = "Thickness") #Join back
    }
    
    #Element statistics
    if(input$ElementStats==TRUE & nrow(df.element)>0){ #Same as above
      df_elementsummarize_striplog <- SectData %>%
        dplyr::group_by(ElementBlockNum) %>%
        dplyr::summarise(Thickness = max(Thickness), ElementThick=n()*(StratInc), ElementMeanGS = mean(GS, na.rm=TRUE), ElementNetGross=sum(Reservoir)/n())
      df_elementsummarize <- df_elementsummarize_striplog %>% select(-ElementBlockNum)
      SectData <- left_join(SectData,df_elementsummarize,by = "Thickness")
    }
    
    #Element Set statistics
    if(input$ElementSetStats==TRUE & nrow(df.elementset)>0){ #Same as above
      df_elementsetsummarize_striplog <- SectData %>%
        dplyr::group_by(ElementSetBlockNum) %>%
        dplyr::summarise(Thickness = max(Thickness), ElementSetThick=n()*(StratInc), ElementSetMeanGS = mean(GS, na.rm=TRUE), ElementSetNetGross=sum(Reservoir)/n())
      df_elementsetsummarize <- df_elementsetsummarize_striplog %>% select(-ElementSetBlockNum)
      SectData <- left_join(SectData,df_elementsetsummarize,by = "Thickness")
    }
    
    #Bind the SectData back into a reactive dataframe so it can be downloaded and drop the first row of it
    SectDataProcessed$df <-  bind_rows(SectDataProcessed$df,SectData)
    SectDataProcessed$df <-  SectDataProcessed$df[-1,]
    
    ####Create reactive dataframe that is in StripLog Format
    ##In this section we are going to first take the SectData and pull out some statistics on each bed. Then a for loop will be used to go through each bed and grab other information before the two dataframes are joined together
    #write.csv(SectData, file="~/SectDataPreStats.csv")
    striplog <- SectData %>% 
      #filter(!is.na(BedNumber)) %>% #remove any NA bed numbers (i.e., cover/missing core) then calculate some key variables 
      group_by(BedNumber) %>% 
      summarize(name = input$section_name, 
                collection = input$section_loc,
                base = min(Thickness),
                tops = max(Thickness),
                th = (max(Thickness)-min(Thickness)+input$StratInc),
                gs_tops = last(GS),
                sand_shl = max(Reservoir),
                mean_gs = mean(GS),
                max_gs = max(GS))
    
    #Here we are going to take the data from SectData and extract which beds correspond to which FaciesBlockNum, ElementBlockNum, and ElementsetBlockNum 
    striplog_blocknums <- SectData %>% 
      group_by(BedNumber) %>% 
      slice(which.max(Thickness)) %>% #extract the top row of each bed number
      select_if(names(.) %in% c("Facies","FaciesBlockNum","Element","ElementBlockNum","ElementSet","ElementSetBlockNum")) %>% #because the statistics might not have been run a variable extractor are used
      ungroup()
    
    striplog <- left_join(striplog, striplog_blocknums, by = "BedNumber") #put the first two parts of the strip log together
    
    #Join the statistics "...summarize_striplog" dataframes if they have been calculated
    if(input$FaciesStats==TRUE & nrow(df.facies)>0){
      striplog <- left_join(striplog, df_facsummarize_striplog, by="FaciesBlockNum")
    }
    
    if(input$ElementStats==TRUE & nrow(df.element)>0){
      striplog <- left_join(striplog, df_elementsummarize_striplog, by="ElementBlockNum")  
    }
    
    if(input$ElementSetStats==TRUE & nrow(df.elementset)>0){
      striplog <- left_join(striplog, df_elementsetsummarize_striplog, by="ElementSetBlockNum")
    }
    
    striplog <- striplog %>% select(-starts_with("Thickness")) # because there are Thickness columns each of the "...summarize_striplog" dataframes we can drop anything with a prefix of Thickness 
    
    ##In this section concatonated strings of input grain size locations are created
    #Expand the MatchBedTops dataframe from above to include the top and bottom of the section.
    MatchBedTops <- rbind(c(input$sectthick_base,1),MatchBedTops) #add the base of section thickness to the top of MatchBedTops dataframe
    MatchBedTops <- rbind(MatchBedTops,c(input$sectthick_top,1)) #add the top of section thickness to the last row of MatchBedTops dataframe
    
    #A dataframe is create that will have the concatinated strings in side it. This will be populated in the loop below. 
    df.GS_join <- data.frame(x=c(1),grain_size_dia=c("1"),grain_size_thick=c("1"),BedBase=c(1), BedTop=c(1)) #create new dataframe for the for loop that is coming up next to add onto
    for(x in 1:(length(MatchBedTops$Thickness)-1)) { # loop through each bed and create two strings for each bed. One that has the grain size values that were clicked on the image in the grain size profile, and one that has the thickness measurements that relate to those clicked grain size values. 
      BedBase_filter <- MatchBedTops[x,1] #thickness of section to the the base of the bed in question
      BedTop_filter <- MatchBedTops[x+1,1] #thickness of section to the top of the bed (the base of the next bed)
      df.GSpts_filter <- df.GSpts %>% filter(Thick>BedBase_filter$Thickness[1] & Thick<BedTop_filter$Thickness[1]) #filter all the grain size points to find the grain size points that are within the bed bounds
      VecLength <- nrow(df.GSpts_filter) #how many points are there

      if(nrow(df.GSpts_filter)==0){ #if there are not points within the bed boundaries (it would have been considered cover), give it a single NA value in both strings
        grain_size_dia <- "NA"
        grain_size_thick <- "NA"
      } else { #if there are points then we need to collapse all the picked points into a string
        grain_size_dia <- paste0(df.GSpts_filter$GS, collapse=",")
        grain_size_thick <- paste0(df.GSpts_filter$Thick, collapse=",")
      } 
      
      df.GS_join <- rbind(df.GS_join,data.frame(x,grain_size_dia,grain_size_thick, BedBase=BedBase_filter$Thickness[1], BedTop=BedTop_filter$Thickness[1])) #Join the strings from the if/else statement into the dataframe
    }
    
    df.GS_join <- df.GS_join[-1,] #drop the first row of the dataframe from above. the first row was a place holder
    df.GS_join$grain_size_dia <- as.character(df.GS_join$grain_size_dia) #convert some these back to strings rather than factors...
    df.GS_join$grain_size_thick <- as.character(df.GS_join$grain_size_thick) #convert some these back to strings rather than factors...
    df.GS_join <- df.GS_join %>% select(x,grain_size_dia,grain_size_thick) %>% rename(BedNumber=x) #rename column "x" to be used as the join in the column below
    
    striplog_join <- left_join(striplog,df.GS_join) #join this data with the original summarize data
    
    str(striplog_join)
    
    StripLogData$df <-  bind_rows(StripLogData$df,striplog_join)
    StripLogData$df <-  StripLogData$df[-1,] #Drop first row from the reactive dataframe that was used as a place holder
    
    #Send notifications
    showNotification("Process Complete - Input stratigraphic data has been digitized", type = "message")
    showNotification("Data is ready to be downloaded", type = "message")
    
    #write.csv(df.GSpts, file="~/dfGSpts.csv", row.names=FALSE)
    #write.csv(MatchBedTops, file="~/MatchBedTops.csv", row.names=FALSE)
    
  }) #End of Process Points
  
  #Download SectDataProcessed$df when downlaod button is clicked
  output$downloadSectData <- downloadHandler(
    filename = function() {
      paste("ProcessedStratgraphicData", Sys.Date(), ".csv", sep="")
    },
    content = function(con) {
      write.csv(SectDataProcessed$df, con, na = "NA", row.names=FALSE, sep=";")
    }
  )
  
  #Download StripLogData$df when downlaod button is clicked
  output$downloadStripLog <- downloadHandler(
    filename = function() {
      paste("GraphicLog", Sys.Date(), ".csv", sep="")
    },
    content = function(con) {
      write.csv(StripLogData$df, con, na = "NA", row.names=FALSE)
    }
  )
  
  ##############################################################################################
  #Server code related to integration of discrete measurements to discretized stratigraphic data
  ##############################################################################################
  #Rendering the table for the .csv file of discrete measurements
  output$MeasurementContents <- renderTable({
    inMeasurementFile <- input$measurementfile
    
    if (is.null(inMeasurementFile))
      return(NULL)
    
    read.csv(inMeasurementFile$datapath, header = input$header)
  }) # end of MeasurementContents
  
  #Joining the data based on rounded coordinates of the measurements
  observeEvent(input$ImportMeasurments_Discrete, {
    
    #Import the files
    inMeasurementFile <- input$measurementfile
    MeasurementData <- read.csv(inMeasurementFile$datapath, header=input$header, sep = ",", stringsAsFactors=FALSE)
    
    inSectDataFile <- input$sectfile
    SectData <-   read.csv(inSectDataFile$datapath, header=input$header, sep = ",", stringsAsFactors=FALSE)
    
    ThickCol <- input$ThickColumn
    
    #Sometimes theres a weird X column when expored out of R. If its there, get rid of it.
    ColNames <- colnames(SectData)
    if(ColNames[1] == "X"){
      SectData <- SectData[,-1]
    }
    
    #Rename the thick column to thickness incase its something else
    colnames(MeasurementData)[input$ThickColumn] <- "Thickness"
    
    #Round the measurment data to the stratigraphic interval is of the input and mutate it onto the data
    SectDataInc <-  SectData[3,ThickCol] - SectData[2,ThickCol]
    MeasurementData <- MeasurementData %>% mutate(Thickness = round((.[[ThickCol]])/SectDataInc)*SectDataInc)
    
    #Join the raw measurement data into the SectData - due to floating point numbers in some of the mutations, the thickness columns are changed to characters/strings prior to joining. SectData$Thickness is then returned to a numeric type after joining. Without this some values are dropped due to an non-identical match in the join.
    MeasurementData$Thickness <- as.character(Continuous.df$Thickness)
    SectData$Thickness <- as.character(SectData$Thickness)
    
    SectData <- left_join(SectData,MeasurementData, by = c("Thickness"))
    SectData$Thickness <- as.numeric(SectData$Thickness)
    
    #Return the column names of the MeasurementData that aren't thickness so we can give the summarized data a proper name
    ColNames <- colnames(MeasurementData)
    ColNames <- ColNames[which(ColNames!="Thickness")]
    
    #If bed is selected group by BedNumber then filter on !is.Na to then summarize different statistics on each of those groups: df_bedsummarize <- dplyr::group_by(SectData, BedNumber) %>% dplyr::summarise(Thickness = max(Thickness), BedThick=n()*0.1, MeanGS = mean(GS), MaxGS=max(GS))
    if(input$Discrete_BedStat == TRUE){
      
      #Get the names of the columns that we'll need then remove them from the SectData
      BedStatCol <- c(ColNames,"BedNumber")
      BedStatJoin <- c("BedNumber","Thickness")
      BedSummarize  <-  SectData[,BedStatCol] # should use dplyr::select for this eventually
      BedStatJoin <- SectData[,BedStatJoin] # should use dplyr::select for this eventually
      
      #Group by BedNum Summarize_all (min, max, average, stdev)
      BedSummarizeStat <- BedSummarize %>%
        group_by(BedNumber) %>% # we want to run the following statistics at the bed level
        summarise_all(.funs = c(min="min", max="max", mean="mean", sd="sd"),na.rm = TRUE) # we are going to find the minimum, maximum, mean, and standar deviation of each descrete measurement type, NAs will be ignored
      BedSummarizeStat[sapply(BedSummarizeStat, is.infinite)] <- NA # because some beds have no measurements they will have a summarized value of infinite (INF), so we will change the INF for NA
      
      #Add a prefix (BedStat.) to each column, then fix the BedNumber column
      BedSummarizeStat <- BedSummarizeStat %>%
        setNames(paste0('BedStat.', names(.))) %>%
        rename(BedNumber=BedStat.BedNumber)
      
      #Make the BedStatJoin dataframe that we will join with the summarized data (i.e., BedSummarizeStat) then join to the section data
      BedStatJoin <- BedStatJoin %>%
        group_by(BedNumber) %>%
        summarise(Thickness = max(Thickness))
      
      #Join back to the sect data, first by joining the two summarized data together, then the summarized data to the SectData
      BedStatJoin <- left_join(BedStatJoin,BedSummarizeStat,by="BedNumber") %>% select(-BedNumber)
      SectData <- left_join(SectData,BedStatJoin, by="Thickness")
    } #End of bed statistics
    
    #If summarize by Facies is selected, group by FaciesBlockNum
    if(input$Discrete_FaciesStat == TRUE & "FaciesBlockNum" %in% colnames(SectData)){
      
      #Get the names of the columns that we'll need then remove them from the SectData
      FaciesStatCol <- c(ColNames,"FaciesBlockNum")
      FaciesStatJoin <- c("FaciesBlockNum","Thickness")
      FaciesSummarize  <-  SectData[,FaciesStatCol] # should use dplyr::select for this eventually
      FaciesStatJoin <- SectData[,FaciesStatJoin] # should use dplyr::select for this eventually
      
      #Group by FaciesNum Summarize_all (min, max, average, stdev)
      FaciesSummarizeStat <- FaciesSummarize %>%
        group_by(FaciesBlockNum) %>%
        summarise_all(.funs = c(min="min", max="max", mean="mean", sd="sd"),na.rm = TRUE)
      FaciesSummarizeStat[sapply(FaciesSummarizeStat, is.infinite)] <- NA # because some facies have no measurements they will have a summarized value of infinite (INF), so we will change the INF for NA
      
      #Add a prefix (FaciesStat.) to each column, then fix the FaciesBlockNum column
      FaciesSummarizeStat <- FaciesSummarizeStat %>%
        setNames(paste0('FaciesStat.', names(.))) %>%
        rename(FaciesBlockNum=FaciesStat.FaciesBlockNum)
      
      #Make the FaciesStatJoin dataframe that we will join with the summarized data (i.e., FaciesSummarizeStat) then join to the section data
      FaciesStatJoin <- FaciesStatJoin %>%
        group_by(FaciesBlockNum) %>%
        summarise(Thickness = max(Thickness))
      
      #Join back to the sect data, first by joining the two summarized data together, then the summarized data to the SectData
      FaciesStatJoin <-left_join(FaciesStatJoin,FaciesSummarizeStat,by="FaciesBlockNum")
      SectData <- left_join(SectData,FaciesStatJoin, by="Thickness")
    }
    
    #If Element is selected group by ElementBlockNum
    if(input$Discrete_ElementStat == TRUE & "ElementBlockNum" %in% colnames(SectData)){
      
      #Get the names of the columns that we'll need then remove them from the SectData
      ElementStatCol <- c(ColNames,"ElementBlockNum")
      ElementStatJoin <- c("ElementBlockNum","Thickness")
      ElementSummarize  <-  SectData[,ElementStatCol] # should use dplyr::select for this eventually
      ElementStatJoin <- SectData[,ElementStatJoin] # should use dplyr::select for this eventually
      
      #Group by ElementNum Summarize_all (min, max, average, stdev)
      ElementSummarizeStat <- ElementSummarize %>%
        group_by(ElementBlockNum) %>%
        summarise_all(.funs = c(min="min", max="max", mean="mean", sd="sd"),na.rm = TRUE)
      ElementSummarizeStat[sapply(ElementSummarizeStat, is.infinite)] <- NA # because some elements will have no measurements they will have a summarized value of infinite (INF), so we will change the INF for NA
      
      #Add a prefix (ElementStat.) to each column, then fix the ElementBlockNum column
      ElementSummarizeStat <- ElementSummarizeStat %>%
        setNames(paste0('ElementStat.', names(.))) %>%
        rename(ElementBlockNum=ElementStat.ElementBlockNum)
      
      #Make the ElementStatJoin dataframe that we will join with the summarized data (i.e., ElementSummarizeStat) then join to the section data
      ElementStatJoin <- ElementStatJoin %>%
        group_by(ElementBlockNum) %>%
        summarise(Thickness = max(Thickness))
      
      #Join back to the sect data, first by joining the two summarized data together, then the summarized data to the SectData
      ElementStatJoin <-left_join(ElementStatJoin,ElementSummarizeStat,by="ElementBlockNum")
      SectData <- left_join(SectData,ElementStatJoin, by="Thickness")
    }
    
    #If ElementSet is selected group by ElementSetBlockNum
    if(input$Discrete_ElementSetStat == TRUE & "ElementSetBlockNum" %in% colnames(SectData)){
      
      #Get the names of the columns that we'll need then remove them from the SectData
      ElementSetStatCol <- c(ColNames,"ElementSetBlockNum")
      ElementSetStatJoin <- c("ElementSetBlockNum","Thickness")
      ElementSetSummarize  <-  SectData[,ElementSetStatCol] # should use dplyr::select for this eventually
      ElementSetStatJoin <- SectData[,ElementSetStatJoin] # should use dplyr::select for this eventually
      
      #Group by ElementSetNum Summarize_all (min, max, average, stdev)
      ElementSetSummarizeStat <- ElementSetSummarize %>%
        group_by(ElementSetBlockNum) %>%
        summarise_all(.funs = c(min="min", max="max", mean="mean", sd="sd"),na.rm = TRUE)
      ElementSetSummarizeStat[sapply(ElementSetSummarizeStat, is.infinite)] <- NA # because some elements will have no measurements they will have a summarized value of infinite (INF), so we will change the INF for NA
      
      
      #Add a prefix (ElementSetStat.) to each column, then fix the ElementSetBlockNum column
      ElementSetSummarizeStat <- ElementSetSummarizeStat %>%
        setNames(paste0('ElementSetStat.', names(.))) %>%
        rename(ElementSetBlockNum=ElementSetStat.ElementSetBlockNum)
      
      #Make the ElementSetStatJoin dataframe that we will join with the summarized data (i.e., ElementSetSummarizeStat) then join to the section data
      ElementSetStatJoin <- ElementSetStatJoin %>%
        group_by(ElementSetBlockNum) %>%
        summarise(Thickness = max(Thickness))
      
      #Join back to the sect data, first by joining the two summarized data together, then the summarized data to the SectData
      ElementSetStatJoin <-left_join(ElementSetStatJoin,ElementSetSummarizeStat,by="ElementSetBlockNum")
      SectData <- left_join(SectData,ElementSetStatJoin, by="Thickness")
    }
    
    showNotification("Process Complete - Discrete measurements have been joined to the stratigraphic data", type = "message")
    showNotification("Merged data is ready to be downloaded", type = "message")
    
    #Bind the joined data to the reactive dataframe
    SectDataJoined_Disc$df <-  bind_rows(SectDataJoined_Disc$df,SectData)
    SectDataJoined_Disc$df <-  SectDataJoined_Disc$df[-1,]
    
  }) #end of ImportMeasurements
  
  #Function for downloading data once it is merged
  output$downloadMergedMeasurementData <- downloadHandler(
    filename = function() {
      paste('MergedStratigraphicData-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(SectDataJoined_Disc$df, con, na = "", row.names=FALSE)
    }
  ) #end of download
  
  
  #####################################################################################################
  #Server code related to the integration of continuous measurements to discretized stratigraphic data
  #####################################################################################################
  
  ##Rendering table for input continuous data .csv file
  output$MeasurementContents2 <- renderTable({
    inMeasurementFile2 <- input$measurementfile2
    
    if (is.null(inMeasurementFile2))
      return(NULL)
    
    read.csv(inMeasurementFile2$datapath, header = input$header2)
  }) # end of MeasurementContents
  
  ##Joining the data based on interpolated log profiles or averaged log profiles depending on the scale of the logs
  observeEvent(input$ImportMeasurments_Continuous, {
    
    ##Import the continuous measurements and the section data
    inMeasurementFile2 <- input$measurementfile2
    MeasurementData <- read.csv(inMeasurementFile2$datapath, header = input$header2, sep = ",")
    
    inSectDataFile2 <- input$sectfile2
    SectData <-   read.csv(inSectDataFile2$datapath, header = input$header2, sep = ",")
    ColNames <- colnames(SectData)
    
    if(ColNames[1] == "X"){
      SectData <- SectData[,-1]
    }
    
    ThickCol <- input$ThickColumn2
    SectDataThickCol <- which(colnames(SectData)=="Thickness")
    
    ##Find the increments of both datasets
    SectDataInc <-  SectData[3,SectDataThickCol] - SectData[2,SectDataThickCol]
    MeasurementInc <- MeasurementData[3,ThickCol] - MeasurementData[2,ThickCol]
    
    ##Create a sequence for the continuous data to be interpolated with and joined on to
    Thickness <- seq(from=SectData[1,SectDataThickCol], to=SectData[nrow(SectData),SectDataThickCol], by = SectDataInc)
    Continuous.df <- data.frame(Thickness)
    
    ##Interpolate each type of continuous data on to the same sequence as the section data and join it together
    if(SectDataInc<MeasurementInc){  #Test to see if we nee to interpolate data (SectDataInc<MeasurementDataInc) or average the data (SectDataInc>MeasurementDataInc)
      if(input$Interpolate_Continuous==TRUE){ #If the measurement data is to be interpolated, create a function to describe each measurement column
        for (i in  1:ncol(MeasurementData)){ #In this loop we want to skip the depth/thickness column. So if i = thickcolumn then we'll skip it and move on to the next value of i
          if(i != ThickCol){
            #Get the column names for the thickness as well as the
            ColNames <- names(MeasurementData)
            DataName <- ColNames[i]
            ThickName <- ColNames[ThickCol]
            
            #Create a function
            DataFun <-  approxfun(x=MeasurementData[[ThickName]],y=MeasurementData[[DataName]])
            
            #Propagate function
            ContData <- DataFun(Thickness)
            
            #Join the data to the thickness
            Continuous.df <- data.frame(Continuous.df, ContData)
            
            #Change the name from ContData back to the name of the column when imported
            colnames(Continuous.df)[colnames(Continuous.df)=="ContData"] <- ColNames[i]
          } #End of processing approx fun data processing
        } #End of for loop
      } else { #If the data is not to be interpolated, rename the indicated thickness column and round thickness measurements to the stratigraphic increments
        
        #Rename thickness Column
        colnames(MeasurementData)[ThickCol] <- "Thickness"
        
        #Round the thickness to the nearest descritized increment
        MeasurementData <- MeasurementData %>% mutate(Thickness = round((.[[ThickCol]])/SectDataInc)*SectDataInc)
        
        #Set measurement data to Continuous.df so it can be joined at the end of all these ifs.
        Continuous.df <- MeasurementData
      }
    }else{ #This is the case the increment of the joining data is smaller than the incriment of the section data and and a rolling average is run over the data prior to joining it. As it is impossible to merely join the data to the section without interpolating it, the "Interpolate" checkbox does nothing here.
      AveWindow <- round(SectDataInc/MeasurementInc)  #We have to find what our window size needs to be for the rolling average - we'll just put this to the closest integer
      for (i in  1:ncol(MeasurementData)){ #In this loop we want to rolling average each the columns, but we want to skip the thickness column
        if(i == ThickCol){
        } else {
          #Get the column names for the thickness as well as the
          ColNames <- names(MeasurementData)
          DataName <- ColNames[i]
          ThickName <- ColNames[ThickCol]
          
          #Create a running average of the data
          MeasurementData  <-  MeasurementData %>% mutate(RollAve = rollmean(x = MeasurementData[[DataName]], k=AveWindow,align="center",na.pad=TRUE))
          
          #Create a function from the rolling average
          DataFun <-  approxfun(x=MeasurementData[[ThickName]],y=MeasurementData$RollAve)
          
          #Propagate function using the thickness and join the new data to the section in a dataframe
          ContData <- DataFun(Thickness)
          Continuous.df <- data.frame(Continuous.df, ContData)
          
          #Change the name from ContData back to the name of the column when imported
          colnames(Continuous.df)[colnames(Continuous.df)=="ContData"] <- DataName
        } #End of processing approx fun data processing
      } #End of for loop
    } #End of processing data
    
    #Join the new columns to the dataframe
    #Due to issues with floating point numbers the thicknesses are converted from numeric to strings prior to joining. Some values disappear without converting to strings.
    Continuous.df$Thickness <- as.character(Continuous.df$Thickness)
    SectData$Thickness <- as.character(SectData$Thickness)
    
    SectData <-  left_join(SectData,Continuous.df, by = c("Thickness"))
    SectData$Thickness <- as.numeric(SectData$Thickness)
    
    ##Statistics based upon user inputs of what level to do the statistics on: bed, facies, element, element set.
    #Get the names of the data/columns that we'll be summarizing and drop "Thickness"
    ColNames <- colnames(Continuous.df)
    
    ####If the user selects the check boxes to summarize the data, summarize the each continuous data based upon the bed number, facies block number, etc.
    ####What we are going to do in this, is we are going to make two dataframes, that summarizes the continuous measurement by bed/block number (e.g., BedSummarizeStat) and the other dataframe calculates that maximum thickness of each bed/block number that we will use to join back to the main section data (e.g., BedStatJoin)
    #If summarize by bed is selected then we summarize based on bed number
    if(input$Continuous_BedStat == TRUE){
      #Do some selection of columns, we are going to use throughout
      BedStatCol <- ColNames
      BedStatCol <- c(BedStatCol,"BedNumber")
      BedStatJoin <- c("BedNumber","Thickness")
      
      #Get the columns from the SectData that match the BedStatCol
      BedSummarize  <-  SectData[,BedStatCol]
      BedStatJoin <- SectData[,BedStatJoin]
      str(BedStatJoin)
      
      #Group by BedNum Summarize_all (min, max, average, stdev)
      BedSummarizeStat <- BedSummarize %>%
        group_by(BedNumber) %>%
        summarise_at(vars(-Thickness), .funs = c(min="min", max="max", mean="mean", sd="sd"), na.rm=TRUE)
      
      #Add a prefix (BedStat.) to each column, then fix the BedNumber column
      BedSummarizeStat <- BedSummarizeStat %>%
        setNames(paste0('BedStat.', names(.))) %>%
        rename(BedNumber=BedStat.BedNumber)
      
      #Make the BedStatJoin dataframe that has the maximum thickness of each bed, that we will use to join back to the section data
      BedStatJoin <- BedStatJoin %>%
        group_by(BedNumber) %>%
        summarise(Thickness = max(Thickness))
      
      #Join the summarized data and the maximum position of each bed together
      BedStatJoin <- left_join(BedStatJoin,BedSummarizeStat,by="BedNumber") %>% select(-BedNumber)
      #BedStatJoin <- BedStatJoin[ , !(names(BedStatJoin) %in% c("BedNumber"))]
      
      #Join the data summarized measurement data to the section data
      SectData <- left_join(SectData,BedStatJoin, by="Thickness")
    }
    
    #If summarize by facies is selected then we summarize based on FaciesBlockNum
    if(input$Continuous_FaciesStat == TRUE & "FaciesBlockNum" %in% colnames(SectData)){
      #Do some selection of columns, we are going to use throughout
      FaciesStatCol <- ColNames
      FaciesStatCol <- c(FaciesStatCol,"FaciesBlockNum")
      FaciesStatJoin <- c("FaciesBlockNum","Thickness")
      
      #Retrieve the columns from the SectData that match the FaciesStatCol (the measurment data + thickness + FaciesBlockNum)
      FaciesSummarize  <-  SectData[,FaciesStatCol]
      FaciesStatJoin <- SectData[,FaciesStatJoin]
      str(FaciesStatJoin)
      
      #Group by FaciesBlockNum Summarize_all (min, max, average, stdev)
      FaciesSummarizeStat <- FaciesSummarize %>%
        group_by(FaciesBlockNum) %>%
        summarise_at(vars(-Thickness), .funs = c(min="min", max="max", mean="mean", sd="sd"), na.rm=TRUE)
      
      FaciesSummarizeStat <- FaciesSummarizeStat %>%
        setNames(paste0('FaciesStat.', names(.))) %>%
        rename(FaciesBlockNum=FaciesStat.FaciesBlockNum)
      
      #Group by FaciesBlockNum and calculate the max thickness of each FaciesBlockNum that we will use to join back to the data
      FaciesStatJoin <- FaciesStatJoin %>%
        group_by(FaciesBlockNum) %>%
        summarise(Thickness = max(Thickness))
      
      #join back to the stect data
      FaciesStatJoin <- left_join(FaciesStatJoin,FaciesSummarizeStat,by="FaciesBlockNum") %>% select(-FaciesBlockNum)
      #FaciesStatJoin <- FaciesStatJoin[ , !(names(FaciesStatJoin) %in% c("FaciesBlockNum"))]
      SectData <- left_join(SectData,FaciesStatJoin, by="Thickness")
    }
    
    #If summarize by element is selected then we summarize based on ElementBlockNum
    if(input$Continuous_ElementStat == TRUE & "ElementBlockNum" %in% colnames(SectData)){
      
      #Do some selection of columns, we are going to
      ElementStatCol <- ColNames
      ElementStatCol <- c(ElementStatCol,"ElementBlockNum")
      ElementStatJoin <- c("ElementBlockNum","Thickness")
      
      #Get the columns from the SectData that match the BedStatCol
      ElementSummarize  <-  SectData[,ElementStatCol]
      ElementStatJoin <- SectData[,ElementStatJoin]
      str(ElementStatJoin)
      
      #Group by ElementBlockNum Summarize_all (min, max, average, stdev)
      ElementSummarizeStat <- ElementSummarize %>%
        group_by(ElementBlockNum) %>%
        summarise_at(vars(-Thickness), .funs = c(min="min", max="max", mean="mean", sd="sd"), na.rm=TRUE)
      
      ElementSummarizeStat <- ElementSummarizeStat %>%
        setNames(paste0('ElementStat.', names(.))) %>%
        rename(ElementBlockNum=ElementStat.ElementBlockNum)
      
      #Group by bed and calculate the max thickness of each ElementBlockNum
      ElementStatJoin <- ElementStatJoin %>%
        group_by(ElementBlockNum) %>%
        summarise(Thickness = max(Thickness))
      
      #join back to the stect data
      ElementStatJoin <- left_join(ElementStatJoin,ElementSummarizeStat,by="ElementBlockNum")  %>% select(-ElementBlockNum)
      #ElementStatJoin <- ElementStatJoin[ , !(names(ElementStatJoin) %in% c("ElementBlockNum"))]
      SectData <- left_join(SectData,ElementStatJoin, by="Thickness")     
    }
    
    #If summarize by facies is selected then we summarize based on ElementSetBlockNum
    if(input$Continuous_ElementSetStat == TRUE & "ElementSetBlockNum" %in% colnames(SectData)){

      #Do some selection of columns, we are going to
      ElementSetStatCol <- ColNames
      ElementSetStatCol <- c(ElementSetStatCol,"ElementSetBlockNum")
      ElementSetStatJoin <- c("ElementSetBlockNum","Thickness")
      
      #Get the columns from the SectData that match the BedStatCol
      ElementSetSummarize  <-  SectData[,ElementSetStatCol]
      ElementSetStatJoin <- SectData[,ElementSetStatJoin]
      str(ElementSetStatJoin)
      
      #Group by ElementSetBlockNum Summarize_all (min, max, average, stdev)
      ElementSetSummarizeStat <- ElementSetSummarize %>%
        group_by(ElementSetBlockNum) %>%
        summarise_at(vars(-Thickness), .funs = c(min="min", max="max", mean="mean", sd="sd"), na.rm=TRUE)
      
      ElementSetSummarizeStat <- ElementSetSummarizeStat %>%
        setNames(paste0('ElementSetStat.', names(.))) %>%
        rename(ElementSetBlockNum=ElementSetStat.ElementSetBlockNum)
      
      #Group by bed and calculate the max thickness of each ElementSetBlockNum
      ElementSetStatJoin <- ElementSetStatJoin %>%
        group_by(ElementSetBlockNum) %>%
        summarise(Thickness = max(Thickness))
      
      #join back to the stect data
      ElementSetStatJoin <- left_join(ElementSetStatJoin,ElementSetSummarizeStat,by="ElementSetBlockNum") %>% select(-ElementSetBlockNum)
      #ElementSetStatJoin <- ElementSetStatJoin[ , !(names(ElementSetStatJoin) %in% c("ElementSetBlockNum"))]
      SectData <- left_join(SectData,ElementSetStatJoin, by="Thickness")
    }
       
    #Bind the joined data to the reactive dataframe
    SectDataJoined_Cont$df <-  bind_rows(SectDataJoined_Cont$df,SectData)
    SectDataJoined_Cont$df <-  SectDataJoined_Cont$df[-1,]
    
    showNotification("Process Complete - Discrete measurements have been joined to the stratigraphic data", type = "message")
    showNotification("Merged data is ready to be downloaded", type = "message")
  }) #end of ImportMeasurements
  
  #Function for downloading data once it is merged
  output$downloadMergedMeasurementData2 <- downloadHandler(
    filename = function() {
      paste('MergedStratigraphicData-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(SectDataJoined_Cont$df, con, na = "", row.names=FALSE)
    }
  ) #end of download
  
  #############################################################################################################################
  #Server code related to the creation of a stratigraphic library through appending multiple descretized stratigraphic sections
  #############################################################################################################################
  
  #Rendering the table that is the selected file
  output$SectDataToImport <- renderTable(NumSectData$df)
  
  observeEvent(input$ImportSectionData,{ #when the "import"Import Selected File" button is clicked
    #Read the .csv and bind all the SectData from that file to the total data keeping all columns
    TempSectFile <- read.csv(input$Sectfile$datapath, header = TRUE, sep = ",", stringsAsFactors=TRUE, na.strings = "NA")
    TotalSectionData$df <-  bind_rows(TotalSectionData$df,TempSectFile)
    
    #Remove previous place holder
    if(TotalSectionData$df[1,1] == 0.1){
      TotalSectionData$df = TotalSectionData$df[-1,]
    }
    
    #Append the file name and the number of rows to the dataframe (NumSectData$df) that is diaplayed on the page (output$SectDataToImport)
    NumSectData$df <- bind_rows(NumSectData$df,tibble(FileName=input$Sectfile$name,NumRows=nrow(TempSectFile)))
    if(NumSectData$df[1,1]=="Example"){
      NumSectData$df = NumSectData$df[-1,]
    }
  }) #end of import data and appending it together
  
  #Function for downloading data once it is imnported and combined
  output$downloadTotalData <- downloadHandler(
    filename = function() {
      paste('MergedStratigraphicData-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(TotalSectionData$df, con, na = "", row.names=FALSE, sep=";")
    }
  ) #end of download
}

# Run the application
shinyApp(ui = ui, server = server)

}


