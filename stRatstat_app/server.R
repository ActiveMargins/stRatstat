
#Required packages
library(shiny)
library(imager)
library(ggplot2)
library(dplyr)
library(zoo)
library(DT)

server <- function(input, output) {
  
  #Reactive variables related to the image
  LoadedImage <- reactive({load.image(gsub("\\\\", "/", input$files$datapath))})
  ImageWidth <- reactive(width(LoadedImage()))
  ImageHeight <- reactive(height(LoadedImage()))
  null <-  reactiveValues(df=tibble("nullx"=c(0,0),"nully"=c(0,0)))
  
  #Reactive dataframes related to logging and processing the data
  values <- reactiveValues(df=tibble(x=-5,y=-5, pt=1))
  topbottom <- reactiveValues(df=tibble(x=-5,y=-5,PtLabel="Example"))
  gsmarkers <- reactiveValues(df=tibble(x=-5,y=-5,pt=1))
  bedtops_raw <- reactiveValues(df=tibble(x=-5,y=-5, pt=1, BedTop=1))
  sedstructnames <- reactiveValues(df=tibble(name = "Example Sed. Structure"))
  sedstrat_raw <- reactiveValues(df=tibble(ymin_raw=-1,ymax_raw=-2, SedStruct="Example"))
  faciesnames <- reactiveValues(df=tibble(name = "Example Facies"))
  facies_raw <- reactiveValues(df=tibble(ymin_raw=-1,ymax_raw=2, FacName="Example", FacBlockNum=0))
  elementnames <- reactiveValues(df=tibble(name = "Example Element"))
  element_raw <- reactiveValues(df=tibble(ymin_raw=-1,ymax_raw=2, ElementName="Example", ElementBlockNum=0))
  elementsetnames <- reactiveValues(df=tibble(name = "Example Element Set"))
  elementset_raw <- reactiveValues(df=tibble(ymin_raw=-1,ymax_raw=2, ElementSetName="Example", ElementSetBlockNum=0))
  NumSectData <- reactiveValues(df=tibble(FileName="Example", NumRows = 1))
  TotalSectionData <- reactiveValues(df=tibble(Thickness=0.1))
  SectDataProcessed <- reactiveValues(df=tibble(Thickness=0.1))
  SectDataJoined <- reactiveValues(df=tibble(Thickness=0.1))
  
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
  
  #--------------------------------------------------------------------
  #CODE TO SAVE AND LOAD DATA
  #--------------------------------------------------------------------
  #Saving the data
  observeEvent(input$save_data,{
    #convert all the reactive dataframes in their current state into static dataframes with a new column that says their function
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
  
  #Downloading Saved Data
  output$downloadSaveData <- downloadHandler(
    filename = function() {
      paste("SavedStratgraphicData", Sys.Date(), ".csv", sep="")
    },
    content = function(con) {
      write.csv(SaveData$df, con, na = "")
    }
  )

  #Loading data
  observeEvent(input$load_data, {
    
    inFile <- input$load_data_file
    
    if(is.null(inFile)) {
      print("NoFile")
      showNotification("No File Loaded", type = "error")
    } else {
      LoadData <- read.csv(inFile$datapath, header=TRUE, stringsAsFactors=FALSE)
      
      #Rip out the logged points data
      topbottompts_load <- LoadData %>% filter (Save=="TopBottom") %>% select(x,y,PtLabel) %>% filter (x>0)
      bedtops_load <- LoadData %>% filter(Save=="BedTops") %>% select(x, y, pt, BedTop) %>% filter (x>0)
      values_load<- LoadData %>% filter(Save=="GSValues") %>% select(x,y,pt) %>% filter (x>0)
      gsmarkers_load <- LoadData %>% filter(Save=="GSMarkers") %>% select(x,y,pt) %>% filter (x>0)
      sedstrat_load <- LoadData %>% filter(Save=="SedStrut") %>% select(ymin_raw, ymax_raw, SedStruct) %>% filter (ymin_raw>0)
      facies_load <- LoadData %>% filter(Save=="Facies") %>% select(ymin_raw, ymax_raw, FacName, FacBlockNum) %>% filter (ymin_raw>0)
      element_load <- LoadData %>% filter(Save=="Elements") %>% select(ymin_raw, ymax_raw, ElementName, ElementBlockNum) %>% filter (ymin_raw>0)
      elementset_load <- LoadData %>% filter(Save=="ElementSets") %>% select(ymin_raw, ymax_raw, ElementSetName, ElementSetBlockNum) %>% filter (ymin_raw>0)
      
      #Rip out the name columns from the logged data a bit differently so we can bind it into the reactive dataframe
      sedstrat_name_load <- data.frame(name=unique(sedstrat_load$SedStruct))
      facies_name_load <- data.frame(name=unique(facies_load$FacName))
      element_name_load <- data.frame(name=unique(element_load$ElementName))
      elementset_name_load <- data.frame(name=unique(elementset_load$ElementSetName))
      
      #Bind data into reactive dataframes
      topbottom$df <- bind_rows(topbottom$df, topbottompts_load) 
      bedtops_raw$df <- bind_rows(bedtops_raw$df, bedtops_load)
      values$df <- bind_rows(values$df, values_load)
      gsmarkers$df <- bind_rows(gsmarkers$df,gsmarkers_load)
      sedstrat_raw$df <- bind_rows(sedstrat_raw$df,sedstrat_load)
      facies_raw$df <- bind_rows(facies_raw$df,facies_load)
      element_raw$df <- bind_rows(element_raw$df,element_load)
      elementset_raw$df <- bind_rows(elementset_raw$df,elementset_load)
      sedstructnames$df <- bind_rows(sedstructnames$df,sedstrat_name_load)
      faciesnames$df <- bind_rows(faciesnames$df,facies_name_load)
      elementnames$df <- bind_rows(elementnames$df,element_name_load)
      elementsetnames$df <- bind_rows(elementsetnames$df,elementset_name_load)

      showNotification("Data Loaded", type = "message")
    }
  })
  
  # ------------------------------------------------------------------  
  #CODE FOR DIGITIZING STRATGRAPHIC SECTIONS 
  # ------------------------------------------------------------------

  # -------------------------------------------------------------------Server code related to the page "2. Digitze beds and grain sizes"  
  # Linked plots (middle and right)
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot2 <- renderPlot({
    if(is.null(input$files)){} # if there's no file input don't plot anything
    else{ # if there is a file input then plot the interactive ggplot
      ggplot(null$df,aes(x=nullx,y=nully)) +
        annotation_raster(LoadedImage(),ymin=2,ymax = height(LoadedImage()), xmin=0, xmax=width(LoadedImage())) + 
        geom_point() + 
        BlankTheme + 
        coord_fixed()+
        xlim(0, width(LoadedImage()))+
        ylim(0, height(LoadedImage()))
    }
  })
  
  output$plot3 <- renderCachedPlot({
    if(is.null(input$files)){} # if there's no file input don't plot anything
    else{ # if there is a file input then plot the interactive ggplot
      ggplot(data=null$df, aes(x=nullx,y=nully,color="NA"))+geom_point()  +
        BlankTheme +
        annotation_raster(LoadedImage(),ymin=0,ymax=height(LoadedImage()), xmin=0, xmax=width(LoadedImage())) + 
        coord_cartesian(xlim=ranges2$x,ylim=ranges2$y) + 
        geom_point(data=values$df, aes(x=x,y=y),inherit.aes = FALSE, color="green") + 
        geom_point(data=gsmarkers$df, aes(x=x,y=y), color="blue")+ 
        geom_point(data=topbottom$df, aes(x=x,y=y), color="red")+
        geom_hline(yintercept=bedtops_raw$df$y, linetype="dashed", color = "red", size=1)+
        geom_label(data=topbottom$df, aes(x=x, y=y, label=ifelse(x>1, as.character(PtLabel),''),hjust=1.25))+
        geom_label(data=values$df, aes(x=x, y=y, label=ifelse(x>1, as.character(pt),''),hjust=1.25)) + 
        geom_label(data=bedtops_raw$df, aes(x=3, y=y, label=ifelse(x>1, as.character(pt),'')))
    }
  },
  cacheKeyExpr = { list(input$updateplot3,input$plot2_brush, input$delete_pt, input$delete_row) }
  )
  
  # When a double-click happens on the left plot, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymax, brush$ymin)
    } else {
      ranges2$x <- null$df$nullx
      ranges2$y <- null$df$nully
    }
  })
  
  #observe clicks on the right plot to get xy coordinates - only keep them if(recordpts == TRUE)
  observeEvent(input$plotclick,{
    if(input$recordpts == TRUE){
      
      if(input$pt_type == 1){
        if(nrow(topbottom$df)==1) {
          topbottom$df <- bind_rows(topbottom$df,tibble(x=input$plotclick$x, y=input$plotclick$y, PtLabel="Bottom"))
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
  
  
  output$pttable <- DT::renderDT(
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
  observeEvent(input$delete_pt, {
    if(input$pt_type == 1){
      topbottom$df <- topbottom$df[-nrow(topbottom$df),]
    }    
    
    if(input$pt_type == 2){
      gsmarkers$df <- gsmarkers$df[-nrow(gsmarkers$df),]
    } 
    
    if(input$pt_type == 3){
      bedtops_raw$df <- bedtops_raw$df[-nrow(bedtops_raw$df),]
    }
    
    if(input$pt_type == 4){
      values$df <- values$df[-nrow(values$df),]
    }
  })
  
  #function for deleting the rows in the selected table 
  observeEvent(input$delete_row, {
    if(!is.null(input$pttable_rows_selected)){
      delete_row <- input$pttable_rows_selected + 1
      if(input$pt_type == 1){
        topbottom$df <- topbottom$df[-(delete_row),]
      }    
      if(input$pt_type == 2){
          gsmarkers$df <- gsmarkers$df[-(delete_row),]
      } 
      if(input$pt_type == 3){
          bedtops_raw$df <- bedtops_raw$df[-(delete_row),]
          bedtops_raw$df <- bedtops_raw$df %>% mutate(pt = row_number()-1)
      }
      if(input$pt_type == 4){
          values$df <- values$df[-(delete_row),]
          values$df <- values$df %>% mutate(pt = row_number()-1)
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
        ggplot(data=null$df, aes(x=nullx,y=nully))+geom_point()  +  
          BlankTheme +
          annotation_raster(LoadedImage(),ymin=0,ymax =height(LoadedImage()), xmin=0, xmax=width(LoadedImage())) + 
          coord_cartesian(xlim=ranges2_sed$x,ylim=ranges2_sed$y) + 
          geom_hline(yintercept=bedtops_raw$df$y, linetype="dashed", color = "red", size=1)+ 
          geom_rect(data=sedstrat_raw$df, mapping=aes(xmin=0, xmax=width(LoadedImage()), ymin=sedstrat_raw$df$ymax_raw, ymax=sedstrat_raw$df$ymin_raw), color="blue", inherit.aes = FALSE, alpha = 0.1)
      }
    })
  
  # If plot 4 is brushed chnage the range of the zoom on the GG plot to the brush range (x,y); if not, reset the zoom.
  observe({
    plot4brush <- input$plot4_brush
    if (!is.null(plot4brush)) {
      ranges2_sed$x <- c(plot4brush$xmin, plot4brush$xmax)
      ranges2_sed$y <- c(plot4brush$ymax, plot4brush$ymin)
    } else {
      ranges2_sed$x <- null$df$nullx
      ranges2_sed$y <- null$dfnully
    }
  })
  
  #Record brush on plot 5 as two y-coordinates and the sed structure value
  observeEvent(input$rec_sed_brush, {
    if(is.null(input$plot5_brush)){ 
      print("No brush, no data")
    }else{
      #if the brushes are outside of the limits of the section then clip them back to the top or bottom of the image
      plot5brush <- input$plot5_brush
      topbottompts <- topbottom$df %>% filter(y>0)
        
      if(plot5brush$ymax[1]>(max(topbottompts$y))){
        plot5brush$ymax[1] <-(max(topbottompts$y))
      }
        
      if(plot5brush$ymin[1]<(min(topbottompts$y))){
        plot5brush$ymin[1] <-(min(topbottompts$y))
      }
       
      sedstrat_raw$df <- bind_rows(sedstrat_raw$df,tibble(ymin_raw=plot5brush$ymax,ymax_raw=plot5brush$ymin, SedStruct=input$sedstruct_input))
    }
  })

  #Creating facies
  observeEvent(input$create_sedstruct_name, {
    sedstructnames$df <- bind_rows(sedstructnames$df,tibble(name=input$sedstruct_text))
  })
  
  #Delete facies from facies list
  observeEvent(input$delete_sedstruct_name, {
    if(nrow(sedstructnames$df)>1){
      sedstructnames$df <- sedstructnames$df[-nrow(sedstructnames$df),]
    }
  })
  
  #Output radio UI
  output$sedstructchoices <- renderUI({
    radioButtons("sedstruct_input", "Sedimentary Structures",choices = sedstructnames$df$name)
  })
  
  #Table for logged data
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
        geom_rect(data=facies_raw$df, mapping=aes(xmin=0, xmax=width(LoadedImage()), ymin=facies_raw$df$ymax_raw, ymax=facies_raw$df$ymin_raw), color="blue", inherit.aes = FALSE, alpha = 0.1)
    }
  })
    
  # If plot 6 is brushed chnage the range of the zoom on the GG plot to the brush range (x,y); if not, reset the zoom.
  observe({
    plot6brush <- input$plot6_brush
    if (!is.null(plot6brush)) {
      ranges2_fac$x <- c(plot6brush$xmin, plot6brush$xmax)
      ranges2_fac$y <- c(plot6brush$ymax, plot6brush$ymin)
      
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
  output$factable <- renderDataTable(
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
      facies_raw$df <- bind_rows(facies_raw$df,tibble(ymin_raw=brushymax,ymax_raw=brushymin, FacName=as.character(input$facies_input), FacBlockNum=nrow(facies_raw$df)+1))
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
    }
  })    
    
  # -------------------------------------------------------------------Server code related to the page "5. Digitizing architectural elements"
  
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
        geom_rect(data=element_raw$df, mapping=aes(xmin=0, xmax=width(LoadedImage()), ymin=element_raw$df$ymax_raw, ymax=element_raw$df$ymin_raw), color="blue", inherit.aes = FALSE, alpha = 0.1)
    }
  })
    
  # If plot 8 is brushed chnage the range of the zoom on the GG plot to the brush range (x,y); if not, reset the zoom.
  observe({
    plot8brush <- input$plot8_brush
    if (!is.null(plot8brush)) {
      ranges2_element$x <- c(plot8brush$xmin, plot8brush$xmax)
      ranges2_element$y <- c(plot8brush$ymax, plot8brush$ymin)
      
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
  output$elementtable <- renderDataTable(
    datatable(filter(element_raw$df, ymin_raw>0), rownames=FALSE, filter=c('none'))
  )
    
  #log Element interval from brush when clicking the update Element button
  #Record brush on plot 7 as two y-coordinates and the Element name from radio buttons
  observeEvent(input$update_element, {
    
    if(!is.null(input$plot9_brush)){
      plot9brush <- input$plot9_brush
      topbottompts<- topbottom$df %>% filter(x>0)
      bedtops_raw <- bedtops_raw$df %>% filter(x>0)
      
      print("Got Input Data")
        
      #if the user wants the Element boundaries to be clipped to bed boundaries then we'll do that
      if(input$modifyelements == TRUE){
        
        bedtops_raw <- bind_rows(topbottompts,bedtops_raw)
        print("Got all bed tops together")
          
        #Top of the Element selection
        bedindex <-which(abs(bedtops_raw$y-plot9brush$ymax)==min(abs(bedtops_raw$y-plot9brush$ymax)))
        brushymax <- bedtops_raw[[bedindex,2]]
        print("NewTop")
        
        #Bottom of the Element selection
        bedindex <- which(abs(bedtops_raw$y-plot9brush$ymin)==min(abs(bedtops_raw$y-plot9brush$ymin)))
        brushymin <- bedtops_raw[[bedindex,2]]
        print("NewBottom")
         
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
      element_raw$df <- bind_rows(element_raw$df,tibble(ymin_raw=brushymax,ymax_raw=brushymin, ElementName=as.character(input$element_input), ElementBlockNum=nrow(element_raw$df)+1))
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
    }
  })  
  
  # -------------------------------------------------------------------Server code related to the page "Digitize architectural element sets or stratigraphic intervals"
    
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
        geom_rect(data=elementset_raw$df, mapping=aes(xmin=0, xmax=width(LoadedImage()), ymin=elementset_raw$df$ymax_raw, ymax=elementset_raw$df$ymin_raw), color="blue", inherit.aes = FALSE, alpha = 0.1)
    }
  })
    
  # If plot 10 is brushed chnage the range of the zoom on the GG plot to the brush range (x,y); if not, reset the zoom.
  observe({
    plot10brush <- input$plot10_brush
    if (!is.null(plot10brush)) {
      ranges2_elementset$x <- c(plot10brush$xmin, plot10brush$xmax)
      ranges2_elementset$y <- c(plot10brush$ymax, plot10brush$ymin)
      
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
  output$elementsettable <- renderDataTable(
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
      elementset_raw$df <- bind_rows(elementset_raw$df,tibble(ymin_raw=brushymax,ymax_raw=brushymin, ElementSetName=as.character(input$elementset_input), ElementSetBlockNum=nrow(elementset_raw$df)+1))
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
    }
  })  
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Code related to processing the collected data points 
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
  
  observeEvent(input$process_pts, {
    #Filter out any negative numbers that may be hiding out, sort based on y-value, and set to non-reactive dataframe 
    topbottom <- topbottom$df %>% filter(x>0) %>% arrange(y)
    gsmarkers <- gsmarkers$df %>% filter(x>0) %>% arrange(x)
    df.bedtops <- bedtops_raw$df %>% filter(x>0) %>% arrange(y)
    df.GSpts <- values$df %>% filter(x>0)%>% arrange(y)
    df.sedstrat <- sedstrat_raw$df %>% filter(ymin_raw>0) %>% arrange(ymin_raw)
    df.facies <- facies_raw$df %>% filter(ymin_raw>0)%>% arrange(ymin_raw)
    df.element <- element_raw$df %>% filter(ymin_raw>0)%>% arrange(ymin_raw)
    df.elementset <- elementset_raw$df %>% filter(ymin_raw>0)%>% arrange(ymin_raw)
    
    StratInc <- as.numeric(isolate(input$StratInc))
      
    ##Compute the grain size function for mutation
    GScheckGroupIndex <- as.numeric(input$GS_checkGroup)
    diameter <- c(isolate(input$MudSize),isolate(input$SiltSize),isolate(input$VeryFineSize),isolate(input$FineSize),isolate(input$MediumSize),isolate(input$CoarseSize),isolate(input$VeryCoarseSize),isolate(input$GranuleSize),isolate(input$PebbleSize),isolate(input$CobbleSize),isolate(input$BoulderSize)) # make a vector out of all the grain sizes in the process page  
    diameter <- as.numeric(diameter[GScheckGroupIndex])
    df.GS1 <- cbind(gsmarkers,diameter)
    print("GS inputs Bound")
    
    #append two rows to extend the grain sizes out to the min and max width of the images gives those extents the min and the max of the grain sizes
    df.GS1 <- rbind(df.GS1, c(0,0,0, min(df.GS1$diameter)))
    df.GS1 <- rbind(df.GS1,c(width(LoadedImage),0,0, max(df.GS1$diameter)))
      
    # compute the grain size that is used to turn x coordinates of grain size points (df.GSpts) in to numeric grain size
    fun.GS1 <-approxfun(df.GS1$x, df.GS1$diameter, method="linear")
      
    ##compute the thickness function for mutation
    #remove the place holder in the top-bottom df
    print("Doing thickness fucntion")
      
    #Get values for the top and bottom of the core (sometimes in thickness, sometimes in depth) and join them to the df.Thick 
    thicknesses  <-  c(isolate(input$sectthick_base),isolate(input$sectthick_top))
    df.Thick <- bind_cols(topbottom, tibble(sectthick=thicknesses))
    
    #make the thickness function
    fun.Thick <-approxfun(df.Thick$y, df.Thick$sectthick, method="linear")
    print("Done thickness function")
    
    ##Take the grain size profile points and send them through our approx funs (fun.GS1 and fun.Thick)
    df.GSpts <- mutate(df.GSpts,GS = fun.GS1(df.GSpts$x))
    df.GSpts <- mutate(df.GSpts, Thick = (fun.Thick(df.GSpts$y)))

    ####Create a sequence for the thickness of the strat section 
    #Make the sequence
    StratInc <- as.numeric(isolate(input$StratInc))
    NewY <- seq(from = isolate(input$sectthick_base), to = isolate(input$sectthick_top), by = StratInc)
    SectData <- data.frame(Thickness = NewY)
      
    print("SectData created")
    print("New sequence complete")

    #mutate in the rounded depth/thickness into the bedtops dataframe as we as df.gspts
    MatchBedTops <- df.bedtops %>%  mutate(Thick = (fun.Thick(df.bedtops$y)))
    MatchBedTops <- MatchBedTops %>% mutate(Thickness = round(MatchBedTops$Thick/StratInc)*StratInc) %>% 
      select(Thickness,BedTop) %>% 
      arrange(Thickness)
    df.GSpts <- df.GSpts %>% mutate(Thick = round(df.GSpts$Thick/StratInc)*StratInc)
      
    print("MatchBedTops created")

    #Join in the Bed Top locations in to the SectData by Thickness (the rounded thickness)
    SectData <- left_join(SectData, MatchBedTops, by = c("Thickness"))
    SectData$BedTop <-  ifelse(is.na(SectData$BedTop), 0, 1)
  
    #The picked grain size points all have grain sizes after the first function (fun.GS1). We can now make a function out of those points (fun.GS2) and run the entire new sequence (SeqY/SectData$Thickness) through it.
    fun.GS2 <-approxfun(df.GSpts$Thick, df.GSpts$GS, method="linear")
    GSInterp <- fun.GS2(NewY)
      
    df.MatchGS <- data.frame(NewY,GSInterp)
    df.MatchGS <- df.MatchGS %>% rename(Thickness=NewY, GS=GSInterp)
    SectData <- full_join(SectData, df.MatchGS, by = c("Thickness"))

    print("Section Data Joined")
      
    #fill in GS data at the start of the log and at the end of the log
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
  
    print("Extra GS added at start and end")
      
    #to modify the the section data based on the number of input GS points between the bed tops
    print("Modifying GS by number of points - Start")
    for (i in 1:(nrow(MatchBedTops)-1)){
      minbedtop <- as.numeric(MatchBedTops[i,1])
      maxbedtop <- as.numeric(MatchBedTops[i+1,1])
        
      #minbedtop <- as.numeric(df.bedtops[i,4]) # the lower bed top bounding the bed of interest - mutated to get real thickness
      #maxbedtop <- as.numeric(df.bedtops[i+1,4]) # the upper bed top bounding the bed of interest - mutated to get real thickness
      gspts <-  sum((df.GSpts$Thick > minbedtop) & (df.GSpts$Thick <= maxbedtop)) # count the number of digitized grain size points are inbetween those two bedtops

      if(gspts==0){
        SectData$GS <- ifelse(SectData$Thickness > minbedtop & SectData$Thickness <= maxbedtop, NA, SectData$GS) #if we have no points inbetween two bed tops delete the grain size data and call it cover
      } 
        
      if(gspts==1){
        gsindex <-  which(df.GSpts$Thick > minbedtop & df.GSpts$Thick <= maxbedtop)
        SectData$GS <-  ifelse(SectData$Thickness > minbedtop & SectData$Thickness<= maxbedtop, df.GSpts[gsindex,4],SectData$GS) #if we have only 1 point inbetween two bed tops find the original mutated GS and use that for the entire bed.=
      }
        
      if(gspts >= 2){
        GS_filter <- df.GSpts %>% filter(between(Thick,minbedtop,maxbedtop)) # filter the original GS points to get all the GS control points that are betwen two bed tops in question
        Min_GS_Thick <- as.numeric(GS_filter[1,5])
        Min_GS_GS <- as.numeric(GS_filter[1,4])
        Max_GS_Thick <- as.numeric(GS_filter[nrow(GS_filter),5])
        Max_GS_GS <- as.numeric(GS_filter[nrow(GS_filter),4])
          
        SectData$GS <- ifelse(SectData$Thickness <= Min_GS_Thick & SectData$Thickness > minbedtop, Min_GS_GS, SectData$GS)
        SectData$GS <- ifelse(SectData$Thickness > Max_GS_Thick & SectData$Thickness<= maxbedtop, Max_GS_GS, SectData$GS)
      }
    }
    
    SectData <- transform(SectData, GS = as.numeric(GS))
    #write.csv(SectData, file = "~/R/POST_DataPManipulation.csv")
    print("Modifying GS by number of points - End")
    
    #append in the name name and location data from the processing page regardless of the inputs
    SectData$BedNumber <- rep(NA,nrow(SectData))
    SectData$SectName <- rep(input$section_name,nrow(SectData)) #Append Section Name
    SectData$SectLoc <- rep(input$section_loc,nrow(SectData)) #Append Section Location
    SectData$UTM_E <- rep(input$UTM_E,nrow(SectData)) #Append UTM E
    SectData$UTM_N <- rep(input$UTM_N,nrow(SectData)) #Append UTM N
    print("Bed Nubmer, Location, UTM columns added")
    
    #fill in bed numbers then enter loop to fill in the bednum column
    bednumber <- 1
    for (i in 1:nrow(SectData)){
      if (!is.na(SectData[i,3]))
      {SectData[i,4] <- bednumber
      }
      if(SectData[i,2]==1){
        bednumber <- bednumber+1
      }
    }
    print("Bed Numbers Added")      
   
    #mud sand indicator (0 = mud, 1 = sand)
    SectData <- SectData %>% mutate(SandMud = ifelse(SectData$GS < input$ResGS | is.na(SectData$GS), 0, 1)) #if the new GS is less than the reservoir cut-off or is NA then call it not mud (=0) 
    print("reservoir/non-reservoir column added")
    
    #amalgamation indicator (0 = no amalgamation, 1 = amalgamation)
    SectData$Amalg <- rep(0,nrow(SectData)) #Create column for the binary classification as to if the bed contact is amalgamated or not
    print("amalg column added")
    
    # loop through the bed tops and see if there is sand above and below the bedding contact
    for (i in 1:(nrow(MatchBedTops))){
      beddingcontact_depth <- as.numeric(MatchBedTops[i,1])
      beddingcontact_index <- match(beddingcontact_depth, SectData$Thickness)

      if(SectData[beddingcontact_index,9]==1 && SectData[beddingcontact_index+1,9]==1){
        SectData[beddingcontact_index,10] <- 1
      } else {
        SectData[beddingcontact_index,10] <- 0
      }
    }
    print("Amalgamation indicators added")
      
    ##Process the sedimentary structure data
    
    print("Starting Sedimentary Structures")
    
    #Mutate in the thicknesses for the top and bottom of the brushed/recorded intervals
    print(df.sedstrat)
    df.sedstrat <- df.sedstrat %>% mutate(ymin_thick = round((fun.Thick(df.sedstrat$ymax_raw))/StratInc)*StratInc, ymax_thick = round((fun.Thick(df.sedstrat$ymin_raw))/StratInc)*StratInc)
    
    #If statment to process the sedstrat data if there is any logged data
    if(nrow(df.sedstrat)>0){  
      #Create the new columns based on the user inputs
      sedstratcolumns <- unique(df.sedstrat$SedStruct)

      for (i in 1:length(sedstratcolumns)){
        colname <- sedstratcolumns[i]
        SectData[, colname] <- as.character(NA)
      }
      
      #Populate the columns
      for (i in 1:nrow(df.sedstrat)){
        #Get nessicary values, the name of sedimentary structure, as well as the min-max bounds of the interval 
        colname <- df.sedstrat[[i,3]]
        sedstratmin_y <- df.sedstrat[[i,4]]
        sedstratmax_y <- df.sedstrat[[i,5]]
          
        #return the column index that that sedimentary structure belongs to
        colindex <- which(colnames(SectData) %in% colname) 
        
        #return the row numbers that fall within the min-max bounds of the interval
        rowindex <- which(SectData$Thickness >= sedstratmin_y & SectData$Thickness <= sedstratmax_y)
        
        #Input the column name/sedimentary structure 
        SectData[rowindex,colindex] <- colname
      }
    }#end if sedstrat nrow  
    print("Done Sed Strat")
    
    ##Process the Facies data
    print("Starting Facies Processing")
    df.facies <- df.facies %>% mutate(ymin_thick = (round((fun.Thick(df.facies$ymax_raw))/StratInc)*StratInc), ymax_thick = round((fun.Thick(df.facies$ymin_raw))/StratInc)*StratInc)
    print(df.facies)
    
    #if statment to process the facies data if the df.facies has any logged data inside it
    if(nrow(df.facies)>0){  
      SectData$Facies <- rep(NA,nrow(SectData))
      SectData$FaciesBlockNum <- rep(NA,nrow(SectData))
        
      #increment through the facies dataframe and if else each row. Try to get the ifelse to not replace the cells if it fails the test. 
      for (i in 1:nrow(df.facies)){
        faciesmin_y <- as.numeric(df.facies[i,5]) ## Do as numeric ahead outside of the for loop
        faciesmax_y <- as.numeric(df.facies[i,6])
        
        SectData$Facies <-  ifelse(SectData$Thickness>faciesmin_y & SectData$Thickness<=faciesmax_y, df.facies[i,3], SectData$Facies)
        SectData$FaciesBlockNum <- ifelse(SectData$Thickness>faciesmin_y & SectData$Thickness<=faciesmax_y, df.facies[i,4], SectData$FaciesBlockNum)
      }
        
      SectData <- transform(SectData, GS = as.numeric(GS),
                            Facies = as.character(Facies),
                            FaciesBlockNum = as.numeric(FaciesBlockNum)
                            )
    }#end if facies nrow
    print("Done Facies Processing")
      
    ##Process the Element data
    
    print("Starting Element Data")
    df.element <- df.element %>% mutate(ymin_thick = (round((fun.Thick(df.element$ymax_raw))/StratInc)*StratInc)+StratInc, ymax_thick = round((fun.Thick(df.element$ymin_raw))/StratInc)*StratInc)
    
    #if statment to process the element data if df.element has any logged data
    if(nrow(df.element)>0){  
      SectData$Element <- rep(NA,nrow(SectData))
      SectData$ElementBlockNum <- rep(NA,nrow(SectData))

      #increment through the element dataframe and if else each row. Try to get the ifelse to not replace the cells if it fails the test. 
      for (i in 1:nrow(df.element)){
        elementmin_y<- as.numeric(df.element[i,5]) ## Do as numeric ahead outside of the for loop
        elementmax_y <- as.numeric(df.element[i,6])
        
        SectData$Element <-  ifelse(SectData$Thickness>elementmin_y & SectData$Thickness<=elementmax_y, df.element[i,3], SectData$Element)
        SectData$ElementBlockNum <- ifelse(SectData$Thickness>elementmin_y & SectData$Thickness<=elementmax_y, df.element[i,4], SectData$ElementBlockNum)
      } #end for loop
  
      SectData <- transform(SectData, Element = as.character(Element),
                              ElementBlockNum = as.numeric(ElementBlockNum)
      )
    } #end if nrow df.element > 0 
    
    ##Process the Element Set data
    print("Starting Element Set Data")
    df.elementset <- df.elementset %>% mutate(ymin_thick = round((fun.Thick(df.elementset$ymax_raw))/StratInc)*StratInc, ymax_thick = round((fun.Thick(df.elementset$ymin_raw))/StratInc)*StratInc)
    
    #if statment to process the element set data if df.elementset has any logged data.
    if(nrow(df.elementset)>0){
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
    }#end if(nrow(elementset)>0)
    print("Element Set Data Processed")

    ##Start bed/facies/element/elementset statistics <- although inelegant, these will be done through if(nrow(df.__)) rather than within the if statments higher up so that the summary statistics are appeneded at the end of the final SectData 
    print("Start statistics")
    
    #Bed Statistics
    df_bedsummarize <- group_by(SectData, BedNumber) %>% summarise(Thickness = max(Thickness), BedThick=n()*StratInc, MeanGS = mean(GS, na.rm=TRUE), MaxGS=max(GS, na.rm=TRUE)) %>% select(-BedNumber)
    SectData <- left_join(SectData,df_bedsummarize,by = "Thickness")
    
    #Facies stats
    if(nrow(df.facies)>0){
      df_facsummarize <-dplyr::group_by(SectData, FaciesBlockNum) %>% dplyr::summarise(Thickness = max(Thickness), FacThick=n()*(StratInc), FacMeanGS = mean(GS, na.rm=TRUE), FacNetGross=sum(SandMud)/n()) %>% select(-FaciesBlockNum)
      SectData <- left_join(SectData,df_facsummarize,by = "Thickness")
      print(df_facsummarize)
    }  
    #Element statistics
    if(nrow(df.element)>0){
      df_elementsummarize <-dplyr::group_by(SectData, ElementBlockNum) %>% dplyr::summarise(Thickness = max(Thickness), ElementThick=n()*(StratInc), ElementMeanGS = mean(GS, na.rm=TRUE)) %>% select(-ElementBlockNum)
      SectData <- left_join(SectData,df_elementsummarize,by = "Thickness")
    } 
    
    #Elementset statistics
    if(nrow(df.elementset)>0){
      df_elementsetsummarize <-dplyr::group_by(SectData, ElementSetBlockNum) %>% dplyr::summarise(Thickness = max(Thickness), ElementSetThick=n()*(StratInc), ElementSetMeanGS = mean(GS, na.rm=TRUE)) %>% select(-ElementSetBlockNum)
      SectData <- left_join(SectData,df_elementsetsummarize,by = "Thickness")
    }
    print("Done Statistics")
      
    ##Bind the SectData back into a reactive dataframe so it can be downloaded and drop the first row of it
    SectDataProcessed$df <-  bind_rows(SectDataProcessed$df,SectData)
    SectDataProcessed$df <-  SectDataProcessed$df[-1,]  
    
    #Send notifications  
    showNotification("Process Complete - Input stratigraphic data has been digitized", type = "message")
    showNotification("Data is ready to be downloaded", type = "message")

  }) # end of Process Points
    
  #Download SectDataProcessed$df when downlaod button is clicked
  output$downloadSectData <- downloadHandler(
     filename = function() {
       paste("ProcessedStratgraphicData", Sys.Date(), ".csv", sep="")
     },
     content = function(con) {
       write.csv(SectDataProcessed$df, con, na = "")
     }
  )
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #Server code related to integration of discrete measurements to discretized stratigraphic data
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
  
  #rendering the table for the .csv file of discrete measurements
  output$MeasurementContents <- renderTable({
    inMeasurementFile <- input$measurementfile
    
    if (is.null(inMeasurementFile))
      return(NULL)
    
    read.csv(inMeasurementFile$datapath, header = input$header)
  }) # end of MeasurementContents
  
  #joining the data based on rounded coordinates of the measurements
  observeEvent(input$ImportMeasurments_Discrete, {
    
    #Import the files  
    inMeasurementFile <- input$measurementfile
    MeasurementData <- read.csv(inMeasurementFile$datapath, header=input$header, sep = ",", stringsAsFactors=FALSE)
      
    inSectDataFile <- input$sectfile
    SectData <-   read.csv(inSectDataFile$datapath, header=input$header, sep = ",", stringsAsFactors=FALSE)

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
      
    #Join the raw measurement data into the SectData
    SectData <- left_join(SectData,MeasurementData, by = "Thickness")

    #Return the column names of the MeasurementData that aren't thickness so we can give the summarized data a proper name
    ColNames <- colnames(MeasurementData)
    ColNames <- ColNames[which(ColNames!="Thickness")] 
      
    #If bed is selected group by BedNumber then filter on !is.Na to then summarize different statistics on each of those groups: df_bedsummarize <- dplyr::group_by(SectData, BedNumber) %>% dplyr::summarise(Thickness = max(Thickness), BedThick=n()*0.1, MeanGS = mean(GS), MaxGS=max(GS))
    if(input$SummarizeMeasurementChoices == "summBed"){
      
      #Get the names of the columns that we'll need then remove them from the SectData
      BedStatCol <- c(ColNames,"BedNumber")
      BedStatJoin <- c("BedNumber","Thickness")
      BedSummarize  <-  SectData[,BedStatCol] # should use dplyr::select for this eventually
      BedStatJoin <- SectData[,BedStatJoin] # should use dplyr::select for this eventually
      
      #Group by BedNum Summarize_all (min, max, average, stdev)
      BedSummarizeStat <- BedSummarize %>% group_by(BedNumber) %>% summarise_all(.funs = c(min="min", max="max", mean="mean", sd="sd"))
      BedStatJoin <- BedStatJoin %>% group_by(BedNumber) %>% summarise(Thickness = max(Thickness))
        
      #join back to the sect data, first by joining the two summarized data together, then the summarized data to the SectData
      BedStatJoin <-left_join(BedStatJoin,BedSummarizeStat,by="BedNumber")
      BedStatJoin <- BedStatJoin[ , !(names(BedStatJoin) %in% c("BedNumber"))]
      SectData <- left_join(SectData,BedStatJoin, by="Thickness")
    } #End of bed statistics

    #If facies is selected group by FaciesBlockNum
      #Append the facies block num to the 
    
    #If element is selected group by ElementNum
    
    #If elementset is selected group by the ElementSetNum
      
      
    showNotification("Process Complete - Discrete measurements have been joined to the stratigraphic data", type = "message")
    showNotification("Merged data is ready to be downloaded", type = "message")

    #Bind the joined data to the reactive dataframe
    SectDataJoined$df <-  bind_rows(SectDataJoined$df,SectData)
    SectDataJoined$df <-  SectDataJoined$df[-1,]  
      
    print(nrow(SectData))
      
  }) #end of ImportMeasurements
    
  #Function for downloading data once it is merged
  output$downloadMergedMeasurementData <- downloadHandler(
    filename = function() {
      paste('MergedStratigraphicData-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(SectDataJoined$df, con, na = "")
    }
  ) #end of download
    
    
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #Server code related to the integration of continuous measurements to discretized stratigraphic data  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #rendering table for input continuous data .csv file
  output$MeasurementContents2 <- renderTable({
    inMeasurementFile2 <- input$measurementfile2
    
    if (is.null(inMeasurementFile2))
      return(NULL)
      
    read.csv(inMeasurementFile2$datapath, header = input$header2)
  }) # end of MeasurementContents
    
  #joining the data based on interpolated log profiles or averaged log profiles depending on the scale of the logs
  observeEvent(input$ImportMeasurments_Continuous, {
    
    inMeasurementFile2 <- input$measurementfile2
    MeasurementData <- read.csv(inMeasurementFile2$datapath, header = input$header2, sep = ",")
      
    inSectDataFile2 <- input$sectfile2
    SectData <-   read.csv(inSectDataFile2$datapath, header = input$header2, sep = ",")
      
    print("Ran Continuous")
      
    ColNames <- colnames(SectData)
    if(ColNames[1] == "X"){
      SectData <- SectData[,-1]
    }
      
    ThickCol <- input$ThickColumn2
    print(ThickCol)
    SectDataThickCol <- which(colnames(SectData)=="Thickness")
      
    #print(SectData)
    #print(MeasurementData)
    #print(ThickCol)
    #str(SectData)
      
    #Find the incriments of both datasets
    SectDataInc <-  SectData[3,SectDataThickCol] - SectData[2,SectDataThickCol]
    MeasurementInc <- MeasurementData[3,ThickCol] - MeasurementData[2,ThickCol]
        
    print(SectDataInc)
    print(MeasurementInc)
    
    #Create a sequnce for the new versions of each dataset to be interpolated on to
    Thickness <- seq(from=SectData[1,SectDataThickCol], to=SectData[nrow(SectData),SectDataThickCol], by = SectDataInc)
    print(Thickness)
      
    Continuous.df <- data.frame(Thickness)
    if(SectDataInc<MeasurementInc){  #test to see if we nee to interpolate data (SectDataInc<MeasurementDataInc) or average the data (SectDataInc>MeasurementDataInc)
    
      print("BigInc")
        
      for (i in  1:ncol(MeasurementData)){
        
        print(ncol(MeasurementData))
        print(MeasurementData)
          
        #in this loop we want to skip the depth/thickness column. So if i = thickcolumn then we'll skip it and move on to the next value of i
        if(i == ThickCol){
          print("Found Thick COl")
        } else {
          #Get the column names for the thickness as well as the 
          print("Not Thick Col")
          ColNames <- names(MeasurementData)
          DataName <- ColNames[i]
          ThickName <- ColNames[ThickCol]
            
          #create a function
          DataFun <-  approxfun(x=MeasurementData[[ThickName]],y=MeasurementData[[DataName]])
          print("Funciton Created")
            
          #propagate function
          ContData <- DataFun(Thickness)
          print("Function propegated")
            
          #join the data to the thickness
          Continuous.df <- data.frame(Continuous.df, ContData)
          
          #change the name from ContData back to the name of the column when imported
          colnames(Continuous.df)[colnames(Continuous.df)=="ContData"] <- ColNames[i]
            
        } # end of processing approx fun data processing
      } # end of for loop
        
    }else{
      #This is the case if the incriment of the joining data is smaller than the incriment of the section data. If this is the case we need to average the data then join it. 
      #we have to find what our window size needs to be for the rolling average - we'll just put this to the cloest integer
      AveWindow <- round(SectDataInc/MeasurementInc)
      print(AveWindow) 
      
      for (i in  1:ncol(MeasurementData)){
        #in this loop we want to skip the depth/thickness column. So if i = thickcolumn then we'll skip it and move on to the next value of i
        if(i == ThickCol){
          print("Found Thick COl")
        } else {
          #Get the column names for the thickness as well as the 
          print("Not Thick Col")
          ColNames <- names(MeasurementData)
          DataName <- ColNames[i]
          ThickName <- ColNames[ThickCol]
          
          #Running average of the data
          MeasurementData  <-  MeasurementData %>% mutate(RollAve = rollmean(x = MeasurementData[[DataName]], k=AveWindow,align="center",na.pad=TRUE))
            
          #print(MeasurementData)
            
          #create a function
          DataFun <-  approxfun(x=MeasurementData[[ThickName]],y=MeasurementData$RollAve)
          print("Funciton Created")
            
          #propagate function
          ContData <- DataFun(Thickness)
          print("Function propegated")
          
          #join the data to the thickness
          Continuous.df <- data.frame(Continuous.df, ContData)
            
          #change the name from ContData back to the name of the column when imported
          colnames(Continuous.df)[colnames(Continuous.df)=="ContData"] <- DataName
          
          str(Continuous.df)
        
        } # end of processing approx fun data processing
      } # end of for loop
    } # end of processing data

    
    SectData <-  left_join(SectData,Continuous.df, by = "Thickness")
      
    #-----NOTE--------End of coode for this section. Below is doubled from the section above and we'll canbalize it for bed, facies, and element statistics.

    #Statistics based upon user inputs of what level to do the statistics on: bed, facies, element, element set. Also what basic statistics to do at those levels: min, max, mean, standard deviation.
    #We need to get the names of the data that we'll be summarizing
    ColNames <- colnames(Continuous.df)
    ColNames <- ColNames[which(ColNames!="Thickness")] 
    print("Data Understood")
      
    #If bed is selected group by BedNumber.x then filter on !is.Na to then summarize different statistics on each of those groups: df_bedsummarize <- dplyr::group_by(SectData, BedNumber) %>% dplyr::summarise(Thickness = max(Thickness), BedThick=n()*0.1, MeanGS = mean(GS), MaxGS=max(GS))
    if(input$SummarizeMeasurementChoices2 == "summBed2"){
      print("Summarizing Beds")
      
      BedStatCol <- ColNames
      BedStatCol <- c(BedStatCol,"BedNumber.x")
      #print(BedStatCol)
      
      BedStatJoin <- c("BedNumber.x","Thickness")
      print ("Column Names Aquired")
        
      #Get the columns from the SectData that match the BedStatCol
      BedSummarize  <-  SectData[,BedStatCol]
      #print(BedSummarize)
      
      BedStatJoin <- SectData[,BedStatJoin]
      
      print ("Data Gathered")
        
      #Group by BedNum Summarize_all (min, max, average, stdev)
      BedSummarize <- BedSummarize %>% na.omit()# THIS FILTER ISNT WORKING
      #print(BedSummarize)
        
      BedSummarizeStat <- BedSummarize %>% group_by(BedNumber.x) %>% summarise_all(.funs = c(min="min", max="max", mean="mean", sd="sd")) # CURRENTLY DOESNT WORK. We get a ton of bullshit NA's in the summarize all table
      #print(BedSummarizeStat)
      
      BedStatJoin <- BedStatJoin %>% group_by(BedNumber.x) %>% summarise(Thickness = max(Thickness))
      #print(BedStatJoin)
        
      print ("Data Summarized")
        
      #join back to the stect data
        
      BedStatJoin <-left_join(BedStatJoin,BedSummarizeStat,by="BedNumber.x")
      BedStatJoin <- BedStatJoin[ , !(names(BedStatJoin) %in% c("BedNumber.x"))]
      SectData <- left_join(SectData,BedStatJoin, by="Thickness")
        
    }
      
    #print(SectData)
      
    #If facies is selected group by FaciesBlockNum
    #Append the facies block num to the 
      
    #If element is selected group by ElementNum
      
    #If elementset is selected group by the ElementSetNum
    
      
    showNotification("Process Complete - Discrete measurements have been joined to the stratigraphic data", type = "message")
    showNotification("Merged data is ready to be downloaded", type = "message")
      
    #Bind the joined data to the reactive dataframe
    SectDataJoined$df <-  bind_rows(SectDataJoined$df,SectData)
    SectDataJoined$df <-  SectDataJoined$df[-1,]  
      
      
  }) #end of ImportMeasurements
  #--------NOTE-----------End of code for canabalization
    
    
    
  #Function for downloading data once it is merged
  output$downloadMergedMeasurementData <- downloadHandler(
    filename = function() {
      paste('MergedStratigraphicData-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(SectDataJoined$df, con, na = "")
    }
  ) #end of download
    
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #Server code related to the creation of a stratigraphic library through appending multiple descretized stratigraphic sections  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #Rendering the table that is the selected file
  output$SectDataToImport <- renderTable(NumSectData$df)
  
  observeEvent(input$ImportSectionData,{
    #Read the .csv and bind all the SectData from that file to the total data keeping all columns
    TempSectFile <- read.csv(input$Sectfile$datapath, header = TRUE, sep = ",", stringsAsFactors=TRUE, na.strings = "NA")
    print(TempSectFile)
    TotalSectionData$df <-  bind_rows(TotalSectionData$df,TempSectFile)
    
    #Remove previous place holder
    if(TotalSectionData$df[1,1] == 0.1){
      TotalSectionData$df = TotalSectionData$df[-1,]
    }
      
    #Temporary printing
    TotalSectionData.df <- TotalSectionData$df
    print(TotalSectionData.df)
    #/Temp printing
      
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
      write.csv(TotalSectionData$df, con, na = "")
    }
  ) #end of download
}