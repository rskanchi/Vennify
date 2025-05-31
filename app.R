# Core function to create proportional Venn diagrams and save outputs
# Input: data with columns for sets, optional sign/direction columns (log2FC, etc.)
getVennDiagram <- function(data, setVariables, signVariables = NULL, setLabels = NULL, setColors = NULL,
                           folder,
                           alpha = 0.7, lwd = 2, label.size = 15, ...){
    library(eulerr)
    library(gplots)
    library(stringi)
    
    setList <- as.list(data[, setVariables])
    setList <- lapply(setList, function(x) unique(x[!is.na(x)]))
    if (!is.null(setLabels)) names(setList) <- setLabels
    plt_ALL <- saveVenn(setList, folder, label.size = label.size, lwd = lwd, setColors = setColors)
    
    if (!is.null(signVariables)){
        signList <- as.list(data[, signVariables])
        signList <- lapply(signList, function(x) x[!is.na(x)])
        
        setList_UP <- lapply(seq_along(setList), function(x) setList[[x]][signList[[x]] > 0])
        names(setList_UP) <- names(setList)
        plt_UP <- saveVenn(setList_UP, folder, vennLabel = "UP", label.size = label.size, lwd = lwd, setColors = setColors)
        
        setList_DOWN <- lapply(seq_along(setList), function(x) setList[[x]][signList[[x]] < 0])
        names(setList_DOWN) <- names(setList)
        plt_DOWN <- saveVenn(setList_DOWN, folder, vennLabel = "DOWN", label.size = label.size, lwd = lwd, setColors = setColors)
    } # end of if (!is.null(signVariables))
    
    # return dependin on whether signVariables were provided
    if (is.null(signVariables)) {return(plt_ALL)} else {
        return(list(ALL = plt_ALL, UP = plt_UP, DOWN = plt_DOWN))}
} # end of function getVennDiagram

saveVenn <- function(setList, folder, vennLabel = "", alpha = 0.7, lwd = 2, label.size = 15, setColors = NULL, ...){
  library(eulerr)
  library(gplots)
  library(stringi)
  
  if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
  nSets <- length(setList)
  if (is.null(setColors)){
    clrs <- c("#0072B2", "#E69F00", "#CC79A7", "#D55E00", "#009E73", "#56B4E9", "#F0E442", "#000000", "#999999")
    setColors <- clrs[1:nSets]
  }
  
  eulerData <- euler(setList)
  write.csv(data.frame(eulerData$original.values), 
            file = file.path(folder, paste0("overlap_frequencies_", vennLabel, ".csv")))
  pdf(file = file.path(folder, paste0("venn_", vennLabel, ".pdf")), width = 15, height = 15)
  par(mar = c(1,1,1,1))
  plt <- plot(eulerData, quantities = list(fontsize = label.size), 
              edges = list(col = setColors, alpha = alpha, lwd = lwd),
              fills = list(fill = setColors, alpha = alpha),
              legend = list(fontsize = label.size, cex = 1.2), labels = list(fontsize = label.size), 
              main = list(label = vennLabel, fontsize = label.size, cex = 1.2))
  
  print(plt)
  dev.off()
  # Save intersection data
  vennData <- gplots::venn(setList, intersections = TRUE, showSetLogicLabel = TRUE, show.plot = FALSE, simplify = TRUE)
  intersectionList <- as.list(attr(vennData, "intersection"))
  intersectionDF <- as.data.frame(stringi::stri_list2matrix(intersectionList), row.names = FALSE)
  colnames(intersectionDF) <- names(intersectionList)
  write.table(intersectionDF, 
              file = file.path(folder, paste0("overlap_elements_", vennLabel, ".csv")), 
              quote = FALSE, sep = ",", row.names = FALSE)
  return(plt)
} # end of function saveVenn

# function to create a dataframe from a list of vectors (of different lengths)
# shorter vectors will have trailing NAs in the dataframe

list2DF <- function(vList) {
  # number of elements in each vector of the list 
  max_length <- max(sapply(vList, length))
  
  # add NAs to shorter vectors
  vList_sameLen <- lapply(vList, function(vector) {
    c(vector, rep(NA, max_length - length(vector)))
  })
  
  # dataframe 
  df <- as.data.frame(do.call(cbind, vList_sameLen))
  return(df)
} # end of function list2DF


# Enhancements to be integrated into Shiny UI:
# 1. Minimal UI with sidebar (light mode)
# 2. ALL/UP/DOWN diagrams shown simultaneously (not in tabs)
# 3. Option to paste in gene lists directly via text boxes (no file upload needed)
# 4. UI elements for custom set labels and user-defined colors

# Shiny App UI and Server scaffold
library(shiny)
library(colourpicker)
library(eulerr)
library(gplots)
library(stringi)

ui <- fluidPage(
    titlePanel("Vennify: Visualize Overlaps. Simplify Insights."),
    sidebarLayout(
        sidebarPanel(
            radioButtons("inputMode", "Choose Input Method:",
                         choices = c("Paste Lists" = "paste", "Upload File" = "upload"),
                         selected = "paste", inline = TRUE),
            
            conditionalPanel(
                condition = "input.inputMode == 'paste'",
                
                h4("Set 1"),
                textAreaInput("set1", NULL, placeholder = "Paste values, one per line"),
                fluidRow(
                    column(6, textInput("label1", "Label", value = "Set A")),
                    column(6, colourInput("color1", "Color", value = "#0072B2"))
                ),
                
                h4("Set 2"),
                textAreaInput("set2", NULL, placeholder = "Paste values, one per line"),
                fluidRow(
                    column(6, textInput("label2", "Label", value = "Set B")),
                    column(6, colourInput("color2", "Color", value = "#E69F00"))
                ),
                
                h4("Set 3 (optional)"),
                textAreaInput("set3", NULL, placeholder = "Paste values, one per line"),
                fluidRow(
                    column(6, textInput("label3", "Label", value = "Set C")),
                    column(6, colourInput("color3", "Color", value = "#CC79A7"))
                ),
                textInput("output_path", "Output Folder", value = "output/"),
                actionButton("pasteplotBtn", "Generate Venn")
            ), # end of conditionalPanel for pasting lists
            
            conditionalPanel(
                condition = "input.inputMode == 'upload'",
                fileInput("datafile", "Upload CSV File", accept = ".csv"),
                uiOutput("set_select_ui"),
                uiOutput("sign_select_ui"),
                uiOutput("color_picker_ui_note"),
                uiOutput("color_picker_ui"),
                textInput("output_path", "Output Folder", value = "output/"),
                actionButton("uploadplotBtn", "Generate Venn")
            ) # end of conditionalPanel for uploading file 
            
        ), # end of sidebarPanel
        mainPanel(
          plotOutput("venn_all"),
          plotOutput("venn_up"),
          plotOutput("venn_down")
            # fluidRow(
            #     column(4, plotOutput("venn_all")),
            #     column(4, plotOutput("venn_up")),
            #     column(4, plotOutput("venn_down"))
            # ) 
        ) # end of mainPanel
    ) # end of sidebarLayout
) # end of ui definition

server <- function(input, output, session) {

  # PASTE lists
  # observeEvent for the plotting of pasted lists
  observeEvent(input$pasteplotBtn, {
    folder_path <- input$output_path
    if (!dir.exists(folder_path)) dir.create(folder_path, recursive = TRUE)
    
    # Collect pasted input
    set1 <- unlist(strsplit(input$set1, "\n")); set2 <- unlist(strsplit(input$set2, "\n")); set3 <- unlist(strsplit(input$set3, "\n"))
    
    # Remove NAs/empties
    sets <- list(set1 = unique(set1[set1 != ""]),
                 set2 = unique(set2[set2 != ""]),
                 set3 = unique(set3[set3 != ""]))
    
    # Labels and colors
    setLabels <- c(input$label1, input$label2, input$label3)
    setColors <- c(input$color1, input$color2, input$color3)
    
    # Truncate empty sets (like if set3 not used)
    non_empty <- sapply(sets, function(x) length(x) > 0)
    sets <- sets[non_empty]
    setLabels <- setLabels[non_empty]
    setColors <- setColors[non_empty]
    
    # Use the getVennDiagram function to create the Venn diagram
    plt <- getVennDiagram(data = list2DF(sets), 
                          setVariables = names(sets), setLabels = setLabels, setColors = setColors, 
                          folder = folder_path)
    
    # Render to UI
    output$venn_all <- renderPlot({plt})
  }) # end of observeEvent input$plotBtn
  
  # UPLOAD lists
  # Reactive expression to read the uploaded CSV file
  
  data_reactive <- reactive({
    req(input$datafile)
    as.data.frame(read.csv(input$datafile$datapath, header = TRUE, check.names = TRUE, 
                           stringsAsFactors = FALSE, row.names = NULL, fill = TRUE))
  }) # end of data_reactive
  
  observeEvent(data_reactive(), {
    updateSelectizeInput(session, "set_cols", choices = names(data_reactive()), server = TRUE)
    updateSelectizeInput(session, "sign_cols", choices = names(data_reactive()), server = TRUE)
  })
  
  output$set_select_ui <- renderUI({
    req(data_reactive())
    selectizeInput("set_cols", "Choose Set Columns", choices = names(data_reactive()), multiple = TRUE)
  })
  
  output$sign_select_ui <- renderUI({
    req(data_reactive())
    selectizeInput("sign_cols", "Choose Sign Columns (optional)", choices = names(data_reactive()), multiple = TRUE)
  })
  
  clrs <- c("#0072B2", "#E69F00", "#CC79A7", "#D55E00", "#009E73", "#56B4E9", "#F0E442", "#000000", "#999999")
  # Provide a note to specify hex code or choose a color (in case the default colors not suitable
  output$color_picker_ui_note <- renderUI({
    req(input$set_cols)
    tagList(
      h4("Choose Colors for Each Set"),
      p("You can specify colors using hex codes (e.g., #FF5733) or use the color picker.")
    )
  }) # end of output$color_picker_ui_note
  
  output$color_picker_ui <- renderUI({
    req(input$set_cols)
    tagList(
      lapply(seq_along(input$set_cols), function(i) {
        colourpicker::colourInput(
          inputId = paste0("color_", i),
          label = paste(input$set_cols[i], "color"),
          value = clrs[i %% length(clrs)]
        )
      })
    )
  }) # end of output$sign_select_ui
  
  # Observe the generate button to create the Venn diagram  
  observeEvent(input$uploadplotBtn, {
    print("Generating venn diagram")
    print(paste("Set columns are:", paste(input$set_cols, collapse = ", ")))
    print(paste("Sign columns are:", paste(if (!is.null(input$sign_cols)) input$sign_cols else NULL, collapse = ", ")))
  })
  
  observeEvent(input$uploadplotBtn, {
    req(data_reactive())
    req(input$set_cols)
    
    # Collect selected colors dynamically
    set_colors <- sapply(seq_along(input$set_cols), function(i) req(input[[paste0("color_", i)]]))
    print(paste("Set colors are:", paste(set_colors, collapse = ", ")))
    
    # create the data with set columns and sign columns (if any)
    # the data set should have all the set columns and the sign columns conditionally
    set_data <- data_reactive()[, input$set_cols, drop = FALSE]
    sign_data <- if (!is.null(input$sign_cols)) data_reactive()[, input$sign_cols, drop = FALSE] else NULL
    # Combine set and sign data into a single data frame
    
    combined_data <- if (is.null(sign_data)) {set_data} else {cbind(set_data, sign_data)}
    # print the first few lines of the combined_data
    print(head(combined_data))
    
    folder_path <- input$output_path
    if (!dir.exists(folder_path)) dir.create(folder_path, recursive = TRUE)

    # Render to UI
    plt <- getVennDiagram(data = combined_data,
                          setVariables = colnames(set_data), setLabels = colnames(set_data), setColors = set_colors, # custom labels??
                          signVariables = if (!is.null(sign_data)) colnames(sign_data) else NULL,
                          folder = folder_path)
    
    # Render plot 
    # depending on whether sign variables were specified, show ALL or ALL,UP,DOWN
    if (!is.null(sign_data)){
      output$venn_all <- renderPlot({plt$ALL})
      output$venn_up <- renderPlot({plt$UP})
      output$venn_down <- renderPlot({plt$DOWN})
    } else {
      output$venn_all <- renderPlot({plt})      
    }
  }) # end of observeEvent input$plotBtn
  
    
    
} # end of server logic



shinyApp(ui, server)

