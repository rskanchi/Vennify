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
    saveVenn(setList, folder, label.size = label.size, lwd = lwd, setColors = setColors)
    
    if (!is.null(signVariables)){
        signList <- as.list(data[, signVariables])
        signList <- lapply(signList, function(x) x[!is.na(x)])
        
        setList_UP <- lapply(seq_along(setList), function(x) setList[[x]][signList[[x]] > 0])
        names(setList_UP) <- names(setList)
        saveVenn(setList_UP, folder, vennLabel = "UP", label.size = label.size, lwd = lwd, setColors = setColors)
        
        setList_DOWN <- lapply(seq_along(setList), function(x) setList[[x]][signList[[x]] < 0])
        names(setList_DOWN) <- names(setList)
        saveVenn(setList_DOWN, folder, vennLabel = "DOWN", label.size = label.size, lwd = lwd, setColors = setColors)
    }
}

saveVenn <- function(setList, folder, vennLabel = "ALL", alpha = 0.7, lwd = 2, label.size = 15, setColors = NULL, ...){
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
    print(
        plot(eulerData, quantities = list(fontsize = label.size), 
             edges = list(col = setColors, alpha = alpha, lwd = lwd),
             fills = list(fill = setColors, alpha = alpha),
             legend = list(fontsize = label.size, cex = 1.2), labels = list(fontsize = label.size), 
             main = list(label = vennLabel, fontsize = label.size, cex = 1.2))
    )
    dev.off()
    
    vennData <- gplots::venn(setList, intersections = TRUE, showSetLogicLabel = TRUE, show.plot = FALSE, simplify = TRUE)
    intersectionList <- as.list(attr(vennData, "intersection"))
    intersectionDF <- as.data.frame(stringi::stri_list2matrix(intersectionList), row.names = FALSE)
    colnames(intersectionDF) <- names(intersectionList)
    write.table(intersectionDF, 
                file = file.path(folder, paste0("overlap_elements_", vennLabel, ".csv")), 
                quote = FALSE, sep = ",", row.names = FALSE)
}

# function to create a dataframe from a list of vectors (of different lengths)
# shorter vectors will have trailing NAs in the dataframe

list2DF <- function(vList) {
  # maximum length of the vectors
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
# 3. Option to paste in gene lists directly via textboxes (no file upload needed)
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
                textInput("output_path", "Output Folder", value = "outputs/")
                
            ),
            
            conditionalPanel(
                condition = "input.inputMode == 'upload'",
                fileInput("datafile", "Upload CSV File", accept = ".csv"),
                uiOutput("set_select_ui"),
                uiOutput("sign_select_ui"),
                uiOutput("color_picker_ui")
            ),
            
            actionButton("plotBtn", "Generate Venn Diagrams")
        ),
        mainPanel(
            fluidRow(
                column(4, plotOutput("venn_all")),
                column(4, plotOutput("venn_up")),
                column(4, plotOutput("venn_down"))
            )
        )
    )
)

server <- function(input, output, session) {
observeEvent(input$plotBtn, {
    folder_path <- input$output_path
    if (!dir.exists(folder_path)) dir.create(folder_path, recursive = TRUE)
    
  # Grab pasted input
  set1 <- unlist(strsplit(input$set1, "\n"))
  set2 <- unlist(strsplit(input$set2, "\n"))
  set3 <- unlist(strsplit(input$set3, "\n"))
  
  # Clean up NAs/empties
  sets <- list(set1 = set1[set1 != ""], 
               set2 = set2[set2 != ""], 
               set3 = set3[set3 != ""])
  
  # Labels and colors
  setLabels <- c(input$label1, input$label2, input$label3)
  setColors <- c(input$color1, input$color2, input$color3)
  
  # Truncate empty sets (like if set3 not used)
  non_empty <- sapply(sets, function(x) length(x) > 0)
  sets <- sets[non_empty]
  setLabels <- setLabels[non_empty]
  setColors <- setColors[non_empty]
  
  # Create euler object for display
  eulerData <- eulerr::euler(sets)
  
  # Render to UI
  output$venn_all <- renderPlot({
    plot(eulerData,
         quantities = list(fontsize = 15),
         fills = list(fill = setColors),
         edges = list(col = setColors, lwd = 2),
         labels = list(fontsize = 15))
  })
  
  # Save files in output folder
  getVennDiagram(
    data = list2DF(sets), 
    setVariables = names(sets), 
    setLabels = setLabels, 
    setColors = setColors, 
    folder = folder_path
  )
})
 # end of observeEvent input$plotBtn
    
    
    data_reactive <- reactive({
        req(input$datafile)
        read.csv(input$datafile$datapath)
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
    
    output$color_picker_ui <- renderUI({
        req(input$set_cols)
        tagList(
            lapply(seq_along(input$set_cols), function(i) {
                colourpicker::colourInput(
                    inputId = paste0("color_", i),
                    label = paste0("Color for ", input$set_cols[i]),
                    value = "#0072B2"
                )
            })
        )
    }) # end of output$sign_select_ui
    
    
    observeEvent(input$generateBtn, {
        req(data_reactive())
        req(input$set_cols)
        
        set_data <- data_reactive()[, input$set_cols, drop = FALSE]
        sign_data <- if (!is.null(input$sign_cols)) data_reactive()[, input$sign_cols, drop = FALSE] else NULL
        
        # Grab selected colors dynamically
        set_colors <- sapply(seq_along(input$set_cols), function(i) input[[paste0("color_", i)]])
        
        # Call your core function
        getVennDiagram(
            data = data_reactive(),
            setVariables = input$set_cols,
            signVariables = if (!is.null(input$sign_cols)) input$sign_cols else NULL,
            setLabels = input$set_cols, # You can make this dynamic if you want custom labels
            setColors = set_colors,
            folder = "output"
        )
        
        # Render plot (assuming you want to show ALL only)
        output$venn_all <- renderPlot({
          sets_named <- as.list(set_data)
          names(sets_named) <- input$set_cols  # or use custom labels if desired
          
          eulerr::plot(eulerr::euler(sets_named),
                       quantities = list(fontsize = 15),
                       fills = list(fill = set_colors),
                       edges = list(col = set_colors, lwd = 2),
                       labels = list(fontsize = 15))        })
    }) # end of observeEvent input$generateBtn
    
    
    
} # end of server logic



shinyApp(ui, server)

