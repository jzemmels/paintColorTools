#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# browser()

paintData <- read_csv("data/paintData.csv")
choiceList <- data.table::as.data.table(x = list("Color ID" = paintData$colorID))
# names(choiceList) <- ""

render <- c(
  "function(data, type, row){",
  "  if(type === 'display'){",
  "    var tag = '<img src=\"' + data + '.png\" width=\"100\"/>';",
  "    return tag;",
  "  } else {",
  "    return data;",
  "  }",
  "}"
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  shinybusy::add_busy_spinner(spin = "fading-circle"),
  # Application title
  navbarPage("Paint Color Tools",
             tabPanel(
               "Paint Color Look-Alikes",
               # Sidebar with a slider input for number of bins
               sidebarLayout(
                 sidebarPanel(width = 3,
                              # fluidRow(selectInput(inputId = "brandList",
                              #                      label = "Select Brands",
                              #                      choices = unique(paintData$brand),
                              #                      multiple = TRUE,
                              #                      selected = "behr")),
                              # selectizeInput(inputId = "color",
                              #             label = "Select a Color (ID: Brand-ColorName-HexCode)",
                              #             choices = "Please wait. Loading color choices..."),
                              selectInput(inputId = "color",
                                             label = "Select a Color (ID: Brand-ColorName-HexCode)",
                                             choices = c(choiceList),
                                          selected = sample(choiceList$`Color ID`,1)),
                              numericInput(inputId = "similarColorsNum",label = "Number of Colors Shown",value = 25,min = 1),
                              actionButton(inputId = "randomSelect",label = "I'm feeling lucky")
                              # fluidRow(conditionalPanel(condition = "input.filterType == 'Brand + Paint Name'",
                              #                           selectInput(inputId = "color",
                              #                                       label = "Select a Color",
                              #                                       choices = paintData %>%
                              #                                         filter(brand == "behr") %>%
                              #                                         pull(colorID) %>%
                              #                                         sample(1)),
                              #                           actionButton(inputId = "randomSelect",label = "I'm feeling lucky")),
                              #          conditionalPanel(condition = "input.filterType == 'RGB'",
                              #                           numericInput(inputId = "red",label = "Red value:",min = 0,max = 255,value = 241),
                              #                           numericInput(inputId = "green",label = "Green value:",min = 0,max = 255,value = 157),
                              #                           numericInput(inputId = "blue",label = "Blue value:",min = 0,max = 255,value = 183)))#,
                              # br(),
                              # br(),
                              # fluidRow(selectInput(inputId = "filterType",
                              #                      label = "Arrange colors based on:",
                              #                      choices = c("Brand + Paint Name","RGB")),
                              #          selectInput(inputId = "metric",
                              #                      label = "Distance metric",
                              #                      choices = c("Euclidean RGB","Euclidean L*a*b*")))
                              # numericInput(inputId = "numSelected",label = "Number of colors to show",value = 10,min = 1)
                 ),

                 # Show a plot of the generated distribution
                 mainPanel(width = 9,
                           plotOutput(outputId = "similarColorPlot",width = "1000",height = "1000")
                           # DT::dataTableOutput("colorDataTable")
                           )
               )
             ),
             tabPanel("Image to Palette",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     fileInput(inputId = "paletteImagePath",
                                                         label = "Choose an Image File",
                                                         multiple = FALSE,
                                                         accept = "image/*"),
                                     uiOutput("paletteFileInfo"),
                                     checkboxInput(inputId = "paletteResizeIm",label = "Resize the image first (to reduce computational time)",value = FALSE),
                                     conditionalPanel(condition = 'input.paletteResizeIm == 1',
                                                      numericInput(inputId = "paletteImWidth",label = "Image Width (pixels)",value = 1000),
                                                      checkboxInput(inputId = "paletteKeepRatio",label = "Keep aspect ratio",value = TRUE),
                                                      conditionalPanel(condition = "input.paletteKeepRatio == 0",numericInput(inputId = "paletteImHeight",label = "Image Height (pixels)",value = NA))),
                                     numericInput(inputId = "paletteSize",label = "Number of Colors in Palette",value = 6,min = 1),
                                     numericInput(inputId = "paletteMinPts",label = "DBSCAN minPts Parameter",value = 5,min = 1),
                                     numericInput(inputId = "paletteEps",label = "DBSCAN epsilon Parameter (0-1 scale)",value = .01,min = 0,max = 1),
                                     actionButton(inputId = "paletteExecute",label = "Calculate Palette")),
                        mainPanel(width = 9,
                                  column(width = 12,plotOutput(outputId = "paletteImagePlot"),
                                         # br(),
                                         plotOutput(outputId = "paletteColors",width = "1000",height = "1000"))))),
             tabPanel("Image Color Picker",
                      sidebarLayout(
                        sidebarPanel(width = 4,
                                     h5("If you haven't done so, upload an image on the previous tab."),
                                     h5("Hover (and pause) on a pixel to see color information below."),
                                     h5("Click on a pixel to see the closest paint colors."),
                                     h5("(Hint: Wait to click on a pixel until after the color information loads below)"),
                                     textOutput(outputId = "colorPickerHoverText")
                                     # fileInput(inputId = "colorPickerPath",
                                     #           label = "Choose an Image File",
                                     #           multiple = FALSE,
                                     #           accept = "image/*"),
                                     # uiOutput("paletteFileInfo")
                                     ),
                        mainPanel(width = 8,
                                  column(width = 12,
                                         plotOutput(outputId = "colorPickerPlot",
                                                    click = "colorPickerClick",
                                                    hover = "colorPickerHover"),
                                         plotOutput(outputId = "colorPickerSimilarColors"))))),
             # tabPanel("Lego-ize",
             #          sidebarLayout(
             #            sidebarPanel(width = 3,
             #                         # fileInput(inputId = "legoizePath",
             #                         #           label = "Choose an Image File",
             #                         #           multiple = FALSE,
             #                         #           accept = "image/*"),
             #                         # fileInput(inputId = "startImage",
             #                         #           label = "Choose an Image File",
             #                         #           multiple = FALSE,
             #                         #           accept = "image/*"),
             #                         uiOutput("fileInfo"),
             #                         checkboxInput(inputId = "resizeIm",label = "Resize the image first (to reduce computational time)",value = FALSE),
             #                         conditionalPanel(condition = 'input.resizeIm == 1',
             #                                          numericInput(inputId = "imWidth",label = "Image Width (pixels)",value = 1000),
             #                                          checkboxInput(inputId = "keepRatio",label = "Keep aspect ratio",value = TRUE),
             #                                          conditionalPanel(condition = "input.keepRatio == 0",numericInput(inputId = "imHeight",label = "Image Height (pixels)",value = NA))),
             #                         numericInput(inputId = "blockSize",label = "Block Size (e.g., 3 by 3 pixels)",value = 3,step = 1,min = 2,width = "200px"),
             #                         # sliderInput(inputId = "blockWidth",label = "Block Widths (pixels)",min = 2,max = 10,value = 3,step = 1,round = TRUE,ticks = FALSE),
             #                         # sliderInput(inputId = "blockHeight",label = "Block Heights (pixels)",min = 2,max = 10,value = 3,step = 1,round = TRUE,ticks = FALSE),
             #                         shiny::actionButton(inputId = "legoize",label = "Lego-ize!",icon = icon("cubes")),
             #                         shiny::downloadButton("downloadLegoImage",label = "Download Lego-ized Image")
             #            ),
             #
             #            # Show a plot of the generated distribution
             #            mainPanel(width = 9,
             #                      fluidRow(splitLayout(cellWidths = c("50%","50%"),
             #                                           plotOutput("originalImage"),
             #                                           plotOutput("processedImage"))
             #                      )
             #
             #            )
             #          ))
  ))

# Define server logic required to draw a histogram
server <- function(session,input, output) {

  ############################################# Color look-alikes tab

  # observeEvent(input$brandList,{
  #
  #   updateSelectInput(inputId = "color",
  #                     choices = paintData %>%
  #                       filter(brand %in% input$brandList) %>%
  #                       pull(colorID) %>%
  #                       unique())
  #
  # })

  # {
    # browser()
    # updateSelectizeInput(session = session,inputId = "color",
    #                      choices = choiceList,selected = sample(choiceList,1),
    #                    # choices = unique(paintData$colorID),
    #                    # selected = sample(paintData$colorID,1),
    #                    server = TRUE,
    #                    options = list(maxOptions = length(choiceList)))}

  output$similarColorPlot <- renderPlot({

    req(input$color)
    req(input$color != "Please wait. Loading color choices...")

    selectedColor <- paintData %>%
      filter(colorID == input$color)

    ret <- paintData %>%
      # filter(brand %in% input$brandList) %>%
      mutate(distanceToColor = sqrt((selectedColor$r - r)^2 + (selectedColor$g - g)^2 + (selectedColor$b - b)^2)) %>%
      arrange(distanceToColor) %>%
      slice(1:input$similarColorsNum) %>%
      select("imgPath",
             "brand","Color Name","distanceToColor","rgb","Hex","colorID") %>%
      mutate(pltNum = 1:nrow(.),
             label = paste0(brand,"\n",`Color Name`,"\n",Hex,"\n",rgb)) %>%
      ggplot(aes(x=1,y=1,fill=Hex)) +
      geom_tile(color = "black",size = .2) +
      geom_label(aes(label = label),fill = "white",alpha = .5) +
      facet_wrap(~ pltNum) +
      theme_void() +
      labs(title = "Similar Paint Colors") +
      theme(strip.text = element_blank(),
            plot.title = element_text(hjust = .5)) +
      coord_fixed(expand = FALSE) +
      scale_fill_identity()

    return(ret)

  })

  observeEvent(input$randomSelect,
               {

                 updateSelectizeInput(inputId = "color",selected = paintData %>%
                                     # filter(brand %in% input$brandList) %>%
                                     pull(colorID) %>%
                                     unique() %>%
                                     sample(1))

               })


  ######################################## Image Palette panel

  output$paletteFileInfo <- renderUI({

    req(input$paletteImagePath)

    fileName <- input$paletteImagePath

    knitr::plot_crop(fileName$datapath)

    ret <- magick::image_read(fileName$datapath) %>%
      imager::magick2cimg()

    return(paste0("The image is ",nrow(ret)," by ",ncol(ret)," pixels.\n"))

  })

  paletteImage <- shiny::reactiveValues()

  observeEvent(input$paletteImagePath,{

    output$paletteColors <- NULL

    req(input$paletteImagePath)

    fileName <- input$paletteImagePath

    ret <- magick::image_read(fileName$datapath) %>%
      imager::magick2cimg() %>%
      imager::mirror(axis = "x")

    if(input$resizeIm){

      newRow <- input$paletteImWidth

      if(!input$paletteKeepRatio){

        newCol <- input$paletteImHeight

      }
      else{
        newCol <- newRow*ncol(ret)/nrow(ret)
      }

      ret <- imager::resize(ret,
                            size_y = newCol,
                            size_x = newRow)

    }

    paletteImage$im <<- ret  %>%
      imager::mirror(axis = "y") %>%
      imager::mirror(axis = "x")


    im <- isolate(paletteImage$im)

    im <- im %>%
      as.data.frame() %>%
      filter(cc != 4) %>%
      arrange(x,y,cc) %>%
      mutate(channel = factor(cc,labels = c("R","G","B"))) %>%
      select(-cc) %>%
      pivot_wider(names_from = channel,values_from = value) %>%
      mutate(hex = rgb(R,G,B,maxColorValue = 1))

    uploadedImagePlot <- im %>%
      ggplot(aes(x=x,y=y,fill=hex)) +
      geom_raster() +
      scale_fill_identity() +
      coord_fixed(expand = FALSE) +
      theme_void() +
      theme(plot.margin = margin(0,0,0,0))

    paletteImage$plt <<- uploadedImagePlot

  })

  output$paletteImagePlot <- renderPlot({

    req(input$paletteImagePath)

    return(paletteImage$plt)

  })

  observeEvent(input$paletteExecute,{

    output$paletteColors <- renderPlot({

      req(input$paletteImagePath)

      im <- isolate(paletteImage$im)

      image <- im %>%
        as.data.frame() %>%
        filter(cc != 4) %>%
        arrange(x,y,cc) %>%
        mutate(channel = factor(cc,labels = c("R","G","B"))) %>%
        select(-cc) %>%
        pivot_wider(names_from = channel,values_from = value) %>%
        mutate(hex = rgb(R,G,B,maxColorValue = 1))

      imageClust <- image %>%
        mutate(clust = image %>%
                 select(R,G,B) %>%
                 dbscan::dbscan(eps = input$paletteEps,minPts = input$paletteMinPts)  %>%
                 .$cluster) %>%
        group_by(clust) %>%
        mutate(clustR = mean(R),
               clustG = mean(G),
               clustB = mean(B)) %>%
        ungroup() %>%
        mutate(clustHex = rgb(clustR,clustG,clustB,maxColorValue = 1))

      # browser()



      pltDat <- imageClust %>%
        group_by(clustHex) %>%
        summarise(n = n()) %>%
        top_n(n = input$paletteSize,wt = n) %>%
        mutate(colRgb = map_chr(clustHex,~ {
          paste0(c(col2rgb(.)),collapse = ", ")
        }),
        label = paste0(clustHex,"\n",colRgb)) %>%
        tidyr::separate(col = colRgb,into = c("R","G","B"),sep = ", ",remove = FALSE) %>%
        mutate(R = as.integer(R),G = as.integer(G),B = as.integer(B)) %>%
        select(clustHex,label,R,G,B) %>%
        mutate(Hex = clustHex)

      similarColors <- pltDat %>%
        pmap_dfr(~ {

          paintData %>%
            mutate(distanceToColor = (r - ..3)^2 + (g - ..4)^2 + (b - ..5)^2) %>%
            arrange(distanceToColor) %>%
            slice(1:3) %>%
            mutate(label = paste0(brand,"\n",`Color Name`,"\n",Hex,"\n",rgb),
                   clustHex = ..1) %>%
            select(Hex,label,clustHex)

        })

      ret <-  pltDat %>%
        mutate(rowNum = 1) %>%
        ggplot(aes(x = 1,y = 1,fill=Hex)) +
        geom_tile(color = "black",size = .2) +
        geom_label(aes(label = label),fill = "white",alpha = .5) +
        facet_grid(rows = vars(rowNum),
                   cols = vars(clustHex)) +
        theme_void() +
        labs(title = "Image Color Palette") +
        theme(strip.text = element_blank(),
              plot.title = element_text(hjust = .5,margin = margin(0,0,-300,0)),
              # plot.title = element_blank(),
              plot.margin=grid::unit(c(-500,0,-1000,0), "mm")) +
        coord_fixed(expand = FALSE) +#,ratio = 1) +
        scale_fill_identity()

      ret1 <- similarColors %>%
        mutate(rowNum = rep(2:4,times = nrow(.)/3)) %>%
        ggplot(aes(x = 1,y = 1,fill=Hex)) +
        geom_tile(color = "black",size = .2) +
        geom_label(aes(label = label),fill = "white",alpha = .5) +
        facet_grid(rows = vars(rowNum),
                   cols = vars(clustHex)) +
        theme_void() +
        labs(title = "Closest Paint Brand Colors") +
        theme(strip.text = element_blank(),
              plot.title = element_text(hjust = .5,margin = margin(-1000,0,-950,0)),
              # plot.title = element_blank(),
              plot.margin=grid::unit(c(-1000,0,0,0), "mm")) +
        coord_fixed(expand = FALSE) +#,ratio = 1) +
        scale_fill_identity()

      library(patchwork)

      return(ret / ret1)

    })

  })


  ######################################## Color Picker panel

  output$colorPickerPlot <- renderPlot({

    req(input$paletteImagePath)

    return(paletteImage$plt)

  })

  observeEvent(input$colorPickerHover,{

    req(input$paletteImagePath)
    req(input$colorPickerHover)

    im <- isolate(paletteImage$im) %>%
      as.data.frame() %>%
      filter(cc != 4) %>%
      arrange(x,y,cc) %>%
      mutate(channel = factor(cc,labels = c("R","G","B"))) %>%
      select(-cc) %>%
      pivot_wider(names_from = channel,values_from = value) %>%
      mutate(hex = rgb(R,G,B,maxColorValue = 1))


    output$colorPickerHoverText <- renderText({

      if(is.null(input$colorPickerHover)){

        return(paste0("Row:  Col:  Hex Code: RGB Value: "))

      }

      ret <- im %>%
        mutate(distanceToHover = (x - input$colorPickerHover$x)^2 + (y - input$colorPickerHover$y)^2) %>%
        arrange(distanceToHover) %>%
        slice(1)

      return(paste0("(Row, Col): (",ret$x,", ",ret$y,"), Hex Code: ",ret$hex,", RGB Value: (",round(ret$R*255,2),", ",round(ret$G*255,2),", ",round(ret$B*255,2),")"))

    })

  })

  observeEvent(input$colorPickerClick,{

    req(input$paletteImagePath)
    req(input$colorPickerHover)

    im <- isolate(paletteImage$im) %>%
      as.data.frame() %>%
      filter(cc != 4) %>%
      mutate(channel = factor(cc,labels = c("R","G","B"))) %>%
      select(-cc) %>%
      pivot_wider(names_from = channel,values_from = value) %>%
      mutate(distanceToHover = (x - input$colorPickerHover$x)^2 + (y - input$colorPickerHover$y)^2) %>%
      arrange(distanceToHover) %>%
      slice(1) %>%
      mutate(hex = rgb(R,G,B,maxColorValue = 1),
             label = paste0(hex,"\n",round(R*255),", ",round(G*255),", ",round(B*255)))

    similarColors <- paintData %>%
      mutate(distanceToColor = (r - im$R*255)^2 + (g - im$G*255)^2 + (b - im$B*255)^2) %>%
      arrange(distanceToColor) %>%
      slice(1:5) %>%
      mutate(label = paste0(brand,"\n",`Color Name`,"\n",Hex,"\n",rgb)) %>%
      select(Hex,label) %>%
      mutate(colNum = 1:nrow(.))

    output$colorPickerSimilarColors <- renderPlot({

      plt1 <- im %>%
        ggplot(aes(x=1,y=1,fill=hex)) +
        geom_tile(color = "black",size = .2) +
        geom_label(aes(label = label),fill = "white",alpha = .5) +
        theme_void() +
        labs(title = "Selected Color") +
        theme(strip.text = element_blank(),
              plot.title = element_text(hjust = .5)) +
        coord_fixed(expand = FALSE) +
        scale_fill_identity()

      plt2 <- similarColors %>%
        ggplot(aes(x=1,y=1,fill=Hex)) +
        geom_tile(color = "black",size = .2) +
        geom_label(aes(label = label),fill = "white",alpha = .5) +
        theme_void() +
        facet_wrap(~ colNum,nrow = 1) +
        labs(title = "Similar Paint Colors") +
        theme(strip.text = element_blank(),
              plot.title = element_text(hjust = .5)) +
        coord_fixed(expand = FALSE) +
        scale_fill_identity()

      library(patchwork)

      return((plt1 | plt2) +
               plot_layout(widths = c(.25,1)))

    })

  })

  ######################################## Lego-ize panel

  # output$fileInfo <- renderUI({
  #
  #   req(input$paletteImagePath)
  #
  #   fileName <- input$paletteImagePath
  #
  #   ret <- magick::image_read(fileName$datapath) %>%
  #     imager::magick2cimg()
  #
  #   return(paste0("The image is ",nrow(ret)," by ",ncol(ret)," pixels.\n"))
  #
  # })
  #
  #
  # regImage <- shiny::reactive({
  #
  #   req(input$paletteImagePath)
  #
  #   fileName <- input$paletteImagePath
  #
  #   ret <- magick::image_read(fileName$datapath) %>%
  #     imager::magick2cimg() %>%
  #     imager::mirror(axis = "x")
  #
  #   if(input$resizeIm){
  #
  #     newRow <- input$imWidth
  #
  #     if(!input$keepRatio){
  #
  #       newCol <- input$imHeight
  #
  #     }
  #     else{
  #       newCol <- newRow*ncol(ret)/nrow(ret)
  #     }
  #
  #     ret <- imager::resize(ret,
  #                           size_y = newCol,
  #                           size_x = newRow)
  #
  #   }
  #
  #   return(ret)
  #
  # })
  #
  # observeEvent(regImage(),{
  #   output$fileInfo <- renderUI({
  #
  #     ret <- isolate(regImage())
  #
  #     return(paste0("The image is ",nrow(ret)," by ",ncol(ret)," pixels.\n"))
  #
  #   })
  # })
  #
  # output$originalImage <- renderPlot({
  #
  #   req(input$paletteImagePath)
  #
  #   return(paletteImage$plt)
  #
  # })
  #
  # observeEvent(input$paletteImagePath,{
  #
  #   output$processedImage <- NULL
  #
  # })
  #
  # observeEvent(input$legoize,{
  #
  #   regImg <- isolate(regImage())
  #
  #   output$processedImage <- renderPlot({
  #
  #     regImageList <- regImg %>%
  #       imager::grayscale() %>%
  #       imager::imsplit(axis = "x",nb = nrow(.)/input$blockSize) %>%
  #       # imager::imsplit(axis = "x",nb = nrow(.)/input$blockHeight) %>%
  #       as.list() %>%
  #       map(~ {
  #
  #         imager::imsplit(.,axis = "y",nb = ncol(.)/input$blockSize)
  #         # imager::imsplit(.,axis = "y",nb = ncol(.)/input$blockWidth)
  #
  #       })
  #
  #     withProgress(message = "Lego-izing",
  #                  value = 0,
  #                  expr = {
  #
  #                    plt <- data.frame(rowStart = regImageList%>%
  #                                        map(function(dat){
  #
  #                                          dat %>%
  #                                            names() %>%
  #                                            str_extract_all("[0-9]{1,}") %>%
  #                                            map_int(~ {
  #
  #                                              as.integer(.[1])
  #
  #                                            })
  #
  #                                        }) %>%
  #                                        unlist(),
  #                                      rowEnd = regImageList %>%
  #                                        map(function(dat){
  #
  #                                          dat %>%
  #                                            names() %>%
  #                                            str_extract_all("[0-9]{1,}") %>%
  #                                            map_int(~ {
  #
  #                                              as.integer(.[2])
  #
  #                                            })
  #
  #                                        }) %>%
  #                                        unlist()) %>%
  #                      rownames_to_column() %>%
  #                      mutate(colStart = map_int(rowname,~ as.integer(str_extract_all(.,"[0-9]{1,}")[[1]][1])),
  #                             colEnd = colStart + (unique(colStart)[2] - unique(colStart)[1]),
  #                             colEnd = ifelse(colStart == max(colStart),nrow(regImg),colEnd),
  #                             rowEnd = rowEnd + 1) %>%
  #                      mutate(regImageSplit = flatten(regImageList),
  #                             regImageAve = map_dbl(regImageSplit,~ {mean(as.matrix(.))}),
  #                             regImageAve = floor(regImageAve*10)/10) %>%
  #                      ggplot(aes(xmin = colStart,xmax = colEnd,ymin = rowStart,ymax = rowEnd,fill = regImageAve,colour = regImageAve)) +
  #                      geom_rect() +
  #                      scale_fill_gradientn(colours = c("gray0","gray10","gray20","gray30","gray40",
  #                                                       "gray50",
  #                                                       "gray60","gray70","#aec6cf","gray90","gray100"),
  #                                           values = seq(0,1,by = .1),aesthetics = c("fill","colour")) +
  #                      coord_fixed(expand = FALSE) +
  #                      scale_y_reverse() +
  #                      theme_void() +
  #                      theme(legend.position = "none") +
  #                      scale_x_reverse()
  #
  #                    incProgress(amount = 1)
  #
  #                    processedIm <<- plt
  #
  #                    return(plt)
  #
  #                  })
  #
  #   })
  #
  # })
  #
  # output$downloadLegoImage <- downloadHandler(filename = function(){
  #   paste0("legoizedImage-",Sys.Date(),".png",sep = "")
  # },
  # content = function(file){
  #   ggplot2::ggsave(processedIm,filename = file)
  #   knitr::plot_crop(file,quiet = TRUE)
  # })
}

# Run the application
shinyApp(ui = ui, server = server)
