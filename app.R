library(shiny)

library(shinyMobile)

library(apexcharter)
library(scales)
library(shinyWidgets)

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
#================================
# Initial  data

# rds_filename_org <- "Data/alldat_2024-01-03.rds"
# alldat <- readRDS(rds_filename_org)

#================================
# Single data

#singledata <- "Data/ALL_REFLEX.rds"
#ALL_REFLEX <- readRDS(singledata)
singledata <- "Data/simstart_ALL_Reflex_Num.rds"
ALL_REFLEX <- readRDS(singledata)

#================================





#================================
# Batch data

batchdata <- "Data/simstart_ALL_Reflex_Num.rds"
simstart_ALL_Reflex_Num <- readRDS(batchdata)

#================================




poll <-ALL_REFLEX %>%
  summarise(
    across(where(is.factor), nlevels),
    n = n(), 
  )



#================================


shinyApp(
  ui = f7Page(
    title = "SMART-GDPU-APP",
    f7TabLayout(
      panels = tagList(
        f7Panel( side = "left", theme = "light", effect = "reveal",
                f7Card(
                    title = "About SMART-GDPU",
                    "This is a simple app with plain text,
                    and simple graphs to show the possibilities of 
                    a fast build Shiny App.",
                    footer = tagList(
                    f7Button(color = "blue", label = "My button"),
                    f7Badge("Badge", color = "green")
          ))
          ),
        f7Panel(title = "Right Panel", side = "right", theme = "light", "Rechts Bla", effect = "cover",
                f7SingleLayout(
                  navbar = f7Navbar(title = "Select EDOS"),
                    f7Select(
                      inputId = "variable1",
                      label = "Choose a variable:",
                      choices = colnames(ALL_REFLEX)[-1],
                      selected = "R-P001"
                      ),
                    tableOutput("data1")
                )
        )
      ),
      navbar = f7Navbar(
        title = "SMART-GDPU-APP",
        hairline = TRUE,
        shadow = TRUE,
        leftPanel = TRUE,
        rightPanel = TRUE
      ),
      f7Tabs(
        animated = TRUE,
        #swipeable = TRUE,
        f7Tab(
          tabName = "ProductionInfo",
          icon = f7Icon("folder"),
          active = TRUE,
          
          f7Flex(
            prettyRadioButtons(
              inputId = "theme",
              label = "Select a theme:",
              thick = TRUE,
              inline = TRUE,
              selected = "md",
              choices = c("ios", "md"),
              animation = "pulse",
              status = "info"
            ),
            
            prettyRadioButtons(
              inputId = "color",
              label = "Select a color:",
              thick = TRUE,
              inline = TRUE,
              selected = "light",
              choices = c("light", "dark"),
              animation = "pulse",
              status = "info"
            )
          ),
          
          tags$head(
            tags$script(
              'Shiny.addCustomMessageHandler("ui-tweak", function(message) {
                var os = message.os;
                var skin = message.skin;
                if (os === "md") {
                  $("html").addClass("md");
                  $("html").removeClass("ios");
                  $(".tab-link-highlight").show();
                } else if (os === "ios") {
                  $("html").addClass("ios");
                  $("html").removeClass("md");
                  $(".tab-link-highlight").hide();
                }

                if (skin === "dark") {
                 $(".view-main").addClass("theme-dark");
                } else {
                  $(".view-main").removeClass("theme-dark");
                }

               });
              '
            )
          ),
          
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "Production and Test Infos",
              apexchartOutput("pie")
              #apexchartOutput("rBar1"),
              
            )
          ),
          
          f7Shadow(
            intensity = 5,
            hover = TRUE,
            f7Gauge(
            id = "mygauge",
            type = "semicircle",
            value = 50,
            borderColor = "#2196f3",
            borderWidth = 10,
            valueFontSize = 41,
            valueTextColor = "#2196f3",
            labelText = "amount of something"
            )
          ),
          f7Gauge(
            id = "mygauge2",
            type = "semicircle",
            value = 30,
            borderColor = "#2196f3",
            borderWidth = 10,
            valueFontSize = 41,
            valueTextColor = "#2196f3",
            labelText = "amount of something"
          )
          
        ),
        f7Tab(
          tabName = "Tab2",
          icon = f7Icon("keyboard"),
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "Batch Info",
              apexchartOutput("scatter"),
              apexchartOutput("scatter2"),
              apexchartOutput("scatter3"),
              apexchartOutput("scatter4"),
              apexchartOutput("scatter5")
            )
            
          )
        ),
        f7Tab(
          tabName = "Tab3",
          icon = f7Icon("layers_alt"),
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "Data Info",
              f7SmartSelect(
                "variable",
                "Variables to show:",
                c("Dose Accuracy" = "mdose",
                  "Actuation Force" = "mact",
                  "Injection Time" = "mitime",
                  "Injection Depth" = "midepth",
                  "Needle Cover" = "mnpos"),
                openIn = "sheet",
                multiple = TRUE
              ),
              tableOutput("data"),
              f7DownloadButton("download","Download!")
            )
          )
        )
      )
    )
  ),
  server = function(input, output, session) {
    
    # river plot
    #dates <- reactive(seq.Date(Sys.Date() - 30, Sys.Date(), by = input$by))
    
    
    output$pie <- renderApexchart({
      apex(
        data = poll,
        type = "pie",
        mapping = aes(x = mat_bez, y = n)
      )|>ax_title(text ='Produced Batches')
    })

    # output$rBar1 <- renderApexchart({
    #   apex(
    #     data = poll|>filter(mat_bez=="JUPITER"),
    #     type = "radialBar",
    #     mapping = aes(x = mat_bez, y = n)
    #   )|>ax_plotOptions(
    #     radialBar = radialBar_opts(
    #       startAngle = -90,
    #       endAngle = 90)
    #   )|>ax_title(text ='Tested  Batches')
    # })
    
    
    
    
      output$scatter <- renderApexchart({
      apex(
        data = ALL_REFLEX,
        type = "line",
        aes(
          x = charge,
          y = mdose,
          fill = mat_bez
          )
      )|>ax_title(text ='Dose Accuracy')|>add_point(x=ALL_REFLEX$charge,y=ALL_REFLEX$mdose)|>ax_states(hover = list(filter = list(type = "darken" ) ))
    })
    output$scatter2 <- renderApexchart({
      apex(
        data = ALL_REFLEX,
        type = "line",
        mapping = aes(
          x = charge,
          y = mact,
          fill = mat_bez
        )
      )|>ax_title(text ='Actuation Force')|>add_point(x=ALL_REFLEX$charge,y=ALL_REFLEX$mact)|>ax_states(hover = list(filter = list(type = "darken" ) ))
    })
    output$scatter3 <- renderApexchart({
      apex(
        data = ALL_REFLEX,
        type = "line",
        mapping = aes(
          x = charge,
          y = mitime,
          fill = mat_bez
        )
      )|>ax_title(text ='Injection Time')|>add_point(x=ALL_REFLEX$charge,y=ALL_REFLEX$mitime)|>ax_states(hover = list(filter = list(type = "darken" ) ))
    })
    output$scatter4 <- renderApexchart({
      apex(
        data = ALL_REFLEX,
        type = "line",
        mapping = aes(
          x = charge,
          y = midepth,
          fill = mat_bez
        )
      )|>ax_title(text ='Injection Depth')|>add_point(x=ALL_REFLEX$charge,y=ALL_REFLEX$midepth)|>ax_states(hover = list(filter = list(type = "darken" ) ))
    })
    output$scatter5 <- renderApexchart({
      apex(
        data = ALL_REFLEX,
        type = "line",
        mapping = aes(
          x = charge,
          y = mnpos,
          fill = mat_bez
        )
      )|>ax_title(text ='Needle Position')|>add_point(x=ALL_REFLEX$charge,y=ALL_REFLEX$mnpos)|>ax_states(hover = list(filter = list(type = "darken" ) ))
    })
    
    
    # datatable
    output$data <- renderTable({
      ALL_REFLEX[, c("charge","mat_bez","NewN20" ,input$variable), drop = FALSE]
    }, rownames = TRUE)
    
    #save
    output$download = downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(ALL_REFLEX[, c("charge","mat_bez","NewN20" ,input$variable)], file)
      }
    )
    
    # send the theme to javascript
    observe({
      session$sendCustomMessage(
        type = "ui-tweak",
        message = list(os = input$theme, skin = input$color)
      )
    })
    
  }
)