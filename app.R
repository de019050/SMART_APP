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
#singledata <- "Data/simstart_ALL_Reflex_Num.rds"
#ALL_REFLEX <- readRDS(singledata)

#================================





#================================
# Batch data

batchdata <- "Data/simstart_ALL_Reflex_Num.rds"
ALL_REFLEX <- readRDS(batchdata)

#================================
# Anzahl der Batches
poll <-ALL_REFLEX|>ungroup()|>select(MAT_BEZ)|>
  reframe(
    across(where(is.factor), nlevels),
    n = n(), .by=MAT_BEZ
  )
#================================================
# Anzahl der Getesten AIs
n_tested <-ALL_REFLEX|>ungroup()|>select(CHARGENGROESSE,N,NEWN20)|>
  reframe(
    across(where(is.numeric), sum),
    n = n(),
      )

#================================================
# Get Mean and SD of first 10 batches for R_D_M

R_D_Ms <- ALL_REFLEX %>%
  filter(MAT_BEZ == 'R_D_M') %>%
  slice(1:5) %>%
  ungroup()|>
  summarise(across(where(is.numeric), mean, na.rm = TRUE))|>
  mutate(MAT_BEZ='R_D_M')

R_D_Js <- ALL_REFLEX |>
  filter(MAT_BEZ == 'R_D_J')|>
  ungroup()|>
  slice(19:29) |>
  summarise(across(where(is.numeric), mean))|>
  mutate(MAT_BEZ='R_D_J')

R_D_MJ_Means<-rbind(R_D_Ms,R_D_Js)




#================================================


#================================================



shinyApp(
  ui = f7Page(
    #title = "SMART-GDPU-APP",
    f7TabLayout(
      panels = tagList(
        f7Panel( side = "left", theme = "light", effect = "cover",
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
        f7Panel(side = "right", theme = "light", effect = "cover",
                f7SingleLayout(
                  navbar = f7Navbar(title = "Select Material"),
                    f7Select(
                      inputId = "variable1",
                      label = "Choose a variable:",
                      #choices = ALL_REFLEXs|>select(MAT_BEZ),
                      choices = c('R_D_J',  'R_D_M'),#  "R_F_M",  "R_SAR2")
                      #selected = 'R_D_M'
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
            )
          ),
            f7Card(
            title = "Tested AIs",
              f7Gauge(
                id = "mygauge",
                type = "semicircle",
                value = 100,
                borderColor = "#ee6b4d",
                borderWidth = 10,
                valueFontSize = 15,
                valueTextColor = "#ee6b4d",
                labelText = paste0("Total tested AIs: ",n_tested$N)
              ),
               f7Gauge(
                id = "mygauge2",
                type = "semicircle",
                value = round(n_tested$NEWN20/n_tested$N*100,digits = 0),
                borderColor = "#f6c243",
                borderWidth = 10,
                valueFontSize = 15,
                valueTextColor = "#f6c243",
                labelText =  paste0("Tested s-Method ",n_tested$NEWN20)
              ),
                f7Gauge(
                id = "mygauge3",
                type = "semicircle",
                value = round(n_tested$N*0.16/n_tested$N*100,digits = 0),
                borderColor = "#62d488",
                borderWidth = 10,
                valueFontSize = 15,
                valueTextColor = "#62d488",
                labelText = paste0("Tested sig-Method ",round(n_tested$N*0.16,digits = 0))
              )  
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
        mapping = aes(x = MAT_BEZ, y = n)
      )|>ax_title(text ='Produced Batches')
    })

    output$scatter <- renderApexchart({
      req(input$variable1)  # Stellt sicher, dass input$variable1 verfügbar ist
      apex(
        data = ALL_REFLEX|>filter(MAT_BEZ==input$variable1),
        type = "line",
        aes(
          x = CHARGE,
          y = mdose,
          fill = MAT_BEZ
          )
      )|>
          #ax_yaxis(min=as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(dose_lsl))-0.01)|>
          ax_title(text ='Dose Accuracy')|>
          add_point(x=ALL_REFLEX$CHARGE,y=ALL_REFLEX$mdose)|>
          ax_states(hover = list(filter = list(type = "darken" ) ))|>
          add_hline(value= as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(mdose)), color = "#62d488", dash = 2, label = NULL)
          #add_hline(value= as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(dose_lsl)), color = "#ee6b4d", dash = 3, label = NULL)
    })

    output$scatter2 <- renderApexchart({
      req(input$variable1)  # Stellt sicher, dass input$variable1 verfügbar ist
      apex(
        data = ALL_REFLEX|>filter(MAT_BEZ==input$variable1),
        type = "line",
        mapping = aes(
          x = CHARGE,
          y = mact,
          fill = MAT_BEZ
        )
      )|>
        ax_yaxis(min=as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(act_lsl))-0.01,max=as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(act_usl))+0.01)|>
        ax_title(text ='Actuation Force')|>
        add_point(x=ALL_REFLEX$CHARGE,y=ALL_REFLEX$mact)|>
        ax_states(hover = list(filter = list(type = "darken" ) ))|>
        add_hline(value= as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(mact)), color = "#62d488", dash = 2, label = NULL)#|>
        #add_hline(value= as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(act_lsl)), color = "#ee6b4d", dash = 3, label = NULL)|>
        #add_hline(value= as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(act_usl)), color = "#ee6b4d", dash = 3, label = NULL)
    })
    output$scatter3 <- renderApexchart({
      req(input$variable1)  # Stellt sicher, dass input$variable1 verfügbar ist
      apex(
        data = ALL_REFLEX|>filter(MAT_BEZ==input$variable1),
        type = "line",
        mapping = aes(
          x = CHARGE,
          y = mitime,
          fill = MAT_BEZ
        )
      )|>
        ax_yaxis(max=as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(itime_usl))+0.01)|>
        ax_title(text ='Injection Time')|>
        add_point(x=ALL_REFLEX$CHARGE,y=ALL_REFLEX$mitime)|>
        ax_states(hover = list(filter = list(type = "darken" ) ))|>
        add_hline(value= as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(mitime)), color = "#62d488", dash = 2, label = NULL)#|>
        #add_hline(value= as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(itime_lsl)), color = "#ee6b4d", dash = 3, label = NULL)|>
    })
    output$scatter4 <- renderApexchart({
      req(input$variable1)  # Stellt sicher, dass input$variable1 verfügbar ist
      apex(
        data = ALL_REFLEX|>filter(MAT_BEZ==input$variable1),
        type = "line",
        mapping = aes(
          x = CHARGE,
          y = midepth,
          fill = MAT_BEZ
        )
      )|>
        ax_yaxis(min=as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(idepth_lsl))-0.01,max=as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(idepth_usl))+0.01)|>
        ax_title(text ='Injection Depth')|>
        add_point(x=ALL_REFLEX$CHARGE,y=ALL_REFLEX$midepth)|>
        ax_states(hover = list(filter = list(type = "darken" ) ))|>
        add_hline(value= as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(midepth)), color = "#62d488", dash = 2, label = NULL)#|>
        #add_hline(value= as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(idepth_lsl)), color = "#ee6b4d", dash = 3, label = NULL)|>
        #add_hline(value= as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(idepth_usl)), color = "#ee6b4d", dash = 3, label = NULL)
    })
    output$scatter5 <- renderApexchart({
      req(input$variable1)  # Stellt sicher, dass input$variable1 verfügbar ist
      apex(
        data = ALL_REFLEX|>filter(MAT_BEZ==input$variable1),
        type = "line",
        mapping = aes(
          x = CHARGE,
          y = mnpos,
          fill = MAT_BEZ
        )
      )|>
        ax_yaxis(max=as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(npos_usl))+0.01)|>
        ax_title(text ='Needle Position')|>
        add_point(x=ALL_REFLEX$CHARGE,y=ALL_REFLEX$mnpos)|>
        ax_states(hover = list(filter = list(type = "darken" ) ))|>
        add_hline(value= as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(mnpos)), color = "#62d488", dash = 2, label = NULL)#|>
        #add_hline(value= as.numeric(R_D_MJ_Means|>filter(MAT_BEZ==input$variable1)|>select(itime_lsl)), color = "#ee6b4d", dash = 3, label = NULL)|>
    })
    
    
    # datatable
    output$data <- renderTable({
      ALL_REFLEX[, c("CHARGE","MAT_BEZ","NEWN20" ,input$variable), drop = FALSE]
    }, rownames = TRUE)
    
    #save
    output$download = downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(ALL_REFLEX[, c("CHARGE","MAT_BEZ","NEWN20" ,input$variable)], file)
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
