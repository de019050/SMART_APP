library(shiny)
library(shinyMobile)
library(apexcharter)
library(scales)
library(shinyWidgets)

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
getwd()
rds_filename_org <- "~/R/SMART_APP/SMART_APP/Data/alldat_2024-01-03.rds"
alldat <- readRDS(rds_filename_org)

#================================
# Single data

ALL_REFLEX<-alldat%>%
  filter(str_detect(alldat$status,"r_f")==TRUE)%>%
  filter(!is.na(import_file))%>%
  filter(!is.na(aql_aql))%>%
  filter(!is.na(geraet))%>%    
  dplyr::select(-c(import_file1,ref_auftr_nr,auftr_typ,storno_grund,storno_datum,four_eyes_check,preisli_nr,preisli_vers,kuli_bez,anz_zyklen,prob_nr,zykl_nr,param_art,param_bez,untergrenzen,obergrenzen))%>%
  #filter(str_detect(alldat$status,"r_f")==TRUE)%>%
  #filter(charge != "2F026")%>%
  #filter(charge != "3F222")%>%
  pivot_wider(id_cols=c(status,frei_datum,auftr_nr,mat_nr,charge,met_d_nr,mat_bez,anzahl_muster),
              names_from = param_nr,
              values_from = e_wert)%>%
  mutate(order=row_number())

# 
#================================





#================================
# Batch data

Refelex_EDOs <- read_csv("~/R/SMART_APP/SMART_APP/Data/Reflex_EDO.csv")
workdays <- read_csv("~/R/SMART_APP/SMART_APP/Data/workdays.csv")
workdays<-workdays%>%
  mutate(measurmenttype=MESSGROESSE)%>%
  mutate(mean=MITTELWERT)%>%
  mutate(sd=STANDARDABWEICHUNG)%>%
  dplyr::select(measurmenttype,mean,sd)%>%
  filter(measurmenttype!='PNEUMATIC')

simstart_ALL_Reflex_Num<-ALL_REFLEX %>%
  group_by(mat_bez,charge) %>% 
  summarize(n=n(),mdose=mean(`R-P001`,na.rm=T), sdose=sd(`R-P001`,na.rm=T), mitime=mean(`R-P027`,na.rm=T),sitime=sd(`R-P027`,na.rm=T),midepth=mean(`R-P028`,na.rm=T),
            sidepth=sd(`R-P028`,na.rm=T), 
            mact=mean(`R-P026`,na.rm=T),sact=sd(`R-P026`,na.rm=T),mnpos=mean(`R-P029`,na.rm=T),snpos=sd(`R-P029`,na.rm=T))


simstart_ALL_Reflex_Num<-simstart_ALL_Reflex_Num%>%mutate(newn = ifelse(n>=497,110,ifelse(n<=310 & n<=320,58,60)))%>%mutate(NewN20=round(newn*(1+0.2^2),digits=0))  

simstart_ALL_Reflex_Num<-simstart_ALL_Reflex_Num%>%
  #added constnts from ISO
  mutate(kfac = ifelse(newn == 58, 2.592, ifelse(newn == 60, 2.5730, 2.449)))%>%
  mutate(pfac = ifelse(newn == 58, 0.003872, ifelse(newn == 60, 0.004150, 0.006602)))%>%
  mutate(fsfac = ifelse(newn == 58, 0.179, ifelse(newn == 60, 0.180, 0.180)))%>%
  # added speclimits from EDOs
  #first  one sided
  mutate(dose_lsl= ifelse(mat_bez == "FITUSIRAN", 0.5, ifelse(mat_bez == "MARS", 1.14, 2.0)))%>%
  mutate(itime_usl= ifelse(mat_bez == "FITUSIRAN", 15, ifelse(mat_bez == "MARS", 15, 15))) %>%
  mutate(npos_usl= ifelse(mat_bez == "FITUSIRAN", 4.5, ifelse(mat_bez == "MARS", 4.5, 4.6)))%>%
  # now 2-sided
  mutate(act_lsl= ifelse(mat_bez == "FITUSIRAN", 3, ifelse(mat_bez == "MARS", 3, 3)))%>% 
  mutate(act_usl= ifelse(mat_bez == "FITUSIRAN", 16, ifelse(mat_bez == "MARS", 16, 24)))%>%
  mutate(idepth_lsl= ifelse(mat_bez == "FITUSIRAN", 4, ifelse(mat_bez == "MARS", 4, 4)))%>% 
  mutate(idepth_usl= ifelse(mat_bez == "FITUSIRAN", 8, ifelse(mat_bez == "MARS", 8, 8)))

#================================




poll <- data.frame(
  answer = c("Yes", "No"),
  n = c(254, 238)
)





#================================


shinyApp(
  ui = f7Page(
    title = "SMART-GDPU-APP",
    f7TabLayout(
      panels = tagList(
        f7Panel(title = "Left Panel", side = "left", theme = "light", "Links Bla", effect = "cover"),
        f7Panel(title = "Right Panel", side = "right", theme = "dark", "Rechts Bla", effect = "cover",f7SingleLayout(
          navbar = f7Navbar(title = "Select EDOS"),
          f7Select(
            inputId = "variable",
            label = "Choose a variable:",
            choices = colnames(ALL_REFLEX)[-1],
            selected = "R-P001"
          ),
          tableOutput("data")
        ))
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
              selected = "dark",
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
              apexchartOutput("scatter")
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
                c("Dose Accuracy" = "R-P001",
                  "Actuation Force" = "R-P026",
                  "Injection Time" = "R-P027",
                  "Injection Depth" = "R-P028",
                  "Needle Cover" = "R-P029"),
                openIn = "sheet",
                multiple = TRUE
              ),
              tableOutput("data")
            )
          )
        )
      )
    )
  ),
  server = function(input, output, session) {
    
    # river plot
    dates <- reactive(seq.Date(Sys.Date() - 30, Sys.Date(), by = input$by))
    
    output$pie <- renderApexchart({
      apex(
        data = poll,
        type = "pie",
        mapping = aes(x = answer, y = n)
      )
    })
    
    output$scatter <- renderApexchart({
      apex(
        data = mtcars,
        type = "scatter",
        mapping = aes(
          x = wt,
          y = mpg,
          fill = cyl
        )
      )
    })
    
    
    # datatable
    output$data <- renderTable({
      ALL_REFLEX[, c("auftr_nr","mat_nr","charge","mat_bez", input$variable), drop = FALSE]
    }, rownames = TRUE)
    
    
    # send the theme to javascript
    observe({
      session$sendCustomMessage(
        type = "ui-tweak",
        message = list(os = input$theme, skin = input$color)
      )
    })
    
  }
)