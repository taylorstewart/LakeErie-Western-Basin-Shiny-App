##############################################################
##############################################################
##
##  LEBS Shiny Application
##
##  SERVER SCRIPT (Functions that build the UI)
##
##############################################################
##############################################################
## -----------------------------------------------------------
## Define server logic required (function will end on the last line of code)
## -----------------------------------------------------------
shinyServer(function(input, output) {

  ## Reactive species input for labels
  tbl_species <- reactive({
    distinct(filter(lw,species == input$species),species)
  })
  
  ## Reactive year input for labels
  tbl_year <- reactive({
    distinct(filter(lw,year == input$year),year)
  })
  
  ## Reactive year input for labels
  tbl_year2 <- reactive({
    distinct(filter(lw,year == input$year2),year)
  })
  
  ## Reactive year input for labels
  tbl_year3 <- reactive({
    distinct(filter(lw,year == input$year3),year)
  })
  
  ## Reactive life stage input for labels
  tbl_ls <- reactive({
    ls <- distinct(filter(catch,life.stage == input$life.stage),life.stage)
    ls <- as.character(ls$life.stage)
    ls <- paste(gsub("_","-",ls),collapse=" ")
  })
  
  ## Reactive species input for CPE plot label
  catch_species <- reactive({
    distinct(filter(lw,species == input$species2),species)
  })
  
  ## Reactive life stage input for CPE plot label
  catch_ls <- reactive({
    ls_vars <- distinct(filter(catch,species == input$species,year == input$year,season == input$season,n.per.ha > 0,kg.per.ha > 0,life.stage != "ALL"),life.stage)%>%
      select(life.stage) %>%
      arrange(life.stage)
    ls_vars <- as.character(ls_vars$life.stage)
    if(length(ls_vars) > 0) {
      ls_vars <- paste(gsub("_"," ",ls_vars),collapse=", ")
      ls_vars <- paste0("All Life Stages, ",ls_vars) } else {
        ls_vars <- paste0("All Life Stages",ls_vars)
      }
    ls_vars <- data_frame(vars=ls_vars)
  })
  
  ## Reactive parameter input for water quality table label
  tbl_par <- reactive({
    distinct(filter(wb_wq,parameter == input$parameter),parameter)
  })
  
  ## Reactive life stage input for label
  map_ls <- reactive({
    ls_vars <- distinct(filter(catch,species == input$species,year == input$year,season == input$season,n.per.ha > 0,kg.per.ha > 0,life.stage != "ALL"),life.stage)%>%
      select(life.stage) %>%
      arrange(life.stage)
    ls_vars <- as.character(ls_vars$life.stage)
    if(length(ls_vars) > 0) {
      ls_vars <- paste(gsub("_"," ",ls_vars),collapse=", ")
      ls_vars <- paste0("All Life Stages, ",ls_vars) } else {
        ls_vars <- paste0("All Life Stages",ls_vars)
      }
    ls_vars <- data_frame(vars=ls_vars)
  })
  
  ## Reactive value input for spatial map label
  map_value <- renderText({
    if(input$density == "n.per.ha") {
      "density" } else {
        "biomass"
      }
  })
  
## -----------------------------------------------------------
## Historical Time Series Data Manipulation and Plot
## -----------------------------------------------------------

  ## Filter catch, returning a data frame
  time_data <- reactive({
    
    l <- select(catch,species,life.stage,Year=year,Season=season,n.per.ha,kg.per.ha) %>% 
    ## filter by species
    filter(species == input$species2)
    
    ## Optional: filter by life stage
    if (!is.null(input$life.stage2) && input$life.stage2 != "All Life Stages") {
      l %<>% filter(life.stage == input$life.stage2)
    }

    l %<>% group_by(Year,Season) %>%
      summarise(density=round(mean(n.per.ha),2),
                biomass=round(mean(kg.per.ha),2)) %>%
      tbl_df() %>% 
        arrange(Year)
  })
  
  ## Filter catch, returning the means
  time_mean <- reactive({
    
    l <- catch %>% select(species,life.stage,Year=year,Season=season,n.per.ha,kg.per.ha) %>%
      arrange(Year) %>% 
    ## filter by species
    filter(species == input$species2)
    
    ## Optional: filter by life stage
    if (!is.null(input$life.stage2) && input$life.stage2 != "All Life Stages") {
      l %<>% filter(life.stage == input$life.stage2)
    }
      ## Determines length (number of years)
      l_rep <- l$Year %>% n_distinct()
      l_seas_year <- l %>% distinct(Season,Year)

      ## Calculate long-term mean
      l <- time_data() %>% group_by(Season) %>%
        summarise(density=round(mean(density),2),
                  biomass=round(mean(biomass),2)) %>%
        tbl_df()
    
      ## Repeat mean for all years
      l_mean_rep <- as.data.frame(do.call(cbind,lapply(l,rep,l_rep))) %>%
        select(density,biomass)
      l_mean_rep <- bind_cols(l_mean_rep,l_seas_year)
  })

  ## Function for generating historical abundance tooltip text
  tooltip <- function(x) {
    wb <- isolate(time_data())
    time_data <- wb[wb$Season == x$Season,] %>% 
      arrange(desc(Year))
    ## paste text string
    paste0("<b>",time_data$Year," Density: ",time_data$density," (N/ha)","<br>")
  }
  
  ## Function for generating historical abundance tooltip text
  tooltip1 <- function(x1) {
    wb <- isolate(time_data())
    time_data <- wb[wb$Season == x1$Season,] %>% 
      arrange(desc(Year))
    ## paste text string
    paste0("<b>",time_data$Year," Biomass: ",time_data$biomass," (Kg/ha)","<br>")
  }
  
  # ## Reactive label for life stages
  # output$catch_ls_label <- renderText({
  #   HTML(paste0("<p><h5>","Available Life Stages for ",catch_species()$species,":","</h5></p>","(",map_ls()$vars,")")
  #   )
  # })
  

  ## A reactive expression with the historical density time series plot
  reactive({

    ggvis(time_data(),~factor(Year),~density) %>%
      group_by(Season) %>%
      layer_points(fill = ~Season,prop("size",80)) %>%
      layer_lines(stroke = ~Season,prop("strokeWidth",2)) %>%
      layer_paths(data=filter(time_mean,Season=="Spring"),~factor(Year),~density,strokeDash:=6,stroke:="#1C6CAB",prop("strokeWidth",1)) %>% 
      layer_paths(data=filter(time_mean,Season=="Autumn"),~factor(Year),~density,strokeDash:=6,stroke:="#FF7311",prop("strokeWidth",1)) %>%
      add_tooltip(tooltip,"hover") %>%
      scale_numeric("y",domain=c(0,NA)) %>%
      add_axis("x",title="Year",ticks=1,title_offset=35,properties = axis_props(
        title=list(fontSize=16),
        labels=list(fontSize=13))) %>%
      add_axis("y",title="Mean Number per Hectare Swept",title_offset=55,properties = axis_props(
        title=list(fontSize=16),
        labels=list(fontSize=13))) }) %>% 
  ## Bind reactive plot to a shiny output
  bind_shiny("time_n","ggvis_kg_time")
  
  ## A reactive expression with the historical biomass time series plot
  reactive({
    
    ggvis(time_data(),~factor(Year),~biomass) %>%
      group_by(Season) %>%
      layer_points(fill = ~Season,prop("size",80)) %>%
      layer_lines(stroke = ~Season,prop("strokeWidth",2)) %>%
      layer_paths(data=filter(time_mean,Season=="Spring"),~factor(Year),~biomass,strokeDash:=6,stroke:="#1C6CAB",prop("strokeWidth",1)) %>% 
      layer_paths(data=filter(time_mean,Season=="Autumn"),~factor(Year),~biomass,strokeDash:=6,stroke:="#FF7311",prop("strokeWidth",1)) %>%
      add_tooltip(tooltip1,"hover") %>%
      scale_numeric("y",domain=c(0,NA)) %>%
      add_axis("x",title="Year",ticks=1,title_offset=35,properties = axis_props(
        title=list(fontSize=16),
        labels=list(fontSize=13))) %>%
      add_axis("y",title="Mean Kilogram per Hectare Swept",title_offset=55,properties = axis_props(
        title=list(fontSize=16),
        labels=list(fontSize=13))) }) %>% 
  ## Bind reactive plot to a shiny output
  bind_shiny("time_kg","ggvis_kg_time")
  
  ## Reactive CPE plot label
  output$catch_label <- renderText({
    HTML(paste("Mean density (N/Ha; top) and biomass (Kg/ha; bottom) of",tags$b(catch_species()$species),
               "by season collected in Ontario, Michigan, and Ohio waters in the western basin of Lake Erie. Dashed lines indicate long-term seasonal means. Note: sampling was not conducted in spring 2018."
    ))
  })
  
  ## Download plot data
  output$downloadCSV_1 <- downloadHandler(
    filename=reactive(paste(if(input$life.stage2 != "All Life Stages") {catch_ls()} else {"All_Life_Stages"},
                            catch_species()$species,"Historical_Abundance_Data",sep="_")),
    content=function(file) {
      write.csv(time_data(),file,row.names=FALSE)
    },
    contentType="text/csv"
  )
  
## -----------------------------------------------------------
## Ranked Catch Data Manipulation and Plot
## -----------------------------------------------------------
  ## Filter density and biomass, returning a data frame
  bar_data <- reactive({
    
    w <- catch %>% 
    ## filter by year
    filter(year == input$year2) %>% 
    ## filter by season
    filter(season == input$season2)
    
    ## Summarize by species
    w_rank <- w %>% group_by(species) %>% 
      summarize(n.per.ha=sum(n.per.ha))
    ## rank in descending order
    w_rank$rank <- dense_rank(desc(w_rank$n.per.ha))
    ## Filter top 10
    w_rank %<>% filter(rank %in% 1:10) %>%
      select(species)
    
    ## Filter top 10 species
    w %<>% filter(species %in% w_rank$species) %>% 
    ## Summarize density and biomass values
      group_by(species) %>%
      summarise(n.per.ha=mean(n.per.ha),
                kg.per.ha=mean(kg.per.ha))
  })

  ## A reactive expression with the density bar plot
  reactive({
    
    ggvis(bar_data(),~factor(species),~n.per.ha) %>% 
      layer_bars() %>% 
      add_axis("x",title="",ticks=0,properties = axis_props(
        labels = list(angle = 45,align = "left",fontSize=14))) %>%
      add_axis("y",title="Mean Number per Hectare Swept",title_offset=55,properties = axis_props(
        title=list(fontSize=16),
        labels=list(fontSize=13)))
  }) %>% 
  ## Bind reactive plot to a shiny output
  bind_shiny("density_bar","ggvis_density_bar")

  # A reactive expression with the biomass bar plot
  reactive({
    
    ggvis(bar_data(),~factor(species),~kg.per.ha) %>% 
      layer_bars() %>% 
      add_axis("x",title="",ticks=0,properties = axis_props(
        labels = list(angle = 45,align = "left",fontSize=14))) %>%
      add_axis("y",title="Mean Kilogram per Hectare Swept",title_offset=55,properties = axis_props(
        title=list(fontSize=16),
        labels=list(fontSize=13)))
  }) %>% 
  ## Bind reactive plot to a shiny output
  bind_shiny("biomass_bar","ggvis_biomass_bar")
  
  ## Reactive Ranked CPUE plot label
  output$rank_label <- renderText({
    HTML(paste("Mean density (N/ha; top) and biomass (Kg/ha; bottom) of the 10 most abundant species in",
               tags$b(tbl_year2()$year),tags$b(input$season2),"collected in Ontario, Michigan, and Ohio waters in the western basin of Lake Erie."
    ))
  })
  
  ## Download plot data
  output$downloadCSV_8 <- downloadHandler(
    filename=reactive(paste(tbl_year2()$year,input$season2,"Ranked_Catch_Data",sep="_")),
    content=function(file) {
      write.csv(bar_data(),file,row.names=FALSE)
    },
    contentType="text/csv"
  )
  
## -----------------------------------------------------------
## Forage Task Group Data Manipulation and Plot
## -----------------------------------------------------------

  ## Filter catch, returning a data frame
  ftg_Rdata <- reactive({
    
    ## Summarize density by year and season
    p <- ftg_data %>% group_by(year,class) %>%
      summarise(n.per.ha=round(mean(n.per.ha),2),
                kg.per.ha=round(mean(kg.per.ha),2)) %>% 
      ungroup() %>% 
      mutate(year=factor(year))
  })
  
  ## Filter catch, returning the means
  ftg_mean <- reactive({
    
    ## Arrange in decending order by year
    p <- ftg_data %>% arrange(desc(year))
    
    ## Determine length (number of years)
    p_rep <- p$year %>% n_distinct()
    
    ## Remove duplicates (i.e. retain unique values)
    p_class <- p %>% distinct(class,year) %>% 
      select(class,year)
    
    ## Summarize density by year and season
    ftg_Rdata2 <- ftg_Rdata() %>%
      group_by(class) %>%
      summarise(n.per.ha=round(mean(n.per.ha),2),
                kg.per.ha=round(mean(kg.per.ha),2)
      )
    
    ## Repeat mean for number of years
    p_mean_rep <- as.data.frame(do.call(cbind,lapply(ftg_Rdata2,rep,p_rep))) %>%
      select(n.per.ha,kg.per.ha)
    p_mean_rep <- bind_cols(p_mean_rep,p_class) %>% 
      mutate(year=factor(year))
  })

  ## Function for generating map tooltip text
  tooltip2 <- function(x2) {
    wb <- isolate(ftg_Rdata())
    ftg_Rdata <- wb[wb$class == x2$class,] %>% 
      arrange(desc(year))
    ## paste text string
    paste0("<b>",ftg_Rdata$year," Density: ",ftg_Rdata$n.per.ha," (N/ha)","<br>")
  }
  
  ## Function for generating map tooltip text
  tooltip3 <- function(x3) {
    wb <- isolate(ftg_Rdata())
    ftg_Rdata <- wb[wb$class == x3$class,] %>% 
      arrange(desc(year))
    ## paste text string
    paste0("<b>",ftg_Rdata$year," Biomass: ",ftg_Rdata$kg.per.ha," (Kg/ha)","<br>")
  }

  ## A reactive expression with the historical density time series plot
  reactive({
    
    ggvis(ftg_Rdata,~year,~n.per.ha) %>%
      group_by(class) %>%
      layer_points(prop("fill",~class),prop("size",80)) %>%
      layer_lines(stroke = ~class,prop("strokeWidth",2)) %>%
      layer_paths(data=filter(ftg_mean,class=="Spiny-rayed"),~year,~n.per.ha,strokeDash:=6,stroke:="#279627",prop("strokeWidth",1)) %>% 
      layer_paths(data=filter(ftg_mean,class=="Soft-rayed"),~year,~n.per.ha,strokeDash:=6,stroke:="#FF7311",prop("strokeWidth",1)) %>%
      layer_paths(data=filter(ftg_mean,class=="Clupeids"),~year,~n.per.ha,strokeDash:=6,stroke:="#1C6CAB",prop("strokeWidth",1)) %>%
      hide_legend("stroke") %>%
      add_legend("fill",title="Functional Groups") %>%
      scale_numeric("y",domain=c(0,NA)) %>%
      add_axis("x",title="Year",ticks=1,title_offset=35,properties = axis_props(
        title=list(fontSize=16),
        labels=list(fontSize=13))) %>%
      add_axis("y",title="Mean Catch per Hectare Swept",title_offset=55,properties = axis_props(
        title=list(fontSize=16),
        labels=list(fontSize=13))) %>%
      add_tooltip(tooltip2, "hover")
  }) %>% 
  ## Bind reactive plot to a shiny output
  bind_shiny("ftg_n","ggvis_n_ftg")
  
  ## A reactive expression with the historical biomass time series plot
  reactive({
    
    ggvis(ftg_Rdata,~year,~kg.per.ha) %>%
      group_by(class) %>%
      layer_points(prop("fill",~class),prop("size",80)) %>%
      layer_lines(stroke = ~class,prop("strokeWidth",2)) %>%
      layer_paths(data=filter(ftg_mean,class=="Spiny-rayed"),~year,~kg.per.ha,strokeDash:=6,stroke:="#279627",prop("strokeWidth",1)) %>% 
      layer_paths(data=filter(ftg_mean,class=="Soft-rayed"),~year,~kg.per.ha,strokeDash:=6,stroke:="#FF7311",prop("strokeWidth",1)) %>%
      layer_paths(data=filter(ftg_mean,class=="Clupeids"),~year,~kg.per.ha,strokeDash:=6,stroke:="#1C6CAB",prop("strokeWidth",1)) %>%
      hide_legend("stroke") %>%
      add_legend("fill",title="Functional Groups") %>%
      scale_numeric("y",domain=c(0,NA)) %>%
      add_axis("x",title="Year",ticks=1,title_offset=35,properties = axis_props(
        title=list(fontSize=16),
        labels=list(fontSize=13))) %>%
      add_axis("y",title="Mean Kilogram per Hectare Swept",title_offset=55,properties = axis_props(
        title=list(fontSize=16),
        labels=list(fontSize=13))) %>%
      add_tooltip(tooltip3, "hover")
  }) %>% 
  ## Bind reactive plot to a shiny output  
  bind_shiny("ftg_kg","ggvis_kg_ftg")
  
  ## Download
  output$downloadCSV_2 <- downloadHandler(
    filename="Forage_Density_Data",
    content=function(file) {
      write.csv(ftg_Rdata(),file,row.names=FALSE)
    },
    contentType="text/csv"
  )

## -----------------------------------------------------------
## Abiotic Data Manipulation and Plot
## -----------------------------------------------------------
  
  # ## A reactive expression with the western basin map
  # abiotic_data <- reactive({
  #   r <- wb_wq %>% 
  #   ## filter by year
  #   filter(year == input$year3) %>% 
  #   ## filter by season
  #   filter(season == input$season3) %>% 
  #   ## filter by parameter
  #   filter(parameter == input$parameter) %>% 
  #     select(serial,day,month,year,season,Depth,value,lat,long) %>% 
  #     arrange(serial)
  #   
  #   ## rename column headers
  #   colnames(r) <- c("Station","Day","Month","Year","Season","Depth (m)",input$parameter,"Latitude","Longitude")
  #   r
  # })
  
  ## A reactive expression with the depth profiles of water quality
  reactive({
    ggvis(wb_wq,~temp.mean,~bin.depth.max,fill=~do.ppm.mean,shape=~season)%>%
    filter(year==eval(input_select(choices=unique(wb_wq$year),selected=2017,label="Year")))%>%
    filter(serial==eval(input_select(choices=unique(wb_wq$serial),selected=37,label="Station")))%>%
    layer_points(size:=200)%>%
    add_axis("x", orient="top",title="Temperature (C) ") %>%
    scale_numeric("y", reverse=T)%>%
    scale_numeric("fill", range=c("red","green"))%>%
    add_axis("y", title="Depth (m)")%>%
    add_legend("fill",title="Dissolved Oxygen (mg/L)")%>%
    add_legend(scales="shape",title="Season",properties=legend_props(legend=list(y=150)))%>%
    set_options(duration = 0)
  }) %>% 
    ## Bind reactive plot to a shiny output  
    bind_shiny("wq_dp","ggvis_wq_dp")
  
      
  # output$abiotic_table <- renderDataTable({
  #   datatable(abiotic_data(),rownames=FALSE,extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = I('colvis'),active="mouseover"))
  # })
  
  # ## label for depth profiles
  # output$wq_table_label <- renderText({
  #   HTML(paste("Depth profiles of temperature and dissolved oxygen from bottom trawl sampling locations in the western basin of Lake Erie."))
  # })
  
  # ## Download plot data
  # output$downloadCSV_7 <- downloadHandler(
  #   filename=reactive(paste(tbl_year3()$year,input$season3,input$parameter,"Abiotic_Table",sep="_")),
  #   content=function(file) {
  #     write.csv(abiotic_data(),file,row.names=FALSE)
  #   },
  #   contentType="text/csv"
  # )
  
## -----------------------------------------------------------
## Density and Biomass Data Manipulation and Map
## -----------------------------------------------------------

  ## Filter density and biomass, returning a data frame
  map_data <- reactive({
    
  ## Individual Species
    c <- catch %>% 
    ## filter by year
      filter(year == input$year) %>% 
    ## filter by season
      filter(season == input$season) %>% 
    ## filter by species
      filter(species == input$species)
    
    ## Optional: filter by life stage
    if (!is.null(input$life.stage) && input$life.stage != "All Life Stages") {
      c %<>% filter(life.stage == input$life.stage)
    }
    
    ## Summarize density and biomass values
    c %<>%
      group_by(serial,year,season,species) %>%
      summarise(n.per.ha=sum(n.per.ha),
                kg.per.ha=sum(kg.per.ha),
                long=mean(long),
                lat=mean(lat))
    
  ## All Species 
    u <- catch %>% 
      ## filter by year
      filter(year == input$year) %>% 
      ## filter by season
      filter(season == input$season)
    
    ## Optional: filter by life stage
    if (!is.null(input$life.stage) && input$life.stage != "All Life Stages") {
      u %<>% filter(life.stage == input$life.stage)
    }
    
    ## Summarize density and biomass values
    u %<>%
      group_by(serial,year,season) %>%
      summarise(Total_NperHA=sum(n.per.ha),
                Total_KgperHA=sum(kg.per.ha),
                long=mean(long),
                lat=mean(lat))
    
  ## Combine
  final <- left_join(c,u,by=c("serial", "year", "season", "long", "lat"))
  })

  ## Function for generating map tooltip text
    tooltip4 <- function(x4) {
      wb <- isolate(map_data())
      map_data <- wb[unique(wb$serial) == unique(x4$serial),]
      
      species <- unique(map_data$species)
      ## Paste text string
      paste0("<b>","Station: ",map_data$serial,"<br>",species," Density (N/ha): ",map_data$n.per.ha,
             "<br>","All Species Density (N/ha): ",map_data$Total_NperHA)
    }
    
  ## Function for generating map tooltip text
  tooltip5 <- function(x5) {
    wb <- isolate(map_data())
    map_data <- wb[unique(wb$serial) == unique(x5$serial),]
    
    species <- unique(map_data$species)
    ## Paste text string
    paste0("<b>","Station: ",map_data$serial,"<br>",species," Biomass (Kg/ha): ",map_data$kg.per.ha,
           "<br>","All Species Biomass (Kg/ha): ",map_data$Total_KgperHA)
  }

  ## A reactive expression with the western basin map
  map_species <- reactive(data_frame(text=paste(as.character(distinct(filter(lw,year == input$year),year)$year),
                                                input$season,
                                                tbl_ls(),
                                                as.character(distinct(filter(lw,species == input$species),species)$species)),
                                     lat=42.12,
                                     long=-82.817)
  )

  ## A reactive expression with the density and biomass spatial data
  reactive({
    ggvis(data=filter(wb_shore,piece=="1" & group=="3.1"),~long,~lat) %>%
      layer_paths() %>%
      layer_paths(data=filter(wb_shore,piece=="2"),~long,~lat) %>%
      layer_paths(data=filter(wb_shore,piece=="3"),~long,~lat) %>%
      layer_paths(data=filter(wb_shore,piece=="4"),~long,~lat) %>%
      layer_paths(data=filter(wb_shore,piece=="5"),~long,~lat) %>%
      layer_paths(data=filter(wb_shore,piece=="6"),~long,~lat) %>%
      layer_points(data=map_data(),~long,~lat,size=~Total_NperHA,
                   fill:=NA,stroke:="black",strokeOpacity:=0.1) %>%
      layer_points(data=map_data(),~long,~lat,size=~n.per.ha,
                   fillOpacity:=0.6,fillOpacity.hover:=1) %>%
      layer_text(data=map_species,~long,~lat,text:=~text,fontSize:= 13,fontWeight:="bold",fill:="black",
                 baseline:="middle", align:="center") %>%
      add_tooltip(tooltip4, "hover") %>%
      hide_legend("size") %>%
      scale_numeric("x",domain=c(-83.514,-82.12),nice=FALSE) %>%
      scale_numeric("y",domain=c(41.306,42.103),nice=FALSE) %>%
      scale_numeric("size",domain=c(0.0001,2000),range=c(0.0001,2000),clamp=TRUE) %>%
      add_axis("x",title="",ticks="",tick_size_end="") %>%
      add_axis("x",orient="top",title="Number per Hectare Swept",title_offset=-24,ticks="",tick_size_end="",
               properties = axis_props(title=list(fontSize=13))) %>% 
      add_axis("y",title="",ticks="",tick_size_end="") %>% 
      add_axis("y",orient="right",title="",ticks="",tick_size_end="") %>% 
      set_options(duration=0)
    }) %>% 
  ## bind reactive plot to a shiny output  
  bind_shiny("density_map","ggvis_density_map")
    
  ## A reactive expression with the density and biomass spatial data
  reactive({
    ggvis(data=filter(wb_shore,piece=="1" & group=="3.1"),~long,~lat) %>%
      layer_paths() %>%
      layer_paths(data=filter(wb_shore,piece=="2"),~long,~lat) %>%
      layer_paths(data=filter(wb_shore,piece=="3"),~long,~lat) %>%
      layer_paths(data=filter(wb_shore,piece=="4"),~long,~lat) %>%
      layer_paths(data=filter(wb_shore,piece=="5"),~long,~lat) %>%
      layer_paths(data=filter(wb_shore,piece=="6"),~long,~lat) %>%
      layer_points(data=map_data(),~long,~lat,size=~Total_KgperHA*10,
                   fill:=NA,stroke:="black",strokeOpacity:=0.1) %>%
      layer_points(data=map_data(),~long,~lat,size=~kg.per.ha*10,
                   fillOpacity:=0.6,fillOpacity.hover:=1) %>%
      layer_text(data=map_species,~long,~lat,text:=~text,fontSize:= 13,fontWeight:="bold",fill:="black",
                 baseline:="middle", align:="center") %>%
      add_tooltip(tooltip5, "hover") %>%
      hide_legend("size") %>%
      scale_numeric("x",domain=c(-83.514,-82.12),nice=FALSE) %>%
      scale_numeric("y",domain=c(41.306,42.103),nice=FALSE) %>%
      scale_numeric("size",domain=c(0.0001,2000),range=c(0.0001,2000),clamp=TRUE) %>%
      add_axis("x",title="",ticks="",tick_size_end="") %>%
      add_axis("x",orient="top",title="Kilogram per Hectare Swept",title_offset=-24,ticks="",tick_size_end="",
               properties = axis_props(title=list(fontSize=13))) %>%
      add_axis("y",title="",ticks="",tick_size_end="") %>%
      add_axis("y",orient="right",title="",ticks="",tick_size_end="") %>% 
      set_options(duration=0)
    }) %>% 
  ## bind reactive plot to a shiny output  
  bind_shiny("biomass_map","ggvis_biomass_map")
    
  ## Reactive label for spatial map
  output$map_label <- renderText({
    HTML(paste("Spatial distribution of",tags$b(tbl_year()$year),tags$b(input$season),tags$b(tbl_ls()),tags$b(tbl_species()$species),"density (N/ha; top) and biomass (Kg/ha; bottom) from bottom trawl samples collected in the western basin of Lake Erie. 
               Symbol sizes are directly proportional to the values plotted, but are truncated at 2000 (N/ha) or 200 (Kg/ha) to be inclusive of all values greater. 
               Hollow circles represent total density and biomass for all species, respectively."
    ))
  })
    
  ## Reactive label for life stages
  output$map_ls_label <- renderText({
    HTML(paste0("<p><h5>","Available Life Stages for ",tbl_species()$species,":","</h5></p>","(",map_ls()$vars,")")
    )
  })
  
  ## Download plot data
  output$downloadCSV_3 <- downloadHandler(
    filename=reactive(paste(tbl_year()$year,input$season,if(input$life.stage != "All Life Stages") {tbl_ls()} else {"All_Life_Stages"},
                            tbl_species()$species,"Map_Data",sep="_")),
    content=function(file) {
      write.csv(map_data(),file,row.names=FALSE)
    },
    contentType="text/csv"
  )

## -----------------------------------------------------------
## Lenght-Weight Data Manipulation and Plot
## -----------------------------------------------------------

  ## Filter, returning a data frame
  length_weight <- reactive({
    
    ## Filter the lengths
    minlength <- input$min_val
    maxlength <- input$max_val
    
    ## Filter by minimum and maximum lengths
    m <- lw %>% 
      filter(
        tl.mm >= minlength,
        tl.mm <= maxlength) %>% 
    ## filter by year
      filter(year == input$year) %>% 
    ## filter by season
      filter(season == input$season) %>% 
    ## filter by species
      filter(species == input$species)
  })
 
  ## Calculate regressions
  reg_fit <- reactive({
    if(input$datatrans == "Linear") {
      result <- try({
        model <- lm(logw~logl,data=length_weight())
      },silent=TRUE)
      
      if(class(result) != "try-error" && length(result$fitted.values) >1) {
        model <- data_frame(fitted.values(result))
        colnames(model) <- "fit_wt"
        cf <- bind_cols(length_weight(),model)
        reg_fit <- cf %>% mutate(fit_tl = logl) %>%
          arrange(fit_tl)
      } else {
        reg_fit <- data.frame(fit_tl = 0, fit_wt = 0)
      }
    } else {
      result <- try({
        model <- lm(logw~logl,data=length_weight())
      },silent=TRUE)
      if(class(result) != "try-error" && length(result$fitted.values) >1) {
        model <- coef(result)
        nlm <- data.frame(tl.mm=seq(min(length_weight()$tl.mm), max(length_weight()$tl.mm), 5))
        reg_fit <- nlm %>% mutate(fit_wt = exp(model[1])*tl.mm^model[2], fit_tl = tl.mm) %>%
          arrange(fit_tl)
      } else {
        reg_fit <- data.frame(fit_tl = 0, fit_wt = 0)
      }
    }
  })
    
  ## Make a dynamic slider
  output$slider <- renderUI({
    sliderInput("inSlider", "Slider", min=input$min_val, max=input$max_val)
  })
  
  ## A reactive expression with the lenght-weight plot
  reactive({
    if (input$datatrans == "Linear") {
      plot_lw <- length_weight() %>% transmute(tl.mm=logl,wt.g=logw)
      xvar_name <- names(axis_vars)[3]
      yvar_name <- names(axis_vars)[4]
    } else {
      plot_lw <- length_weight()
      xvar_name <- names(axis_vars)[1]
      yvar_name <- names(axis_vars)[2]
    }
  
  ggvis(data=plot_lw,~tl.mm, ~wt.g) %>%
    layer_points(size := 50,fillOpacity := 0.2) %>%
    layer_paths(data=reg_fit,~fit_tl,~fit_wt) %>%
    add_axis("x", title = xvar_name, title_offset = 40,properties = axis_props(
      title=list(fontSize=16),
      labels=list(fontSize=13))) %>%
    add_axis("y", title = yvar_name, title_offset = 55,properties = axis_props(
      title=list(fontSize=16),
      labels=list(fontSize=13))) }) %>% 
  ## Bind reactive plot to a shiny output
  bind_shiny("lw_plot","ggvis_lw_plot")
  
  ## Reactive label for regression plot
  output$reg_plot_label <- renderText({
    if (input$datatrans == "Linear") {
      HTML(paste("Fitted line plot for the regression of natural-log transformed weight on natural-log transformed total length of",tags$b(tbl_year()$year),tags$b(input$season),tags$b(tbl_species()$species),"from western basin of Lake Erie. 
      Total lengths and weights collected from a size-mode specific subsample on board the R/V Muskie."))
      } else {
      HTML(paste("Fitted line plot for the regression of weight (g) on  total length (mm) of",tags$b(tbl_year()$year),tags$b(input$season),tags$b(tbl_species()$species),"from western basin of Lake Erie. 
      Total lengths and weights collected from a size-mode specific subsample on board the R/V Muskie."))
    }
  })
  
  ## Reactive label for regression summary table label
  output$reg_tbl_label <- renderText({
    if (input$datatrans == "Linear") {
      HTML(paste("Parameters for the regression of natural-log transformed weight on natural-log transformed total length from",tags$b(tbl_year()$year),tags$b(input$season),tags$b(tbl_species()$species),"in the western basin of Lake Erie. 
      Non-linear and log-transformed versions of the model are listed below."))
    } else {
      HTML(paste("Parameters for the regression of weight (g) on  total length (mm) from",tags$b(tbl_year()$year),tags$b(input$season),tags$b(tbl_species()$species),"in the western basin of Lake Erie. 
      Non-linear and log-transformed versions of the model are listed below."))
    }
  })
  
  ##  Reactive regression summary table
  output$reg_tbl <- renderTable({
    result <- try({
      model <- lm(logw~logl,data=length_weight())
    },silent=TRUE)
    
    if(class(result) != "try-error" && length(result$fitted.values) >1) {
      model <- coef(summary(lm(logw~logl,data=length_weight())))
      if(input$datatrans == "Linear") {
        rownames(model) <- c("log(a)","b")
        colnames(model) <- c("Estimate","Std. Error","t-value","p-value")
        model
      } else {
        model[1,1] <- exp(model[1,1])
        rownames(model) <- c("a","b")
        colnames(model) <- c("Estimate","Std. Error","t-value","p-value")
        model
      }
    } else {
      no_results <- data_frame(a = c(NA,NA))
      rownames(no_results) <- c("a","b")
      colnames(no_results) <- c("Model was unable to fit")
      no_results
    }
  },digits=8)

  ## Sample size text output
  output$n_fish <- renderText({ if(nrow(length_weight()) > 0) {
    nrow(length_weight())
    } else {
      "No Data - Consider Revising Inputs"
    }
  })

  ## Download
  output$downloadCSV_4 <- downloadHandler(
    filename=reactive(paste(tbl_year()$year,input$season,if(input$life.stage != "All Life Stages") {tbl_ls()} else {"All_Life_Stages"},
                            tbl_species()$species,"Weight_Length_Data",sep="_")),
    content=function(file) {
      write.csv(length_weight(),file,row.names=FALSE)
    },
    contentType="text/csv"
  )

## -----------------------------------------------------------
## Lenght Frequency Data Manipulation and Histogram
## -----------------------------------------------------------
  
  ## Filter length frequency, returning a data frame
  len_freq <- reactive({
    
    ## Define user defined length inputs
    minlength2 <- input$min_val2
    maxlength2 <- input$max_val2
    
    ## Filter by minimum and maximum lengths
    len <- wb_exp %>% 
      filter(
        tl.mm >= minlength2,
        tl.mm <= maxlength2) %>% 
    ## filter by year
      filter(year == input$year) %>% 
    ## filter by season
      filter(season == input$season) %>% 
    ## filter by species
      filter(species == input$species)
    
    if(nrow(len) == 1) {
      len <- data_frame(tl.mm=0)
    } else {
      len %<>% as.data.frame()
    }
  })

  ## Make dynamic slider
  output$slider2 <- renderUI({
    sliderInput("inSlider2", "Slider", min=input$min_val2, max=input$max_val2)
  })

  ## A reactive expression with the length frequency plot
  reactive({
    
    ## Lables for axes
    xvar_name <- "Total Length (mm)"
    yvar_name <- "Frequency"
    
    len_freq %>%
      ggvis(~tl.mm) %>%
      add_axis("x", title = xvar_name, title_offset = 40, properties = axis_props(
        title=list(fontSize=16),
        labels=list(fontSize=13))) %>%
      add_axis("y", title = yvar_name, title_offset = 70, properties = axis_props(
        title=list(fontSize=16),
        labels=list(fontSize=13))) %>%
      layer_histograms(width = input$slider1)
    }) %>% 
  ## Bind reactive plot to a shiny output
  bind_shiny("hist","ggvis_hist")
  
  ## Reactive label for length frequency plot
  output$len_freq_label <- renderText({
    HTML(paste("Length frequency of",tags$b(tbl_year()$year),tags$b(input$season),tags$b(tbl_species()$species),"from the western basin of Lake Erie. 
          Lengths were expanded from measured total lengths collected on board the R/V Muskie."))
  })
  
  ## Sample size text output
  output$n_size <- renderText({ if(nrow(len_freq()) > 0) {
    nrow(len_freq())
    } else {
      "No Data - Consider Revising Inputs"
    }
  })

  ## Download
  output$downloadCSV_5 <- downloadHandler(
    filename=reactive(paste(tbl_year()$year,input$season,if(input$life.stage != "All Life Stages") {tbl_ls()} else {"All_Life_Stages"},
                            tbl_species()$species,"Length_Frequency_Data",sep="_")),
    content=function(file) {
      write.csv(len_freq(),file,row.names=FALSE)
    },
    contentType="text/csv"
  )
})
## The End!
