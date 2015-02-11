# Define server logic required
shinyServer(function(input, output) {
  
## -----------------------------------------------------------
## Historical Time Series Data Manipulation and Plot
## -----------------------------------------------------------

  # Filter catch, returning a data frame
  time_data <- reactive({
    
    l <- select(catchHA,species=species,Year=year,Season=season,NperHA=NperHA)
    
    # Optional: filter by species
    if (!is.null(input$species2) && input$species2 != "") {
      l <- l[l$species == input$species2,]
    }
    
    if (length(l$NperHA) > 0) {
    l %<>% group_by(Year,Season) %>%
      summarise(density=round(mean(NperHA),2)) %>%
      tbl_df() } else {
        data_frame(Year = 0, Season = 0, density = 0, text = "No Data")
      }
    
  })

  # Function for generating map tooltip text
  tooltip <- function(x) {
    if (is.null(x)) return(NULL)    
    
    # Pick out the individual with this ID
    wb <- isolate(time_data())
    time_data <- wb[wb$Season == x$Season,]
    
    paste0("<b>",time_data$Year," Density: ",time_data$density," (N/ha)","<br>")
  }
  
  # A reactive expression with the historical time series plot
  reactive({

    if(nrow(time_data()) > 1) {
    time_data() %>% group_by(Season) %>%
    ggvis(~factor(Year),~density) %>%
      layer_points(fill = ~Season,prop("size",80)) %>%
      layer_lines(stroke = ~Season,prop("strokeWidth",2)) %>%
      add_tooltip(tooltip,"hover") %>%
      scale_numeric("y",domain=c(0,NA)) %>%
      add_axis("x",title="Year",ticks=1,title_offset=35,properties = axis_props(
        title=list(fontSize=13))) %>%
      add_axis("y",title="Mean Catch Per Hectare Swept",title_offset=65,properties = axis_props(
        title=list(fontSize=13))) } else {
          time_data() %>% ggvis(~Year,~Season) %>%
            layer_points(~text) %>%
            scale_numeric("y",domain=c(0,NA)) %>%
            add_axis("x",title="Year",ticks=1,title_offset=35,properties = axis_props(
              title=list(fontSize=13))) %>%
            add_axis("y",title="Mean Catch Per Hectare Swept",title_offset=65,properties = axis_props(
              title=list(fontSize=13)))
        } }) %>% bind_shiny("time")
  
  output$ggvis_time <- renderUI({
    ggvisOutput("time")
  })

  # Download
  output$downloadCSV_1 <- downloadHandler(
    filename="catch_data",
    content=function(file) {
      write.csv(time_data(),file,row.names=FALSE)
    },
    contentType="text/csv"
  )

## -----------------------------------------------------------
## Forage Task Group Data Manipulation and Plot
## -----------------------------------------------------------

  # Filter catch, returning a data frame
  ftg_Rdata <- reactive({
    
    p <- ftg_data
    
    # Summarize density by year and season
    p %<>%
      group_by(year,class) %>%
      summarise(NperHA=round(mean(NperHA),2)
      )
  })

  # Function for generating map tooltip text
  tooltip2 <- function(x2) {
    if (is.null(x2)) return(NULL)
    
    # Pick out the individual with this ID
    wb2 <- isolate(ftg_Rdata())
    ftg_Rdata <- wb2[wb2$class == x2$class,]
    
    paste0("<b>",ftg_Rdata$year," Density: ",ftg_Rdata$NperHA," (N/ha)","<br>")
  }

  # A reactive expression with the historical time series plot
  reactive({
    
    ftg_Rdata %<>% group_by(class) %>%
      ggvis(~factor(year),~NperHA) %>%
        layer_points(prop("fill",~class),prop("size",80)) %>%
        layer_lines(stroke = ~class,prop("strokeWidth",2)) %>%
        hide_legend("stroke") %>%
        add_legend("fill",title="Functional Groups") %>%
        scale_numeric("y",domain=c(0,NA)) %>%
        add_axis("x",title="Year",ticks=1,title_offset=35,properties = axis_props(
          title=list(fontSize=13))) %>%
        add_axis("y",title="Mean Catch Per Hectare Swept",title_offset=65,properties = axis_props(
          title=list(fontSize=13))) %>%
        add_tooltip(tooltip2, "hover")
  }) %>% bind_shiny("ftg")
  
  output$ggvis_ftg <- renderUI({
    ggvisOutput("ftg")
  })
  
  # Download
  output$downloadCSV_2 <- downloadHandler(
    filename="forage_density_data",
    content=function(file) {
      write.csv(ftg_Rdata(),file,row.names=FALSE)
    },
    contentType="text/csv"
  )

## -----------------------------------------------------------
## Density and Biomass Data Manipulation and Map
## -----------------------------------------------------------

  # Filter density and biomass, returning a data frame
  map_data <- reactive({
    
    c <- catchHA
    
    # Optional: filter by year
    if (!is.null(input$year) && input$year != "") {
      c <- c[c$year == input$year,]
    }
    # Optional: filter by season
    if (!is.null(input$season) && input$season != "") {
      c <- c[c$season == input$season,]
    }
    # Optional: filter by serial 
    if (!is.null(input$serial) && input$serial != "All") {
      c <- c[c$serial == input$serial,]
    }
    # Optional: filter by species
    if (!is.null(input$species) && input$species != "") {
      c <- c[c$species == input$species,]
    }
    # Optional: filter by species
    if (!is.null(input$life_stage) && input$life_stage != "All Life Stages") {
      c <- c[c$life_stage == input$life_stage,]
    }
    
    # Summarize density and biomass values
    c %<>%
      group_by(serial) %>%
      summarise(NperHA=sum(NperHA),
                KgperHA=sum(KgperHA),
                long=mean(long),
                lat=mean(lat)
      )
  })

  # Function for generating map tooltip text
    tooltip3 <- function(x3) {
      if (is.null(x3)) return(NULL)
      if (is.null(unique(x3$serial))) return(NULL)
      
    # Pick out the individual with this ID
      wb3 <- isolate(map_data())
      map_data <- wb3[unique(wb3$serial) == unique(x3$serial),]
      
      paste0("<b>","Station: ",map_data$serial,"<br>","Density (N/ha): ",map_data$NperHA,"<br>","Biomass (Kg/ha): ",map_data$KgperHA)
    }

  # A reactive expression with the western basin map
  reactive({
    sizevar <- prop("size",as.symbol(input$density))
      
        ggvis(data=filter(wb_shore,piece=="1" & group=="3.1"),~long,~lat) %>%
        layer_paths() %>%
        layer_paths(data=filter(wb_shore,piece=="2"),~long,~lat) %>%
        layer_paths(data=filter(wb_shore,piece=="3"),~long,~lat) %>%
        layer_paths(data=filter(wb_shore,piece=="4"),~long,~lat) %>%
        layer_paths(data=filter(wb_shore,piece=="5"),~long,~lat) %>%
        layer_paths(data=filter(wb_shore,piece=="6"),~long,~lat) %>%
        layer_points(data=effort,~long_st,~lat_st,size=6,
                    fill:=NA,stroke:="black",strokeOpacity:=0.1) %>%
        layer_points(data=map_data,~long,~lat,size:=sizevar,key:=~serial,
                     fillOpacity:=0.6) %>% #, fillOpacity.hover:=1) %>%
        add_legend("size","fill",title="Value",
                   values=factor(c(25,50,100,250,500,1000,1500,2000),labels=c("25","50","100","250","500","1000","1500","2000")),
                   properties=legend_props(
                     symbol=list(fill="black"))) %>%
        add_tooltip(tooltip3, "hover") %>%
        scale_numeric("x",domain=c(-83.514,-82.12),nice=FALSE) %>%
        scale_numeric("y",domain=c(41.306,42.103),nice=FALSE) %>%
        scale_numeric("size",domain=c(25,2000),range=c(25,2000),clamp=TRUE) %>%
        add_axis("x",title="",ticks="",tick_size_end="") %>%
        add_axis("x",orient="top",title="",ticks="",tick_size_end="") %>%
        add_axis("y",title="",ticks="",tick_size_end="") %>%
        add_axis("y",orient="right",title="",ticks="",tick_size_end="")
  }) %>% bind_shiny("map","ggvis_map")

  # Download
  output$downloadCSV_3 <- downloadHandler(
    filename="map_data",
    content=function(file) {
      write.csv(map_data(),file,row.names=FALSE)
    },
    contentType="text/csv"
  )

## -----------------------------------------------------------
## Lenght-Weight Data Manipulation and Plot
## -----------------------------------------------------------

  # Filter, returning a data frame
  length_weight <- reactive({
    
    # Filter the lengths
    minlength <- input$min_val
    maxlength <- input$max_val
    
    m <- lw %>% 
      filter(
        tl >= minlength,
        tl <= maxlength)
    
    # Optional: filter by year
    if (!is.null(input$year) && input$year != "") {
      m <- m[m$year == input$year,]
    }
    # Optional: filter by season
    if (!is.null(input$season) && input$season != "") {
      m <- m[m$season == input$season,]
    }
    # Optional: filter by serial
    if (!is.null(input$serial) && input$serial != "") {
      m <- m[m$serial == input$serial,]
    }
    # Optional: filter by species
    if (!is.null(input$species) && input$species != "") {
      m <- m[m$species == input$species,]
    }
    m <- as.data.frame(m)
  })
 
  # Make a dynamic slider
  output$slider <- renderUI({
    sliderInput("inSlider", "Slider", min=input$min_val, max=input$max_val)
  })

  # A reactive expression with the lenght-weight plot
  reactive({
    
    if (!is.null(input$datatrans) && input$datatrans != "None") {
      length_weight() %<>% transmute(tl=logl,wt=logw)
      xvar_name <- names(axis_vars)[3]
      yvar_name <- names(axis_vars)[4]
      
      result <- try({
        model <- lm(logw~logl,data=length_weight())
      },silent=TRUE)
      
      if(class(result) != "try-error") {
        model <- data_frame(fitted.values(lm(logw~logl,data=length_weight())))
        colnames(model) <- "fit_wt"
        cf <- bind_cols(length_weight(),model)
        reg_fit <- cf %>% mutate(fit_tl = logl) %>%
          arrange(fit_tl)
      } else {
        reg_fit <- data_frame(fit_tl = 0, fit_wt = 0)
      }
    } else {
      xvar_name <- names(axis_vars)[1]
      yvar_name <- names(axis_vars)[2]
      
      result <- try({
        model <- (lm(logw~logl,data=length_weight()))
      },silent=TRUE)
      
      if(class(result) != "try-error") {
        model <- coef(lm(logw~logl,data=length_weight()))
        nlm <- data.frame(tl=seq(min(length_weight()$tl),max(length_weight()$tl),0.5))
        reg_fit <- nlm %>% mutate(fit_wt = exp(model[1])*tl^model[2], fit_tl = tl) %>%
          arrange(fit_tl)
      } else {
        reg_fit <- data_frame(fit_tl = 0, fit_wt = 0)
      } 
    }
    
    length_weight %>%
      ggvis(~tl, ~wt) %>%
      layer_points(size := 50,fillOpacity := 0.2) %>%
      layer_paths(data=reg_fit,~fit_tl,~fit_wt) %>%
      add_axis("x", title = xvar_name, title_offset = 35,properties = axis_props(
        title=list(fontSize=13))) %>%
      add_axis("y", title = yvar_name, title_offset = 55,properties = axis_props(
        title=list(fontSize=13))) }) %>% bind_shiny("lw_plot")
  
  output$ggvis_lw_plot <- renderUI({
    ggvisOutput("lw_plot")
  })
  
  # Create table for the linear regression parameter estimates
  output$lm_call <- renderTable({
    result <- try({
      model <- lm(logw~logl,data=length_weight())
      results <- coef(summary(model))
      rownames(results) <- c("log(a)","b")
      colnames(results) <- c("Estimate","Std. Error","t-value","p-value")
      results
    },silent=TRUE)
    
    if(class(result) != "try-error") {
      model <- lm(logw~logl,data=length_weight())
      results <- coef(summary(model))
      rownames(results) <- c("log(a)","b")
      colnames(results) <- c("Estimate","Std. Error","t-value","p-value")
      results
    } else {
      results <- data_frame(a = c(0,0))
      rownames(results) <- c("a","b")
      colnames(results) <- c("Model was unable to fit")
      results
    }
  },digits=8)
    
  # Calculate fit of the power function
  lm_fit <- reactive({
    result <- try({
      model <- lm(logw~logl,data=length_weight())
      cf <- data_frame(fitted.values(model))
      cf <- bind_cols(length_weight(),cf) %>%
        tbl_df()
    },silent=TRUE)
    
    if(class(result) != "try-error") {
      model <- lm(logw~logl,data=length_weight())
      cf <- data_frame(fitted.values(model))
      colnames(cf) <- "fit"
      cf <- bind_cols(length_weight(),cf) %>%
        arrange(tl) %>%
        tbl_df()
    } else {
      data_frame(logl = 0, logw = 0, text = "No Data")
    }
  })
  
  # A reactive expression with the power function fit plot
  reactive({
    if(nrow(lm_fit()) > 1) {
    ggvis(data=length_weight(),~logl, ~logw) %>%
      layer_points(size := 50,fillOpacity := 0.2) %>%
      layer_paths(data=lm_fit(),~logl,~fit) %>%
      add_axis("x", title = "Log Length", title_offset = 35,properties = axis_props(
        title=list(fontSize=13))) %>%
      add_axis("y", title = "Log Weight", title_offset = 55,properties = axis_props(
        title=list(fontSize=13))) } else {
          ggvis(data=lm_fit(),~logl,~logw) %>%
            layer_points(~text) %>%
            add_axis("x", title = "Log Length", title_offset = 35,properties = axis_props(
              title=list(fontSize=13))) %>%
            add_axis("y", title = "Log Weight", title_offset = 55,properties = axis_props(
              title=list(fontSize=13)))
        }
  }) %>% bind_shiny("lm_plot")
  
  output$ggvis_lm_plot <- renderUI({
    ggvisOutput("lm_plot")
  })
  
  # Create table for the power function parameter estimates
  output$nlm_call <- renderTable({
    result <- try({
      model <- nls(log(wt)~log(a*tl^b),start=list(a=0.000001,b=3),data=length_weight())
      results <- coef(summary(model))
      rownames(results) <- c("a","b")
      colnames(results) <- c("Estimate","Std. Error","t-value","p-value")
      results
    },silent=TRUE)
    
    if(class(result) != "try-error") {
      model <- nls(log(wt)~log(a*tl^b),start=list(a=0.000001,b=3),data=length_weight())
      results <- coef(summary(model))
      rownames(results) <- c("a","b")
      colnames(results) <- c("Estimate","Std. Error","t-value","p-value")
      results
    } else {
      results <- data_frame(a = c(0,0))
      rownames(results) <- c("a","b")
      colnames(results) <- c("Model was unable to fit")
      results
    }
  },digits=8)
  
  # Calculate fit of the power function
  nlm_fit <- reactive({
    result <- try({
      model <- nls(log(wt)~log(a*tl^b),start=list(a=0.000001,b=3),data=length_weight())
      coef(model)
    },silent=TRUE)
    
    if(class(result) != "try-error") {
      model <- nls(log(wt)~log(a*tl^b),start=list(a=0.000001,b=3),data=length_weight())
      coef(model)
    } else {
     c(0,0)
    }
  })

  # A reactive expression with the power function fit plot
  reactive({
    
    cf <- nlm_fit()
    if(nrow(length_weight()) > 0) {
      nlm <- data.frame(tl=seq(min(length_weight()$tl),max(length_weight()$tl),0.5))
      nlm %<>% mutate(wt=cf[1]*tl^cf[2]) %>%
        arrange(tl)
    } else {
      nlm <- data_frame(tl = 0, wt = 0, text = "No Data")
    }
    
    if(nrow(length_weight()) > 0) {
      ggvis(data=length_weight(),~tl, ~wt) %>%
        layer_points(size := 50,fillOpacity := 0.2) %>%
        layer_paths(data=nlm,~tl,~wt) %>%
        add_axis("x", title = "Total Length (mm)", title_offset = 35,properties = axis_props(
          title=list(fontSize=13))) %>%
        add_axis("y", title = "Weight (g)", title_offset = 55,properties = axis_props(
          title=list(fontSize=13))) } else {
            ggvis(data=nlm,~tl, ~wt) %>%
            layer_points(~text) %>%
            add_axis("x", title = "Total Length (mm)", title_offset = 35,properties = axis_props(
              title=list(fontSize=13))) %>%
            add_axis("y", title = "Weight (g)", title_offset = 55,properties = axis_props(
              title=list(fontSize=13))) }
  }) %>% bind_shiny("nlm_plot")
  
  output$ggvis_nlm_plot <- renderUI({
    ggvisOutput("nlm_plot")
  })

  # Sample size text output
  output$n_fish <- renderText({ nrow(length_weight()) })

  # Download
  output$downloadCSV_4 <- downloadHandler(
    filename="weight_length_data",
    content=function(file) {
      write.csv(length_weight(),file,row.names=FALSE)
    },
    contentType="text/csv"
  )

## -----------------------------------------------------------
## Lenght Frequency Data Manipulation and Histogram
## -----------------------------------------------------------
  
  # Filter length frequency, returning a data frame
  len_freq <- reactive({
    
    # Filter the lengths
    minlength2 <- input$min_val2
    maxlength2 <- input$max_val2
    
    len <- wb_exp %>% 
      filter(
        tl_exp >= minlength2,
        tl_exp <= maxlength2)
    
    # Optional: filter by year
    if (!is.null(input$year) && input$year != "") {
      len <- len[len$year == input$year,]
    }
    # Optional: filter by season
    if (!is.null(input$season) && input$season != "") {
      len <- len[len$season == input$season,]
    } 
    # Optional: filter by species
    if (!is.null(input$species) && input$species != "") {
      len <- len[len$species == input$species,]
    }
    len <- as.data.frame(len)
  })

  #make dynamic slider
  output$slider2 <- renderUI({
    sliderInput("inSlider2", "Slider", min=input$min_val2, max=input$max_val2)
  })

  # A reactive expression with the length frequency plot
  reactive({
    
    # Lables for axes
    xvar_name <- "Total Length (mm)"
    yvar_name <- "Frequency"
    
    if(nrow(len_freq()) < 1) {
      len_freq <- data_frame(tl_exp=0,n=0,text="No Data")
    }
    
    if(nrow(len_freq()) > 1) {
    len_freq %>%
      ggvis(~tl_exp) %>%
      add_axis("x", title = xvar_name, title_offset = 35, properties = axis_props(
        title=list(fontSize=13))) %>%
      add_axis("y", title = yvar_name, title_offset = 55, properties = axis_props(
        title=list(fontSize=13))) %>%
      layer_histograms(width = input$slider1) } else {
        ggvis(data=len_freq,~tl_exp,~n) %>%
          layer_points(~text) %>%
          add_axis("x", title = xvar_name, title_offset = 35, properties = axis_props(
            title=list(fontSize=13))) %>%
          add_axis("y", title = yvar_name, title_offset = 55, properties = axis_props(
            title=list(fontSize=13)))
      }
    }) %>% bind_shiny("len_freq_plot")
  
  output$ggvis_hist <- renderUI({
    ggvisOutput("len_freq_plot")
  })
  
  # Sample size text output
  output$n_size <- renderText({ nrow(len_freq()) })

  # Download
  output$downloadCSV_5 <- downloadHandler(
    filename="length_frequency_data",
    content=function(file) {
      write.csv(len_freq(),file,row.names=FALSE)
    },
    contentType="text/csv"
  )
})