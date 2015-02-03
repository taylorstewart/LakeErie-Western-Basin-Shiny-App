# Define server logic required
shinyServer(function(input, output, session) {
  
## -----------------------------------------------------------
## Historical Time Series Data Manipulation and Plot
## -----------------------------------------------------------

  # Filter catch, returning a data frame
  time_data <- reactive({
    
    l <- rename(catchHA,species=species,Year=year,Season=season,NperHA=NperHA)
        
    # Optional: filter by species
    if (!is.null(input$species2) && input$species2 != "All") {
      l <- l[l$species == input$species2,]
    }

    # Summarize total catch by year and season
    l %<>%
      group_by(Year,Season) %>%
      summarise(density=mean(NperHA)) %>%
      tbl_df()
  })
  
  # A reactive expression with the historical time series plot
  reactive({
    
    time_data %>% group_by(Season) %>%
    ggvis(~factor(Year),~density) %>%
      layer_points(fill = ~Season,prop("size",80)) %>%
      layer_lines(stroke = ~Season,prop("strokeWidth",2)) %>%
      add_axis("x",title="Year",ticks=1,title_offset=35) %>%
      add_axis("y",title="Mean Catch Per Hectare Swept",title_offset=65)
  }) %>% bind_shiny("time")
  
  output$ggvis_time <- renderUI({
    ggvisOutput("time")
  })

## -----------------------------------------------------------
## Catch Data Manipulation and Table Output
## -----------------------------------------------------------
  output$table <- renderDataTable(options = list(pageLength = 10),{
    catch_tbl <- catch
    
    # Optional: filter by species
    if (!is.null(input$species2) && input$species2 != "All") {
      catch_tbl <- catch_tbl[catch_tbl$Species == input$species2,]
    }
    
    #Summarize total count by species, year, and season
    catch_tbl %<>%
      rename(species=species,count=count_final,year=year,season=season) %>%
      group_by(species,year,season) %>%
      summarise(count=round(sum(count),0)) %>%
      rename(Species=species,Year=year,Season=season,Total_Count=count) %>%
      tbl_df()
  })

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
        layer_bars(width=0.5,prop("fill",~class)) %>%
        #layer_points(prop("fill",~class),prop("size",80)) %>%
        #layer_lines(stroke = ~class,prop("strokeWidth",2)) %>%
        hide_legend("stroke") %>%
        add_legend("fill",title="Functional Groups") %>%
        add_axis("x",title="Year",ticks=1,title_offset=35) %>%
        add_axis("y",title="Mean Catch Per Hectare Swept",title_offset=65) %>%
        add_tooltip(tooltip2, "hover")
  }) %>% bind_shiny("ftg")
  
  output$ggvis_ftg <- renderUI({
    ggvisOutput("ftg")
  })

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
    tooltip <- function(x) {
      if (is.null(x)) return(NULL)
      if (is.null(unique(x$serial))) return(NULL)
      
    # Pick out the individual with this ID
      wb <- isolate(map_data())
      map_data <- wb[unique(wb$serial) == unique(x$serial),]
      
      paste0("<b>","Station: ",map_data$serial,"<br>","Density (N/ha): ",map_data$NperHA,"<br>","Biomass (Kg/ha): ",map_data$KgperHA)
    }

  # A reactive expression with the western basin map
  observe(autoDestroy=TRUE, {
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
                     fillOpacity:=0.6) %>% #,fillOpacity.hover:=1) %>%
        add_legend("size","fill",title="Value",
                   values=factor(c(25,50,100,250,500,1000,1500,2000),labels=c("25","50","100","250","500","1000","1500","2000")),
                   properties=legend_props(
                     symbol=list(fill="black"))) %>%
        #add_tooltip(tooltip, "hover") %>%
        scale_numeric("x",domain=c(-83.514,-82.12),nice=FALSE) %>%
        scale_numeric("y",domain=c(41.306,42.103),nice=FALSE) %>%
        scale_numeric("size",domain=c(25,2000),range=c(25,2000),clamp=TRUE) %>%
        add_axis("x",title="",ticks="",tick_size_end="") %>%
        add_axis("x",orient="top",title="",ticks="",tick_size_end="") %>%
        add_axis("y",title="",ticks="",tick_size_end="") %>%
        add_axis("y",orient="right",title="",ticks="",tick_size_end="") %>% bind_shiny("map","ggvis_map")
  })

## -----------------------------------------------------------
## Lenght-Weight Data Manipulation and Plot
## -----------------------------------------------------------

  # Filter, returning a data frame
  length_weight <- reactive({
    
    # Filter the lengths
    minlength <- input$tl[1]
    maxlength <- input$tl[2]
    
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
  
  # A reactive expression with the lenght-weight plot
  reactive({
    
    if (!is.null(input$datatrans) && input$datatrans != "None") {
      length_weight %<>% transmute(tl=log(tl),wt=log(wt))
      xvar_name <- names(axis_vars)[3]
      yvar_name <- names(axis_vars)[4]
    } else {
      xvar_name <- names(axis_vars)[1]
      yvar_name <- names(axis_vars)[2]
    }
    
    length_weight %>%
      ggvis(~tl, ~wt) %>%
      layer_points(size := 50,fillOpacity := 0.2) %>%
      add_axis("x", title = xvar_name, title_offset = 35) %>%
      add_axis("y", title = yvar_name, title_offset = 55)
  }) %>% bind_shiny("lw_plot")
  
  output$ggvis_lw_plot <- renderUI({
    ggvisOutput("lw_plot")
  })
  
  # Create table for the linear regression parameter estimates
  output$lm_call <- renderTable({
    model <- lm(logw~logl,data=length_weight())
    results <- coef(summary(model))
    rownames(results) <- c("log(a)","b")
    colnames(results) <- c("Estimate","Std. Error","t-value","p-value")
    results
  },digits=12)
  
  # Calculate residuals of the linear regression
  lm_resid <- reactive({
    model <- lm(logw~logl,data=length_weight())
    resid <- data.frame(resid(model))
    colnames(resid) <- "resid"
    resid <- bind_cols(length_weight(),resid) %>%
      tbl_df()
  })
  
  # A reactive expression with the linear regression residual plot
  reactive({
    
    ggvis(data=lm_resid,~tl,~resid) %>%
      layer_points() %>%
      add_axis("x",title="Total Length (mm)") %>%
      add_axis("y",title="Residuals")
  }) %>% bind_shiny("lm_resid_plot")
  
  output$ggvis_lm_resid <- renderUI({
    ggvisOutput("lm_resid_plot")
  })
  
  # Create table for the power function parameter estimates
  output$nlm_call <- renderTable({
    st <- coef(lm(logw~logl,data=length_weight()))
    names(st) <- c("a","b")
    model <- nls(wt~a*tl^b,start=st,control=nls.control(maxiter=1000),data=length_weight())
    results <- coef(summary(model))
    rownames(results) <- c("a","b")
    colnames(results) <- c("Estimate","Std. Error","t-value","p-value")
    results
  },digits=12)
  
  # Calculate residuals of the power function
  nlm_resid <- reactive({
    st <- coef(lm(logw~logl,data=length_weight()))
    names(st) <- c("a","b")
    model <- nls(wt~a*tl^b,start=st,control=nls.control(maxiter=1000),data=length_weight())
    resid <- data.frame(resid(model))
    colnames(resid) <- "resid"
    resid <- bind_cols(length_weight(),resid) %>%
      tbl_df()
  })
  
  # A reactive expression with the power function residual plot
  reactive({
    
    ggvis(data=nlm_resid,~tl,~resid) %>%
      layer_points() %>%
      add_axis("x",title="Total Length (mm)") %>%
      add_axis("y",title="Residuals")
  }) %>% bind_shiny("nlm_resid_plot")
  
  output$ggvis_nlm_resid <- renderUI({
    ggvisOutput("nlm_resid_plot")
  })
  
  # Sample size text output
  output$n_fish <- renderText({ nrow(length_weight()) })

## -----------------------------------------------------------
## Lenght Frequency Data Manipulation and Histogram
## -----------------------------------------------------------
  
  # Filter length frequency, returning a data frame
  len_freq <- reactive({
    
    # Filter the lengths
    minlength2 <- input$tl2[1]
    maxlength2 <- input$tl2[2]
    
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
  
  # A reactive expression with the length frequency plot
  reactive({
    
    # Lables for axes
    xvar_name <- "Total Length (mm)"
    yvar_name <- "Frequency"
    
    len_freq %>%
      ggvis(~tl_exp) %>%
      add_axis("x", title = xvar_name, title_offset = 35) %>%
      add_axis("y", title = yvar_name, title_offset = 55) %>%
      layer_histograms(width = input$slider1)
  }) %>% bind_shiny("len_freq_plot")
  
  output$ggvis_hist <- renderUI({
    ggvisOutput("len_freq_plot")
  })
  
  # Sample size text output
  output$n_size <- renderText({ nrow(len_freq()) })
})