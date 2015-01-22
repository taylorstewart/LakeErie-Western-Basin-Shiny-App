library(ggvis)
library(RSQLite)

# Define server logic required
shinyServer(function(input, output, session) {
  
  # Filter, returning a data frame
  data <- reactive({
      
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
    if (!is.null(input$serial) && input$serial != "All") {
      m <- m[m$serial == input$serial,]
    }
    
    # Optional: filter by species
    if (!is.null(input$species) && input$species != "") {
      m <- m[m$species == input$species,]
    }
    
  m <- as.data.frame(m)
    
})

  # Filter, returning a data frame
  data2 <- reactive({
    
    # Filter the lengths
      minlength <- input$tl[1]
      maxlength <- input$tl[2]
      
      len <- wb_exp %>% 
        filter(
          tl_exp >= minlength,
          tl_exp <= maxlength)
    
    
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

  output$table <- renderDataTable(options = list(pageLength = 10),{
    data3 <- catch
    
    # Optional: filter by species
    if (!is.null(input$species2) && input$species2 != "All") {
      data3 <- data3[data3$Species == input$species2,]
    }
    
    colnames(data3) <- c("Station","Species","Size","Weight","Total_Count","Year","Season")
    
    data3 %<>%
      group_by(Species,Year,Season) %>%
      summarise(count=round(sum(Total_Count),0)
      )
    
    colnames(data3) <- c("Species","Year","Season","Total Count")
        
    data3 <- as.data.frame(data3)
})

  # Filter, returning a data frame
  data4 <- reactive({
  
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
    
     c %<>%
       group_by(serial) %>%
       summarise(NperHA=sum(NperHA),
                 KgperHA=sum(KgperHA),
                 long=mean(long),
                 lat=mean(lat)
         )
  
})
  
  # Filter, returning a data frame
  data5 <- reactive({
    
    l <- catch
    
    colnames(l) <- c("serial","species","size","weight","count","year","season")
      
    # Optional: filter by species
    if (!is.null(input$species2) && input$species2 != "All") {
      l <- l[l$species == input$species2,]
    }

    l %<>%
      group_by(year,season) %>%
      summarise(count=sum(count)
      )
        
    colnames(l) <- c("Year","Season","Count")
    
    l <- as.data.frame(l)
    
  })

  # Function for generating serial tooltip text
  tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(unique(x$serial))) return(NULL)
    
    # Pick out the individual with this ID
    wb <- isolate(data4())
    data4 <- wb[unique(wb$serial) == unique(x$serial),]
    
    paste0("<b>","Station: ",data4$serial,"<br>","Density (N/ha): ",data4$NperHA,"<br>","Biomass (Kg/ha): ",data4$KgperHA)
  }

  # A reactive expression with the ggvis plot
  vis <- reactive({
    
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    data %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50,fillOpacity := 0.2) %>%
      add_axis("x", title = xvar_name, title_offset = 35) %>%
      add_axis("y", title = yvar_name, title_offset = 55)
    })
  
  vis %>% bind_shiny("plot1")

  output$ggvis_plot <- renderUI({
    ggvisOutput("plot1")
    })
  
  # A reactive expression with the ggvis plot
  vis2 <- reactive({
    
    # Lables for axes
    xvar_name <- "Total Length (mm)"
    yvar_name <- "Frequency"
    
    data2 %>%
      ggvis(~tl_exp) %>%
        add_axis("x", title = xvar_name, title_offset = 35) %>%
        add_axis("y", title = yvar_name, title_offset = 55) %>%
        layer_histograms(width = input$slider1)
  })

  vis2 %>% bind_shiny("plot2")
  
  output$ggvis_hist <- renderUI({
    ggvisOutput("plot2")
  })

  # A reactive expression with the ggvis plot
  vis3 <- reactive({

    sizevar <- prop("size",as.symbol(input$density))
    
    ggvis(data=filter(wb_shore,piece=="1" & group=="3.1"),~long,~lat) %>%
      layer_paths() %>%
      layer_paths(data=filter(wb_shore,piece=="2"),~long,~lat) %>%
      layer_paths(data=filter(wb_shore,piece=="3"),~long,~lat) %>%
      layer_paths(data=filter(wb_shore,piece=="4"),~long,~lat) %>%
      layer_paths(data=filter(wb_shore,piece=="5"),~long,~lat) %>%
      layer_paths(data=filter(wb_shore,piece=="6"),~long,~lat) %>%
      layer_points(data=data4,~long,~lat,size:=sizevar,key:=~serial,fillOpacity:=0.6,fillOpacity.hover:=1) %>%
      add_tooltip(tooltip, "hover") %>%
      scale_numeric("x",domain=c(-83.514,-82.12),nice=FALSE) %>%
      scale_numeric("y",domain=c(41.306,42.103),nice=FALSE) %>%
      add_legend("size",title="Value") %>%
      add_axis("x",title="",ticks="",tick_size_end="") %>%
      add_axis("x",orient="top",title="",ticks="",tick_size_end="") %>%
      add_axis("y",title="",ticks="",tick_size_end="") %>%
      add_axis("y",orient="right",title="",ticks="",tick_size_end="")
      
  })

  vis3 %>% bind_shiny("plot3")

  output$ggvis_plot2 <- renderUI({
    ggvisOutput("plot3")
  })

  # A reactive expression with the ggvis plot
  vis4 <- reactive({
      
    ggvis(data=data5,~factor(Year),~Count) %>%
      layer_points(fill = ~Season) %>%
      group_by(Season) %>%
      layer_lines(stroke = ~Season) %>%
      add_axis("x",title="Year",ticks=1,title_offset=35) %>%
      add_axis("y",title="Total Count",title_offset=65)
  })
  
  vis4 %>% bind_shiny("plot4")
  
  output$ggvis_time <- renderUI({
    ggvisOutput("plot4")
  })

  output$n_fish <- renderText({ nrow(data()) })

  output$n_size <- renderText({ nrow(data2()) })

  output$lm_call <- renderTable({
    model <- lm(logw~logl,data=data())
    results <- coef(summary(model))
    rownames(results) <- c("Intercept","Slope")
    colnames(results) <- c("Estimate","Std. Error","t-value","p-value")
    results
  },digits=12)

  lm_resid <- reactive({
    model <- lm(logw~logl,data=data())
    resid <- data.frame(resid(model))
    colnames(resid) <- "resid"
    resid$n <- seq(1,nrow(resid))
    resid <- as.data.frame(resid)
  })

  # A reactive expression with the ggvis plot
  vis5 <- reactive({
    
    ggvis(data=lm_resid,~n,~resid) %>%
      layer_points() %>%
      add_axis("x",title="Index") %>%
      add_axis("y",title="Residuals")
  })
  
  vis5 %>% bind_shiny("plot5")
  
  output$ggvis_lm_resid <- renderUI({
    ggvisOutput("plot5")
  })

  output$nlm_call <- renderTable({
    model <- nls(wt~a*tl^b,start=c(a=1,b=1),control=nls.control(maxiter=1000),data=data())
    results <- coef(summary(model))
    rownames(results) <- c("Scaling Factor","Exponent")
    colnames(results) <- c("Estimate","Std. Error","t-value","p-value")
    results
  },digits=12)

  nlm_resid <- reactive({
    model <- nls(wt~a*tl^b,start=c(a=1,b=1),control=nls.control(maxiter=1000),data=data())
    resid <- data.frame(resid(model))
    colnames(resid) <- "resid"
    resid$n <- seq(1,nrow(resid))
    resid <- as.data.frame(resid)
  })
  
  # A reactive expression with the ggvis plot
  vis6 <- reactive({
    
    ggvis(data=nlm_resid,~n,~resid) %>%
      layer_points() %>%
      add_axis("x",title="Index") %>%
      add_axis("y",title="Residuals")
  })
  
  vis6 %>% bind_shiny("plot6")
  
  output$ggvis_nlm_resid <- renderUI({
    ggvisOutput("plot6")
  })
})
