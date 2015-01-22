library(shiny)

shinyUI(fluidPage(
  titlePanel(h3("USGS Lake Erie Biological Station - Western Basin Reactive Summary")),
  h4("Developed by: Taylor R. Stewart"),
  h5("Email: trstewart@usgs.gov"),
  
  # Create a new panel for the historical time series plot and table.
  fluidRow(),
  titlePanel(h3("Historical Time Series")),
  fluidRow(
    column(3,
           wellPanel(
             selectInput("species2","Species Input",c("All",species_vars))
           )
    ),
    column(9,
           htmlOutput("ggvis_time")
    )
  ),
  fluidRow(
           dataTableOutput(outputId="table")
  ),
  
  # Create a new panel for input selectors.
  fluidRow(),
  titlePanel(h3("Reactive Plots")),
  fluidRow(
    column(3,
           selectInput("year",label=h4("Year Input"),year_vars,selected="2014")
           ),
    column(3,
           selectInput("season",label=h4("Season Input"),c("Spring","Autumn"),selected="Autumn")
           ),
    column(3,
           selectInput("species",label=h4("Species Input"),species_vars,selected="Yellow Perch")
    ),
    column(3,
           selectInput("serial",label=h4("Station Input"),c("All",serial_vars),selected="All")
    )
  ),
  
  # Create a new panel for the western basin map.
  titlePanel(h5("Lake Erie Western Basin Map")),
  fluidRow(
    column(3,
      wellPanel(
        radioButtons("density","Value:",
                     c("Density (N/ha)" = "NperHA",
                       "Biomass (Kg/ha)" = "KgperHA")),
        selectInput("life_stage","Life Stage",c("All Life Stages",life_vars),selected="All Life Stages")
      )
    ),
    column(9,
           h5("Hover over point to display station number and detailed density value."),
           htmlOutput("ggvis_plot2")
    )
  ),
  
  # Create a new panel for lenght-weight plot and summarys.
  titlePanel(h5("Length-Weight")),
  fluidRow(
    column(3,
           wellPanel(
             sliderInput("tl", "Length Range (mm)",0,1000,step=5,value = c(0, 1000)),
             selectInput("xvar", "X-axis Variable", axis_vars,selected="tl"),
             selectInput("yvar", "Y-axis Variable", axis_vars,selected="wt")
             )
          ),
    column(9,
           tabsetPanel(type="tabs",
                       tabPanel("Length-Weight Plot",htmlOutput("ggvis_plot")),
                       tabPanel("Linear Model Summary",tableOutput("lm_call"),htmlOutput("ggvis_lm_resid")),
                       tabPanel("Power Function Summary",tableOutput("nlm_call"),htmlOutput("ggvis_nlm_resid"))
           ),
           wellPanel(
             span("Number of individuals selected:",
                  textOutput("n_fish")
             )
           )
    )
  ),
  
  # Create a new panel for the length-frequency.
  titlePanel(h5("Length Frequency")),
  fluidRow(
    column(3,
      wellPanel(
        sliderInput("slider1",label="Bin Width (mm)",min=5,max=25,step=5,value=10))),
    column(9,
      htmlOutput("ggvis_hist"),
      wellPanel(
        span("Number of individuals selected:",
             textOutput("n_size")
        )
      )
    )
  ),
  fluidRow(),
  h5("Source code available at: https://github.com/taylorstewart/lebs-western-basin")
)
)