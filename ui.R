library(shiny)

shinyUI(fluidPage(
  fluidRow(
    column(12,
           tags$img(src = "https://raw.githubusercontent.com/taylorstewart/lebs-western-basin/master/usgs_banner.tif",height="100px",width="100%")
           )
    ),
  HTML("<h2>Put Catchy Name Here!</h2>"),
  
  # Create a new panel for the historical time series plot and table.
  HTML("<br><h3>Historical Time Series</h3>"),
  HTML("<br><h4>Total Catch</h4>"),
  fluidRow(
    column(3,
           wellPanel(
             selectInput("species2",h5("Species Input"),c("All",species_vars))
           )
    ),
    column(9,
           htmlOutput("ggvis_time")
    )
  ),
  #fluidRow(
  #         dataTableOutput(outputId="table")
  #),
  HTML("<br><h4>Forage Density</h4>"),
  fluidRow(
    column(3),
    column(9,
           htmlOutput("ggvis_ftg")
    )
  ),

  # Create a new panel for input selectors.
  HTML("<br><h3>Reactive Plots</h3>"),
  fluidRow(
    column(3,
           selectInput("year",label=h5("Year Input"),year_vars,selected="2014")
           ),
    column(3,
           selectInput("season",label=h5("Season Input"),c("Spring","Autumn"),selected="Autumn")
           ),
    column(3,
           selectInput("species",label=h5("Species Input"),species_vars,selected="Yellow Perch")
    ),
    column(3,
           selectInput("serial",label=h5("Station Input"),c("All",serial_vars),selected="All")
    )
  ),
  
  # Create a new panel for the western basin map.
  HTML("<br><h5>Lake Erie Western Basin Map</h5>"),
  fluidRow(
    column(3,
      wellPanel(
        radioButtons("density",h5("Value:"),
                     c("Density (N/ha)" = "NperHA",
                       "Biomass (Kg/ha)" = "KgperHA")),
        selectInput("life_stage",h5("Life Stage"),c("All Life Stages",life_vars),selected="All Life Stages")
      )
    ),
    column(9,
           h5("Hover over point to display station number and detailed density value."),
           htmlOutput("ggvis_map")
    )
  ),
  
  # Create a new panel for lenght-weight plot and summarys.
  HTML("<br><h5>Length-Weight</h5>"),
  fluidRow(
    column(3,
           wellPanel(
             sliderInput("tl",h5("Length Range (mm)"),0,1000,step=5,value = c(0, 1000)),
             selectInput("xvar",h5("X-axis Variable"), axis_vars,selected="tl"),
             selectInput("yvar",h5("Y-axis Variable"), axis_vars,selected="wt")
             )
          ),
    column(9,
           tabsetPanel(type="tabs",
                       tabPanel("Length-Weight Plot",htmlOutput("ggvis_lw_plot")),
                       tabPanel("Linear Regression Summary",
                                withMathJax("$$\\log(W)=\\log(a)+b\\log(L)$$"),
                                tableOutput("lm_call"),
                                htmlOutput("ggvis_lm_resid")),
                       tabPanel("Power Function Summary",
                                ("$$W=aL^b$$"),
                                tableOutput("nlm_call"),
                                htmlOutput("ggvis_nlm_resid"))
           ),
           wellPanel(
             span("Number of individuals selected:",
                  textOutput("n_fish")
             )
           )
    )
  ),
  
  # Create a new panel for the length-frequency.
  HTML("<br><h5>Length Frequency</h5>"),
  fluidRow(
    column(3,
      wellPanel(
        sliderInput("slider1",label=h5("Bin Width (mm)"),min=5,max=25,step=5,value=10))),
    column(9,
      htmlOutput("ggvis_hist"),
      wellPanel(
        span("Number of individuals selected:",
             textOutput("n_size")
        )
      )
    )
  ),
  
  ## USGS Reference and Disclaimer
  HTML("<br><br><p><i>U.S. Geological Survey</i> (USGS) Computer Program <b>XXXX</b> version 2015-01.
    Written by Taylor R. Stewart (email: trstewart@usgs.gov),"),
    tags$a(href="http://www.glsc.usgs.gov","USGS - Great Lakes Science Center,"),
    HTML("Ann Arbor, Michigan, USA. Written in programming language R (R Core Team, 2015,"),
    tags$a(href="http://www.r-project.org","www.r-project.org"), 
    HTML(") version 3.1.2 (2014-10-31).</p>"),
  HTML("<p><i>Disclaimer:</i> Although this program has been used by the USGS, no warranty, expressed or implied, 
    is made by the USGS or the United States Government as to the accuracy and functioning of the program 
    and related program material nor shall the fact of distribution constitute any such warranty, and no 
    responsibility is assumed by the USGS in connection therewith.</p>")
)
)