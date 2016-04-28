##############################################################
##############################################################
##
##  LEBS Shiny Application
##
##  UI SCRIPT (User Interface)
##
##############################################################
##############################################################
## -----------------------------------------------------------
## Define user interface required
## -----------------------------------------------------------
shinyUI(fluidPage(
  tags$head(includeScript("www/google-analytics.js")),
  HTML("<p>"),
  fluidRow(
    column(12,
           tags$img(src="usgs_banner_gr.jpg",height="120px",width="100%")
           )
    ),
  HTML("<h2>Lake Erie Fish Community Data Explorer</h2>"),
  HTML("<p>&nbsp;&nbsp;&nbsp;&nbsp;This data exploration tool is intented for use by researchers, resource managers, and the public
       to better understand the status of the fish community in Lake Erie.  The data were collected as part of a scientific trawl survey
       to quantify trends in the distribution and abundance of forage and other fish species.  The survey is responsive to the "),
       tags$a(href="http://www.glfc.org/fishmgmt/jsp97.pdf",HTML(paste0(tags$span(style="color:royalblue","Joint 
       Strategic Plan for Fishery Management ")))),
       HTML("in the Great Lakes and to a Memorandum of Understanding between the Great Lakes "),
            tags$a(href="http://www.glfc.org/boardcomm/clc/clchome.php",HTML(paste0(tags$span(style="color:royalblue",
            "Council of Lake Committees ")))),
       HTML("and the US Geological Survey. This tool lets the user explore, plot and download summarized results. 
             The official data sets with complete descriptions of the methods and metadata 
             are available in a publically accessible permanent repository on "),tags$a(href="https://www.sciencebase.gov/catalog/item/56951c63e4b039675d005ed9",
         HTML(paste0(tags$span(style="color:royalblue","sciencebase.gov"),".</p>"))),
  HTML("<p>&nbsp;&nbsp;&nbsp;&nbsp;Please note the disclaimer and citation of this work at the bottom of this page, and
         consider providing an email contact (via the link below) that will allow us to notify you of any updates to the data.</p>"),
  tags$a(href="mailto:rkraus@usgs.gov?Subject=Western Basin Data Downloaded",style="text-decoration:none !important;",
         HTML(paste(tags$span(style="color:royalblue","Send Notification Email")))),
  HTML("<br><br>"),
  
  ## -----------------------------------------------------------
  ## Create a new panel for the historical time series plot and table.
  ## -----------------------------------------------------------
  HTML("<h3>Abundance</h3>"),
  fluidRow(
    column(3,
           HTML("<h5>Historical Time-Series Abundance</h5>"),
           wellPanel(
             selectInput("species2",h5("Select a Species:"),species_vars,selected="Walleye"),
             selectInput("life.stage2",h5("Select a Life Stage:"),c("All Life Stages",life_vars),selected="All Life Stages"),
             downloadButton("downloadCSV_1","Download Plot Data")
           )
    ),
    column(9,align="center",
           HTML("Hover over points to display detailed density values."),
           ggvisOutput("time_n"),
           uiOutput("ggvis_n_time"),
           ggvisOutput("time_kg"),
           uiOutput("ggvis_kg_time"),
           htmlOutput("catch_label")
    )
  ),
  HTML("<br><br>"),
  
  ## -----------------------------------------------------------
  ## 
  ## -----------------------------------------------------------
  HTML("<h5>Dominant Species Abundance</h5>"),
  fluidRow(
    column(3,
           wellPanel(
             selectInput("year2",label=h5("Select a Year:"),year_vars,selected="2015"),
             selectInput("season2",label=h5("Select a Season:"),c("Spring","Autumn"),selected="Autumn"),
             downloadButton("downloadCSV_8","Download Plot Data")
           )
    ),
    column(9,align="center",
           ggvisOutput("density_bar"),
           uiOutput("ggvis_density_bar"),
           ggvisOutput("biomass_bar"),
           uiOutput("ggvis_biomass_bar"),
           htmlOutput("rank_label")
    )
  ),
  HTML("<br><br>"),

  ## -----------------------------------------------------------
  ## 
  ## -----------------------------------------------------------
  HTML("<h5>Historical Forage Abundance</h5>"),
  fluidRow(
    column(3,
           wellPanel(
             HTML("<p>Functional groups, as defined by the Lake Erie Committee Forage Task Group, are inclusive of the following species:</p>"),
             HTML("<p>Clupeids: Age-0 Gizzard Shad (<i>Dorosoma cepedianum</i>) and Alewife (<i>Alosa pseudoharengus</i>)</p>
                  <p>Soft-rayed fish: Rainbow Smelt (<i>Osmerus mordax</i>), Emerald Shiner (<i>Notropis atherinoides</i>), Spottail Shiner (<i>Notropis hudsonius</i>), 
                  Silver Chub (<i>Macrhybopsis storeriana</i>), Trout-perch (<i>Percopsis omiscomaycus</i>), Round Goby (<i>Neogobius melanostomus</i>), and other cyprinids</p>
                  <p>Spiny-rayed fish: Age-0 for each of White Perch (<i>Morone americana</i>), White Bass (<i>Morone chrysops</i>), Yellow Perch (<i>Perca flavescens</i>), 
                  Walleye (<i>Sander vitreus</i>), and Freshwater Drum (<i>Aplodinotus grunniens</i>)</p>"),
             downloadButton("downloadCSV_2","Download Plot Data")
           )
    ),
    column(9,align="center",
           HTML("Hover over points to display detailed density values."),
           ggvisOutput("ftg_n"),
           uiOutput("ggvis_n_ftg"),
           ggvisOutput("ftg_kg"),
           uiOutput("ggvis_kg_ftg"),
           HTML("Mean density (N/ha; top) and biomass (Kg/ha; bottom) by functional group in Ontario, Michigan, 
                and Ohio waters in the western basin of Lake Erie. Restricted to Autumn sampling. Dashed lines indicate long-term means for each functional group.")
    )
  ),
  HTML("<br><br>"),
  
  ## -----------------------------------------------------------
  ## 
  ## -----------------------------------------------------------
  HTML("<h3>Abiotic</h3>"),
  HTML("<h5>Water Quality Parameters</h5>"),
  fluidRow(
    column(3,
           wellPanel(
             selectInput("parameter",h5("Parameters:"),par_vars),
             selectInput("year3",label=h5("Year:"),year_vars,selected="2015"),
             selectInput("season3",label=h5("Season:"),c("Spring","Autumn"),selected="Autumn"),
             downloadButton("downloadCSV_7","Download Table Data")
           )
    ),
    column(9,align="center",
           htmlOutput("wq_table_label"),
           HTML("<br>"),
           dataTableOutput("abiotic_table")
    )
  ),
  HTML("<br><br>"),

  ## -----------------------------------------------------------
  ## Create a new panel for input selectors
  ## -----------------------------------------------------------
  HTML("<h3>Dynamic Plots</h3><h6>Please allow adequate time for browser to update plots.</h6>"),
  fluidRow(
    column(12,align="center",
           wellPanel(HTML("<h4>Select Inputs for Plots Below</h4>"),
                     tags$div(class="row",
                              tags$div(class="col-sm-4",
                                       selectInput("year",label=h5("Year"),year_vars,selected="2015")
                              ),
                              tags$div(class="col-sm-4",
                                       selectInput("season",label=h5("Season"),c("Spring","Autumn"),selected="Autumn")
                              ),
                              tags$div(class="col-sm-4",
                                       selectInput("species",label=h5("Species"),species_vars,selected="Walleye")
                              )
                     )
           )
    )
  ),
  HTML("<br>"),
  
  ## -----------------------------------------------------------
  ## Create a new panel for the western basin map.
  ## -----------------------------------------------------------
  HTML("<h5>Lake Erie Western Basin Map</h5>"),
  fluidRow(
    column(3,
           wellPanel(
             selectInput("life.stage",h5("Life Stage:"),c("All Life Stages",life_vars),selected="All Life Stages"),
             htmlOutput("map_ls_label"),
             HTML("<br>"),
             downloadButton("downloadCSV_3","Download Plot Data")
           )
    ),
    column(9,align="center",
           HTML("Hover over point to display station number and detailed value for each plot."),
           ggvisOutput("density_map"),
           uiOutput("ggvis_denisty_map"),
           ggvisOutput("biomass_map"),
           uiOutput("ggvis_biomass_map"),
           htmlOutput("map_label")
    )
  ),
  HTML("<br><br>"),
  
  ## -----------------------------------------------------------
  ## Create a new panel for lenght-weight plot and summarys.
  ## -----------------------------------------------------------
  HTML("<h5>Weight-Length</h5>"),
  fluidRow(
    column(3,
           wellPanel(
             #Numeric Inputs
             numericInput("min_val",h5("Enter Minimum Length (mm):"),0),
             numericInput("max_val",h5("Enter Maximum Length (mm):"),1500),
             #display dynamic UI
             uiOutput("inSlider"),
             radioButtons("datatrans",h5("Transformation:"),
                          c("None" = "None",
                            "Natural Log" = "Linear")),
             downloadButton("downloadCSV_4","Download Plot Data")
           )
    ),
    column(9,align="center",
           ggvisOutput("lw_plot"),
           uiOutput("ggvis_lw_plot"),
           htmlOutput("reg_plot_label"),
           HTML("<br>"),
           htmlOutput("reg_tbl_label"),
           withMathJax("$$W=aL^b$$"),
           withMathJax("$$\\log(W)=\\log(a)+b\\log(L)$$"),
           tableOutput("reg_tbl"),
           wellPanel(
             span("Number of individuals selected:",textOutput("n_fish")
             )
           )
    )
  ),
  HTML("<br><br>"),
  
  ## -----------------------------------------------------------
  ## Create a new panel for the length-frequency.
  ## -----------------------------------------------------------
  HTML("<h5>Length Frequency</h5>"),
  fluidRow(
    column(3,
           wellPanel(
             #Numeric Inputs
             numericInput("min_val2",h5("Enter Minimum Length (mm):"),0),
             numericInput("max_val2",h5("Enter Maximum Length (mm):"),1500),
             #display dynamic UI
             uiOutput("inSlider2"),
             sliderInput("slider1",label=h5("Bin Width (mm):"),ticks=F,min=5,max=25,step=5,value=10),
             downloadButton("downloadCSV_5","Download Plot Data")
           )
    ),
    column(9,align="center",
           ggvisOutput("hist"),
           uiOutput("ggvis_hist"),
           htmlOutput("len_freq_label"),
           HTML("<br>"),
           wellPanel(
             span("Number of individuals selected:",
                  textOutput("n_size")
             )
           )
    )
  ),
  HTML("<br><br>"),
  
  ## -----------------------------------------------------------
  ## Metadata Link
  ## -----------------------------------------------------------
  HTML("A link to the metadata for completed surveys and data files can be found"),
  tags$a(href="https://www.sciencebase.gov/catalog/item/56951c63e4b039675d005ed9",
         HTML(paste0(tags$span(style="color:royalblue","here"),"."))),
  HTML("<br><br>"),
  
  ## -----------------------------------------------------------
  ## USGS Reference and Disclaimer
  ## -----------------------------------------------------------
  HTML("<p>Written by Taylor R. Stewart"),
  tags$a(href="mailto:taylor.stewart@uvm.edu?Subject=LEBS - Western Basin Site Question/Comment/Report",style="text-decoration:none !important;",
         HTML(paste0(tags$span(style="color:black","("),
                    tags$span(style="color:royalblue","taylor.stewart@uvm.edu"),
                    tags$span(style="color:black",").")))),
  HTML("U.S. Geological Survey, Great Lakes Science Center, Lake Erie Biological Station, Sandusky, Ohio. 
       Written in programming language R (R Development Core Team, 2015. Vienna, Austria."),
    tags$a(href="http://www.r-project.org",style="text-decoration:none !important;",HTML(paste(tags$span(style="color:royalblue","www.r-project.org"),tags$span(style="color:black",")"),sep=""))), 
    HTML("version 3.2.2 (2015-08-14).</p>"),
    HTML("<p><i>Disclaimer:</i> Although this program has been used by the USGS, no warranty, expressed or implied, 
    is made by the USGS or the United States Government as to the accuracy and functioning of the program 
    and related program material nor shall the fact of distribution constitute any such warranty, and no 
    responsibility is assumed by the USGS in connection therewith.</p>")
)
)
## The End!