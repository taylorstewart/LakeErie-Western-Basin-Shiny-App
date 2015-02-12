# Define user interface required
shinyUI(fluidPage(
  tags$head(includeScript("www/google-analytics.js")),
  fluidRow(
    column(12,
           tags$img(src="usgs_banner_gr.jpg",height="110px",width="100%")
           )
    ),
  HTML("<h2>Lake Erie Biological Station - Western Basin Trawl Survey</h2>"),
  HTML("<i>BETA VERSION STATEMENT:"),
  HTML("This data exploration tool is intended for use by Lake Erie fisheries managers, academia, the fishing industry and the public. 
       The data presented here have been checked for accuracy, but are still considered provisional at this time. 
       You may request a subset of the data by contacting us directly via email. 
       Please send questions, comments, suggestions for improvements, and error reports via email to USGS - Lake Erie Biological Station c/o "),
  tags$a(href="mailto:rkraus@usgs.gov, trstewart@usgs.gov?Subject=LEBS - Western Basin Site Question/Comment/Report",style="text-decoration:none !important;",
         HTML(paste(tags$span(style="color:black","Richard Kraus ("),
                    tags$span(style="color:royalblue","rkraus@usgs.gov"),
                    tags$span(style="color:black",")"),
                    tags$span(style="color:black"," and/or Taylor Stewart "),
                    tags$span(style="color:black","("),
                    tags$span(style="color:royalblue","trstewart@usgs.gov"),
                    tags$span(style="color:black",")."),sep=""))), 
  HTML("The current web location for this tool is temporary and it will be hosted on a USGS server as soon as a suitable one can be located.</i><br><br>"),
  
  HTML("<p>&nbsp;&nbsp;&nbsp;&nbsp;Lake Erie Biological Station (LEBS), located in Sandusky, Ohio, is a field station of the USGS Great Lakes Science Center (GLSC).
       LEBS is the primary federal agency for applied fisheries science excellence in Lake Erie. 
       Since 2004, LEBS has participated in a collaborative, multiagency effort to assess forage fish populations in the western basin of Lake Erie. 
       The objectives of this evaluation are to provide estimates of abundance of key forage and predator species, 
       to assess seasonal and spatial distributions of fishes, and assess year class strength of key forage and predator species in the western basin of Lake Erie. 
       In 2012, the original vessel used since 2004, the R/V Musky II, was retired and replaced with the R/V Muskie. 
       The change in vessel necessitated changing the gear used to capture fish. 
       Previous surveys used a different catch processing protocol that did not include measurements of biomass or lengths of all species; thus, those historical data are not compatible with this presentation format. 
       Under the new protocol, 41 stations were sampled during June (Spring) and September (Autumn). 
       The 2013 western basin survey season marked the first year in which the grid sampling design was employed in both spring and autumn. 
       Thus, we present data starting from 2013. This data presentation tool will automatically update with new data as surveys are completed in future years.</p>"),
  HTML("<p>&nbsp;&nbsp;&nbsp;&nbsp;The vessel used for the survey is the R/V Muskie, a 70’ aluminum mono-hull research vessel, with a double-warp trawl system. 
       The net is a four-seam, three-bridle bottom trawl, with a fishing circle of 200 x 12 (cm) meshes at the mouth of the net, and with ground gear consisting of a rock-hopper with 8” diameter rubber discs. 
       The cod-end liner is constructed of 14 (mm) knotless dyneema mesh. The net is towed at a target speed of 3 knots, and wingspread estimates are obtained on each tow with an acoustic mensuration system to standardize catches per area swept. 
       Typical net geometry was a 6 (m) wingspread and 3 (m) headline height on each tow. 
       Typical area swept was 0.5 hectares. 
       Individual lengths and weights were obtained from a subsample of each species’ size group. 
       Total counts for each species and size group were obtained by expansions of mean individual weight by the aggregate weight. 
       The exceptions to this method of enumeration were percids (Yellow Perch and Walleye), where every fish was counted, and size groups with low numbers (n<10), where each fish was measured.</p><br><br>"),
  
  # Create a new panel for the historical time series plot and table.
  HTML("<h3>Historical Time Series</h3>"),
  fluidRow(
    column(3,
           HTML("<h5>Catch</h5>"),
           wellPanel(
             selectInput("species2",h5("Select a Species:"),species_vars,selected="Yellow Perch"),
             downloadButton("downloadCSV_1","Download Plot Data")
           )
    ),
    column(9,align="center",
           HTML("Hover over points to display detailed density values."),
           ggvisOutput("time"),
           uiOutput("ggvis_time"),
           htmlOutput("catch_label"),
           HTML("<br>")
    )
  ),

  HTML("<h5>Forage Density</h5>"),
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
           ggvisOutput("ftg"),
           uiOutput("ggvis_ftg"),
           HTML("Mean catch per hectare swept by functional group in Ontario, Michigan, 
                and Ohio waters in the western basin of Lake Erie. Restricted to Autumn sampling.<br><br><br>")
    )
  ),

  # Create a new panel for input selectors.
  HTML("<h3>Dynamic Plots</h3><h6>Please allow adequate time for browser to update plots.</h6>"),
  fluidRow(
    column(12,align="center",
           wellPanel(HTML("<h4>Select Inputs for Plots Below</h4>"),
                     tags$div(class="row",
                              tags$div(class="col-sm-4",
                                       selectInput("year",label=h5("Year"),year_vars,selected="2014")
                              ),
                              tags$div(class="col-sm-4",
                                       selectInput("season",label=h5("Season"),c("Spring","Autumn"),selected="Spring")
                              ),
                              tags$div(class="col-sm-4",
                                       selectInput("species",label=h5("Species"),species_vars,selected="Yellow Perch")
                              )
                     )
           )
    )
  ),
  
  # Create a new panel for the western basin map.
  HTML("<br><br><h5>Lake Erie Western Basin Map</h5>"),
  fluidRow(
    column(3,
           wellPanel(
             radioButtons("density",h5("Value:"),
                          c("Density (N/ha)" = "NperHA",
                            "Biomass (Kg/ha)" = "KgperHA")),
             selectInput("life_stage",h5("Life Stage:"),c("All Life Stages",life_vars),selected="All Life Stages"),
             htmlOutput("map_ls_label"),
             HTML("<br>"),
             downloadButton("downloadCSV_3","Download Plot Data")
           )
    ),
    column(9,align="center",
           HTML("Hover over point to display station number and detailed density and biomass values."),
           ggvisOutput("map"),
           uiOutput("ggvis_map"),
           htmlOutput("map_label"),
           HTML("<br>"),
           HTML("<br>Biomass values for 2014 are still being calculated. They will be added as soon as possible.")
    )
  ),
  
  # Create a new panel for lenght-weight plot and summarys.
  HTML("<br><br><br><br><h5>Weight-Length</h5>"),
  fluidRow(
    column(3,
           wellPanel(
             #Numeric Inputs
             numericInput("min_val",h5("Enter Minimum Length (mm):"),0),
             numericInput("max_val",h5("Enter Maximum Length (mm):"),1000),
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
  
  # Create a new panel for the length-frequency.
  HTML("<br><br><br><br><h5>Length Frequency</h5>"),
  fluidRow(
    column(3,
           wellPanel(
             #Numeric Inputs
             numericInput("min_val2",h5("Enter Minimum Length (mm):"),0),
             numericInput("max_val2",h5("Enter Maximum Length (mm):"),1000),
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
  
  ## USGS Reference and Disclaimer
  HTML("<br><br><p>Written by Taylor R. Stewart"),
  tags$a(href="mailto:trstewart@usgs.gov?Subject=LEBS - Western Basin Site Question/Comment/Report",style="text-decoration:none !important;",
         HTML(paste0(tags$span(style="color:black","("),
                    tags$span(style="color:royalblue","trstewart@usgs.gov"),
                    tags$span(style="color:black",")."),sep=""))),
  HTML("U.S. Geological Survey, Great Lakes Science Center, Lake Erie Biological Station, Sandusky, Ohio. 
       Written in programming language R (R Development Core Team, 2015. Vienna, Austria."),
    tags$a(href="http://www.r-project.org",style="text-decoration:none !important;",HTML(paste(tags$span(style="color:royalblue","www.r-project.org"),tags$span(style="color:black",")"),sep=""))), 
    HTML("version 3.1.2 (2014-10-31).</p>"),
    HTML("<p><i>Disclaimer:</i> Although this program has been used by the USGS, no warranty, expressed or implied, 
    is made by the USGS or the United States Government as to the accuracy and functioning of the program 
    and related program material nor shall the fact of distribution constitute any such warranty, and no 
    responsibility is assumed by the USGS in connection therewith.</p>")
)
)