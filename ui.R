shinyUI(fluidPage(
  fluidRow(
    column(12,
           tags$img(src = "https://raw.githubusercontent.com/taylorstewart/lebs-western-basin/master/usgs_banner.png",height="80px",width="100%")
           )
    ),
  HTML("<h2>Western Basin Trawl Survey</h2>"),
  HTML("BETA VERSION STATEMENT:"),
  HTML("This data exploration tool is intended for use by Lake Erie fisheries managers, academia, the fishing industry and the public. 
       The data presented here have been checked for accuracy, but are still considered provisional at this time. 
       You may request a subset of these data by contacting us directly via email. We hope you will take time to send us suggestions on how to improve this tool. 
       Please send questions, comments, and error reports via email to USGS - Lake Erie Biological Station c/o Richard Kraus "),
  tags$span(style="color:royalblue", "(rkraus@usgs.gov)"),
  HTML("and/or Taylor Stewart "),
  tags$span(style="color:royalblue","(trstewart@usgs.gov)."),
  HTML("<br><br>"),
  
  HTML("<i><p>&nbsp;&nbsp;&nbsp;&nbsp;Lake Erie Biological Station (LEBS), located in Sandusky, Ohio, is a field station of the USGS Great Lakes Science Center (GLSC).
       LEBS is the primary federal agency for applied fisheries science excellence in Lake Erie. 
       Since 2004, LEBS has participated in a collaborative, multiagency effort to assess forage fish populations in the western basin of Lake Erie. 
       The objectives of this evaluation are to provide estimates of abundance of key forage and predator species, 
       to assess seasonal and spatial distributions of fishes, and assess year class strength of key forage and predator species in the western basin of Lake Erie. 
       In 2012, the original vessel used since 2004, the R/V Musky II, was retired and replaced with the R/V Muskie. 
       The change in vessel neccessitated change in the net and all of the hardware needed to tow. 
       Those changes essentially ended the original time series and began a new one. 
       Under the recently revised grid sampling design, we sample 41 stations during two sampling periods that occurr in June (Spring) and Spetember (Autumn). 
       The 2013 western basin survey season marked the first year in which the grid sampling design was employed in both spring and autumn. 
       Thus, we present data starting from 2013.</p>"),
  HTML("<p>&nbsp;&nbsp;&nbsp;&nbsp;The vessel used for the survey is the R/V Muskie, a 70’ aluminum mono-hull research vessel, with a double-warp trawl system. 
       The net is a four-seam, three-bridle, bottom trawl, with a fishing circle of 200 x 12cm meshes at the mouth of the net, and with ground gear consisting of a rock-hopper with 8” diameter floppy discs. 
       The cod end liner is constructed of 14 mm knotless dyneema mesh. The net is towed at a target speed of 3 knots, and wingspread estimates are obtained on each tow with an acoustic mensuration system to standardize catches per area swept. 
       Biomass was measured onboard with a motion compensating scale, and individual lengths and weights were obtained from sub-samples of each species’ size group. 
       Total counts were obtained by expansions of mean individual weight by the aggregate weight (by species and size group). 
       The exceptions to this method of enumeration were percids (Yellow Perch and Walleye), where we counted every fish, and size groups with low numbers (n<10), where each fish was measured.</p></i><br><br>"),
  
  # Create a new panel for the historical time series plot and table.
  HTML("<h3>Historical Time Series</h3>"),
  fluidRow(
    column(3,
           HTML("<h4>Catch</h4>"),
           wellPanel(
             selectInput("species2",h5("Species Input"),c("All",species_vars))
           )
    ),
    column(9,align="center",
           htmlOutput("ggvis_time"),
           HTML("<p>Historical total catch trend by season of western Lake Erie species.<br><br><br><br>")
    )
  ),
  #fluidRow(
  #         dataTableOutput(outputId="table")
  #),
  HTML("<h4>Forage Density</h4>"),
  fluidRow(
    column(3,
           wellPanel(
             HTML("<p>These functional groups are used by the Lake Erie Committee Forage Task Group and are inclusive of the following species:</p>"),
             HTML("<p>Clupeids: Age-0 Gizzard Shad (<i>Dorosoma cepedianum</i>) and Alewife (<i>Alosa pseudoharengus</i>)</p>
                  <p>Soft-rayed fish: Rainbow Smelt (<i>Osmerus mordax</i>), Emerald Shiner (<i>Notropis atherinoides</i>), Spottail Shiner (<i>Notropis hudsonius</i>), Silver Chub (<i>Macrhybopsis storeriana</i>), Trout-perch (<i>Percopsis omiscomaycus</i>), Round Goby (<i>Neogobius melanostomus</i>), and other cyprinids</p>
                  <p>Spiny-rayed fish: Age-0 for each of White Perch(<i>Morone americana</i>), White Bass (<i>Morone chrysops</i>), Yellow Perch (<i>Perca flavescens</i>), Walleye (<i>Sander vitreus</i>), and Freshwater Drum (<i>Aplodinotus grunniens</i>)</p>")
           )
    ),
    column(9,align="center",
           htmlOutput("ggvis_ftg"),
           HTML("Mean density of fish (number per hectare) by functional group in Ontario, Michigan, 
                and Ohio waters in the western basin of Lake Erie.<br><br><br>")
    )
  ),

  # Create a new panel for input selectors.
  HTML("<h3>Reactive Plots</h3>"),
  fluidRow(
    column(12,align="center",
           wellPanel(HTML("<h4>Select Inputs for Plots Below</h4>"),
                     tags$div(class="row",
                              tags$div(class="col-sm-4",
                                       selectInput("year",label=h5("Year"),year_vars,selected="2014")
                              ),
                              tags$div(class="col-sm-4",
                                       selectInput("season",label=h5("Season"),c("Spring","Autumn"),selected="Autumn")
                              ),
                              tags$div(class="col-sm-4",
                                       selectInput("species",label=h5("Species"),species_vars,selected="Yellow Perch")
                              )
                     )
           )
    )
  ),
  HTML("<br><br>"),
  
  # Create a new panel for the western basin map.
  HTML("<h5>Lake Erie Western Basin Map</h5>"),
  fluidRow(
    column(3,
           wellPanel(
             radioButtons("density",h5("Value:"),
                          c("Density (N/ha)" = "NperHA",
                            "Biomass (Kg/ha)" = "KgperHA")),
             selectInput("life_stage",h5("Life Stage"),c("All Life Stages",life_vars),selected="All Life Stages")
           )
    ),
    column(9,align="center",
           HTML("<h5>Hover over point to display station number and detailed density value.</h5>"),
           htmlOutput("ggvis_map"),
           HTML("Spatial distribution of species specific density or biomass from bottom trawl samples in the western basin of Lake Erie. 
                Hollow circles represent station localities.<br><br><br><br>")
    )
  ),
  
  # Create a new panel for lenght-weight plot and summarys.
  HTML("<h5>Length-Weight</h5>"),
  fluidRow(
    column(3,
           wellPanel(
             sliderInput("tl",h5("Length Range (mm)"),0,1000,step=5,value = c(0, 1000)),
             selectInput("xvar",h5("X-axis Variable"), axis_vars,selected="tl"),
             selectInput("yvar",h5("Y-axis Variable"), axis_vars,selected="wt")
             )
          ),
    column(9,align="center",
           tabsetPanel(type="tabs",
                       tabPanel("Weight-Length Plot",htmlOutput("ggvis_lw_plot"),
                                HTML("Species specific weight-length data collected from western basin of Lake Erie. 
                                     Total lengths and weights collected from a size-mode specific subsample on board the R/V Muskie.<br><br>")),
                       tabPanel("Linear Regression Summary",
                                withMathJax("$$\\log(W)=\\log(a)+b\\log(L)$$"),
                                tableOutput("lm_call"),
                                HTML("Species specific weight-length data collected from western basin of Lake Erie. 
                                     Total lengths and weights collected from a size-mode specific subsample on board the R/V Muskie.<br><br>"),
                                htmlOutput("ggvis_lm_resid"),
                                HTML("Species specific residual fit from linear model.<br><br>")),
                       tabPanel("Power Function Summary",
                                ("$$W=aL^b$$"),
                                tableOutput("nlm_call"),
                                HTML("Species specific weight-length data collected from western basin of Lake Erie. 
                                     Total lengths and weights collected from a size-mode specific subsample on board the R/V Muskie.<br><br>"),
                                htmlOutput("ggvis_nlm_resid"),
                                HTML("Species specific residual fit from power function.<br><br>"))
           ),
           wellPanel(
             span("Number of individuals selected:",textOutput("n_fish")
             )
           )
    )
  ),
  HTML("<br><br><br><br>"),
  
  # Create a new panel for the length-frequency.
  HTML("<h5>Length Frequency</h5>"),
  fluidRow(
    column(3,
           wellPanel(
             sliderInput("slider1",label=h5("Bin Width (mm)"),min=5,max=25,step=5,value=10))),
    column(9,align="center",
           htmlOutput("ggvis_hist"),
           HTML("Species specific length frequency from western basin of Lake Erie. 
                Lengths were expanded from measured total lengths collected on board the R/V Muskie.<br><br>"),
           wellPanel(
             span("Number of individuals selected:",
                  textOutput("n_size")
             )
           )
    )
  ),
  HTML("<br><br>"),
  
  ## USGS Reference and Disclaimer
  HTML("<p><i>U.S. Geological Survey</i> (USGS) Computer Program <b>XXXX</b> version 2015-01.
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