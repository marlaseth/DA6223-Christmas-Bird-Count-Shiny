
library(shiny)
library(tidyverse)
library(plotly)
library(magick)
library(DT)

bird<-read.csv(url("https://raw.githubusercontent.com/marlaseth/DA6223-Christmas-Bird-Count-Shiny/main/birddata.csv"))
birdlook<-read.delim("https://raw.githubusercontent.com/marlaseth/DA6223-Christmas-Bird-Count-Shiny/main/birdlookup.txt", sep = "\t")
countstats <- read.csv(url("https://raw.githubusercontent.com/marlaseth/DA6223-Christmas-Bird-Count-Shiny/main/countstats.csv"))


ui <- fluidPage(

    h4(textOutput("nameandclass")),
    
    br(),
    
    fluidRow(
        column(3,img(src = "lessergoldfinch.jpg", width = 250, height = 250)
               
        ),
        column(9,em(h1("Christmas Bird Count Data for Commonly Seen Birds in the Six Largest Cities in Texas from 2009 to 2019")), align = "center" )
    ),
      
    
    br(),


    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("database",
                        "What would you like to do?",
                       c("Compare Count Statistics among the Six Cities", "Look at Species Data for a Location"), 
                         selected = "Look at Species Data for a Location"),
                       
            conditionalPanel(condition = 'input.database == "Compare Count Statistics among the Six Cities"', 
                             selectInput("countstatvar", "Please choose a variable", c("Number of Participants","Number of Species", "Number of Hours"),
                                         selected = "Number of Species")),
            
            conditionalPanel(condition = 'input.database == "Look at Species Data for a Location"',
                             selectInput("birdgroup", "Please choose a group", c("Fowl (includes water and land)",
                                                         "Loons, Grebes, Cormorants, Pelicans, Herons, Egrets, Ibis",
                                                         "Buteos, Accipiters, Falcons",
                                                         "Rails, Coots, Cranes, Plovers, Sandpipers",
                                                         "Gulls, Terns, Pigeons, Doves",
                                                         "Roadrunners, Owls, Swifts, Hummingbirds",
                                                         "Kingfishers, Woodpeckers, Parakeets",
                                                         "Flycatchers, Shrikes, Vireos, Corvids",
                                                         "Swallows, Chickadees, Titmice, Nuthatches, Wrens",
                                                         "Gnatcatchers, Kinglets, Bluebirds, Thrushes, Thrashers, Mockingbirds",
                                                         "Starlings, Pipits, Waxwings, Warblers",
                                                         "Sparrows, Towhees, Cardinals",
                                                         "Blackbirds, Finches, Weaverfinches")),
                             
                             conditionalPanel(condition = 'input.birdgroup == "Fowl (includes water and land)"',
                                              selectInput("species1","Please choose a species", c("Black-bellied Whistling-Duck",
                                                                                                 "Greater White-fronted Goose",
                                                                                                 "Snow Goose",
                                                                                                 "Ross's Goose",
                                                                                                 "Canada Goose",
                                                                                                 "Mute Swan",
                                                                                                 "Wood Duck",
                                                                                                 "Gadwall",
                                                                                                 "American Wigeon",
                                                                                                 "Mallard",
                                                                                                 "Mottled Duck",
                                                                                                 "Blue-winged Teal",
                                                                                                 "Cinnamon Teal",
                                                                                                 "Northern Shoveler",
                                                                                                 "Northern Pintail",
                                                                                                 "Green-winged Teal",
                                                                                                 "Canvasback",
                                                                                                 "Redhead",
                                                                                                 "Ring-necked Duck",
                                                                                                 "Greater Scaup",
                                                                                                 "Lesser Scaup",
                                                                                                 "Bufflehead",
                                                                                                 "Common Goldeneye",
                                                                                                 "Hooded Merganser",
                                                                                                 "Common Merganser",
                                                                                                 "Ruddy Duck",
                                                                                                 "Scaled Quail",
                                                                                                 "Gambel's Quail",
                                                                                                 "Northern Bobwhite",
                                                                                                 "Wild Turkey"))),
                             
                             conditionalPanel(condition = 'input.birdgroup == "Loons, Grebes, Cormorants, Pelicans, Herons, Egrets, Ibis"',
                                              selectInput("species2","Please choose a species",c("Common Loon",
                                                                                                "Least Grebe",
                                                                                                "Pied-billed Grebe",
                                                                                                "Horned Grebe",
                                                                                                "Red-necked Grebe",
                                                                                                "Eared Grebe",
                                                                                                "Neotropic Cormorant",
                                                                                                "Double-crested Cormorant",
                                                                                                "Anhinga",
                                                                                                "American White Pelican",
                                                                                                "Brown Pelican",
                                                                                                "Great Blue Heron",
                                                                                                "Great Egret",
                                                                                                "Snowy Egret",
                                                                                                "Little Blue Heron",
                                                                                                "Tricolored Heron",
                                                                                                "Cattle Egret",
                                                                                                "Green Heron",
                                                                                                "Black-crowned Night-Heron",
                                                                                                "Yellow-crowned Night-Heron",
                                                                                                "White Ibis",
                                                                                                "White-faced Ibis",
                                                                                                "Roseate Spoonbill"))),
                             
                             conditionalPanel(condition = 'input.birdgroup == "Buteos, Accipiters, Falcons"',
                                              selectInput("species3","Please choose a species",c("Black Vulture",
                                                                                                "Turkey Vulture",
                                                                                                "Osprey",
                                                                                                "Northern Harrier",
                                                                                                "Sharp-shinned Hawk",
                                                                                                "Cooper's Hawk",
                                                                                                "Bald Eagle",
                                                                                                "Harris's Hawk",
                                                                                                "Red-shouldered Hawk",
                                                                                                "Red-tailed Hawk",
                                                                                                "Ferruginous Hawk",
                                                                                                "Crested Caracara",
                                                                                                "American Kestrel",
                                                                                                "Merlin",
                                                                                                "Peregrine Falcon",
                                                                                                "Prairie Falcon"))),
                             
                             conditionalPanel(condition = 'input.birdgroup == "Rails, Coots, Cranes, Plovers, Sandpipers"',
                                              selectInput("species4","Please choose a species",c("Virginia Rail",
                                                                                                "Sora",
                                                                                                "Common Gallinule",
                                                                                                "American Coot",
                                                                                                "Sandhill Crane",
                                                                                                "Black-necked Stilt",
                                                                                                "American Avocet",
                                                                                                "Black-bellied Plover",
                                                                                                "Semipalmated Plover",
                                                                                                "Killdeer",
                                                                                                "Greater/Lesser Yellowlegs",
                                                                                                "Spotted Sandpiper",
                                                                                                "Solitary Sandpiper",
                                                                                                "Willet",
                                                                                                "Ruddy Turnstone",
                                                                                                "Sanderling",
                                                                                                "Western Sandpiper",
                                                                                                "Least Sandpiper",
                                                                                                "Dunlin",
                                                                                                "Short-billed/Long-billed Dowitcher",
                                                                                                "Wilson's Snipe"))),
                             
                             conditionalPanel(condition = 'input.birdgroup == "Gulls, Terns, Pigeons, Doves"',
                                              selectInput("species5","Please choose a species",c("Laughing Gull",
                                                                                                "Franklin's Gull",
                                                                                                "Bonaparte's Gull",
                                                                                                "Ring-billed Gull",
                                                                                                "California Gull",
                                                                                                "Herring Gull",
                                                                                                "Lesser Black-backed Gull",
                                                                                                "Caspian Tern",
                                                                                                "Royal Tern",
                                                                                                "Forster's Tern",
                                                                                                "Black Skimmer",
                                                                                                "Rock Pigeon",
                                                                                                "Eurasian Collared-Dove",
                                                                                                "Inca Dove",
                                                                                                "Common Ground-Dove",
                                                                                                "White-winged Dove",
                                                                                                "Mourning Dove"))),
                             
                             conditionalPanel(condition = 'input.birdgroup == "Roadrunners, Owls, Swifts, Hummingbirds"',
                                              selectInput("species6","Please choose a species",c("Greater Roadrunner",
                                                                                                "Barn Owl",
                                                                                                "Eastern Screech-Owl",
                                                                                                "Great Horned Owl",
                                                                                                "Barred Owl",
                                                                                                "Long-eared Owl",
                                                                                                "Common Pauraque",
                                                                                                "White-throated Swift",
                                                                                                "Anna's Hummingbird",
                                                                                                "Broad-tailed Hummingbird",
                                                                                                "Rufous Hummingbird"))),
                             
                             conditionalPanel(condition = 'input.birdgroup == "Kingfishers, Woodpeckers, Parakeets"',
                                              selectInput("species7","Please choose a species",c("Ringed Kingfisher",
                                                                                                "Belted Kingfisher",
                                                                                                "Green Kingfisher",
                                                                                                "Golden-fronted Woodpecker",
                                                                                                "Acorn Woodpecker",
                                                                                                "Red-bellied Woodpecker",
                                                                                                "Yellow-bellied Sapsucker",
                                                                                                "Ladder-backed Woodpecker",
                                                                                                "Downy Woodpecker",
                                                                                                "Northern Flicker",
                                                                                                "Pileated Woodpecker",
                                                                                                "Monk Parakeet"))),
                             
                             conditionalPanel(condition = 'input.birdgroup == "Flycatchers, Shrikes, Vireos, Corvids"',
                                              selectInput("species8","Please choose a species",c("Eastern Phoebe",
                                                                                                "Black Phoebe",
                                                                                                "Say's Phoebe",
                                                                                                "Vermilion Flycatcher",
                                                                                                "yellow-bellied kingbird sp.",
                                                                                                "Scissor-tailed Flycatcher",
                                                                                                "Loggerhead Shrike",
                                                                                                "White-eyed Vireo",
                                                                                                "Blue-headed Vireo",
                                                                                                "Blue Jay",
                                                                                                "Western Scrub-Jay",
                                                                                                "American Crow",
                                                                                                "Common Raven"))),
                             
                             conditionalPanel(condition = 'input.birdgroup == "Swallows, Chickadees, Titmice, Nuthatches, Wrens"',
                                              selectInput("species9","Please choose a species",c("Tree Swallow",
                                                                                                "Cliff/Cave Swallow",
                                                                                                "Carolina Chickadee",
                                                                                                "Tufted Titmouse",
                                                                                                "Black-crested Titmouse",
                                                                                                "Verdin",
                                                                                                "Red-breasted Nuthatch",
                                                                                                "White-breasted Nuthatch",
                                                                                                "Brown Creeper",
                                                                                                "Rock Wren",
                                                                                                "Canyon Wren",
                                                                                                "House Wren",
                                                                                                "Winter Wren",
                                                                                                "Sedge Wren",
                                                                                                "Marsh Wren",
                                                                                                "Carolina Wren",
                                                                                                "Bewick's Wren",
                                                                                                "Cactus Wren"))),
                             
                             conditionalPanel(condition = 'input.birdgroup == "Gnatcatchers, Kinglets, Bluebirds, Thrushes, Thrashers, Mockingbirds"',
                                              selectInput("species10","Please choose a species",c("Blue-gray Gnatcatcher",
                                                                                                "Golden-crowned Kinglet",
                                                                                                "Ruby-crowned Kinglet",
                                                                                                "Eastern Bluebird",
                                                                                                "Western Bluebird",
                                                                                                "Hermit Thrush",
                                                                                                "American Robin",
                                                                                                "Gray Catbird",
                                                                                                "Curve-billed Thrasher",
                                                                                                "Brown Thrasher",
                                                                                                "Long-billed Thrasher",
                                                                                                "Crissal Thrasher",
                                                                                                "Northern Mockingbird"))),
                             
                             conditionalPanel(condition = 'input.birdgroup == "Starlings, Pipits, Waxwings, Warblers"',
                                              selectInput("species11","Please choose a species",c("European Starling",
                                                                                                "American Pipit",
                                                                                                "Sprague's Pipit",
                                                                                                "Cedar Waxwing",
                                                                                                "Phainopepla",
                                                                                                "Black-and-white Warbler",
                                                                                                "Orange-crowned Warbler",
                                                                                                "Common Yellowthroat",
                                                                                                "Yellow Warbler",
                                                                                                "Pine Warbler",
                                                                                                "Yellow-rumped Warbler"))),
                             
                             conditionalPanel(condition = 'input.birdgroup == "Sparrows, Towhees, Cardinals"',
                                              selectInput("species12","Please choose a species",c("Grasshopper Sparrow",
                                                                                                "Le Conte's Sparrow",
                                                                                                "Seaside Sparrow",
                                                                                                "Chipping Sparrow",
                                                                                                "Black-chinned Sparrow",
                                                                                                "Brewer's Sparrow",
                                                                                                "Field Sparrow",
                                                                                                "Black-throated Sparrow",
                                                                                                "Lark Sparrow",
                                                                                                "Lark Bunting",
                                                                                                "Fox Sparrow",
                                                                                                "Dark-eyed Junco",
                                                                                                "White-crowned Sparrow",
                                                                                                "Harris's Sparrow",
                                                                                                "White-throated Sparrow",
                                                                                                "Vesper Sparrow",
                                                                                                "Savannah Sparrow",
                                                                                                "Song Sparrow",
                                                                                                "Lincoln's Sparrow",
                                                                                                "Swamp Sparrow",
                                                                                                "Rufous-crowned Sparrow",
                                                                                                "Canyon Towhee",
                                                                                                "Green-tailed Towhee",
                                                                                                "Spotted Towhee",
                                                                                                "Eastern Towhee",
                                                                                                "Pyrrhuloxia",
                                                                                                "Northern Cardinal"))),
                             
                             conditionalPanel(condition = 'input.birdgroup == "Blackbirds, Finches, Weaverfinches"',
                                              selectInput("species13","Please choose a species",c("Red-winged Blackbird",
                                                                                                "Western/Eastern Meadowlark",
                                                                                                "Yellow-headed Blackbird",
                                                                                                "Rusty Blackbird",
                                                                                                "Brewer's Blackbird",
                                                                                                "Common Grackle",
                                                                                                "Boat-tailed Grackle",
                                                                                                "Great-tailed Grackle",
                                                                                                "Brown-headed Cowbird",
                                                                                                "Hooded Oriole",
                                                                                                "House Finch",
                                                                                                "Pine Siskin",
                                                                                                "Lesser Goldfinch",
                                                                                                "American Goldfinch",
                                                                                                "House Sparrow"))), 
                             
                             selectInput("location","Please choose a location",c("Austin","Dallas","El Paso","Ft Worth","Houston","San Antonio")),
        
                             selectInput("type","Please choose a graph type",c("Bar graph","Line graph")),
                             
                             selectInput("color","Please choose a color",c("steelblue", "firebrick", "burlywood", "coral", "seagreen", "orchid" )),
                             
                             checkboxInput("trend", "Show Linear Trend Line", value = FALSE),
                             
                             checkboxInput("ster","Show Standard Error", value = FALSE)

                             

) #second/nested conditionalPanel close

        ), #sidebarpanel close

        mainPanel (
            
        tabsetPanel(
            
           tabPanel("Plots", 
           
               plotlyOutput("plot"),
           
               br(),
           
               textOutput("subheading"),
           
               br(),
           
               textOutput("graphnote")),
        
           tabPanel ("Bird Lookup", dataTableOutput("lookup"))

           
        ) #tabsetPanel close
        ) #main panel close
        ), #sidebarlayout close

    textOutput("source"),
    br(),
    textOutput("photocredit")
    
) #fluid page close










# Define server logic required to draw a histogram 
server <- function(input, output) {
    
    
         output$lookup<-DT::renderDataTable({
          DT::datatable(data=birdlook, options = list(ordering=F, pageLength = 20))
          })
    
         
        output$nameandclass <- renderText ({"Marla Seth, ibc346, DA 6223 - Data Analytics Tools and Techniques"})
        
        output$graphnote <- renderText(if (input$database == "Look at Species Data for a Location") {"If no bars appear on a bar graph or the line graph is a straight line at y=0, the species was not sighted in that city.  
        Please choose a different location"})
        
        output$subheading <- renderText(if (input$database == "Look at Species Data for a Location") {"Only species that have been sighted on Christmas Count Day for at least 6 years in at least one city are included"})
        
        output$photocredit <- renderText({"Lesser Goldfinch image used with creative commons license courtesy of https://www.flickr.com/photos/johnkay/4239238250"})
        
        output$source <- renderText({"Source:National Audubon Society (2020). The Christmas Bird Count Historical Results [Online]. Available from http://www.christmasbirdcount.org [April 2021]"})
    
        output$plot <- renderPlotly({
            
            bdata <- dplyr::filter(bird, location == input$location)
            
            if (input$database == "Look at Species Data for a Location") {
                
                ster <- input$ster
                
                trendcolor <- switch(input$color,
                                     "steelblue" = "hotpink",
                                     "orchid" = "slategrey",
                                     "firebrick" = "wheat3",
                                     "burlywood" = "royalblue4",
                                     "coral" = "sienna4",
                                     "seagreen" = "navajowhite1")
                
                if (input$birdgroup == "Fowl (includes water and land)") {
                    
                    name <- input$species1
                    
                    yvar <- switch(input$species1,
                                   "Black-bellied Whistling-Duck" = "Black.bellied.Whistling.Duck",
                                   "Greater White-fronted Goose" = "Greater.White.fronted.Goose",
                                   "Snow Goose" = "Snow.Goose",
                                   "Ross's Goose" = "Ross.s.Goose",
                                   "Canada Goose" = "Canada.Goose",
                                   "Mute Swan" = "Mute.Swan",
                                   "Wood Duck" = "Wood.Duck",
                                   "Gadwall" = "Gadwall",
                                   "American Wigeon" = "American.Wigeon",
                                   "Mallard" = "Mallard",
                                   "Mottled Duck" = "Mottled.Duck",
                                   "Blue-winged Teal" = "Blue.winged.Teal",
                                   "Cinnamon Teal" = "Cinnamon.Teal",
                                   "Northern Shoveler" = "Northern.Shoveler",
                                   "Northern Pintail" = "Northern.Pintail",
                                   "Green-winged Teal" = "Green.winged.Teal",
                                   "Canvasback" = "Canvasback",
                                   "Redhead" = "Redhead",
                                   "Ring-necked Duck" = "Ring.necked.Duck",
                                   "Greater Scaup" = "Greater.Scaup",
                                   "Lesser Scaup" = "Lesser.Scaup",
                                   "Bufflehead" = "Bufflehead",
                                   "Common Goldeneye" = "Common.Goldeneye",
                                   "Hooded Merganser" = "Hooded.Merganser",
                                   "Common Merganser" = "Common.Merganser",
                                   "Red-breasted Merganser" = "Red.breasted.Merganser",
                                   "Ruddy Duck" = "Ruddy.Duck",
                                   "Scaled Quail" = "Scaled.Quail",
                                   "Gambel's Quail" = "Gambel.s.Quail",
                                   "Northern Bobwhite" = "Northern.Bobwhite",
                                   "Wild Turkey" = "Wild.Turkey")
                }
            
                if (input$birdgroup == "Loons, Grebes, Cormorants, Pelicans, Herons, Egrets, Ibis") {
                    
                    name <- input$species2
                    
                    yvar <- switch(input$species2,
                                   "Common Loon" = "Common.Loon",
                                   "Least Grebe" = "Least.Grebe",
                                   "Pied-billed Grebe" = "Pied.billed.Grebe",
                                   "Horned Grebe" = "Horned.Grebe",
                                   "Red-necked Grebe" = "Red.necked.Grebe",
                                   "Eared Grebe" = "Eared.Grebe",
                                   "Neotropic Cormorant" = "Neotropic.Cormorant",
                                   "Double-crested Cormorant" = "Double.crested.Cormorant",
                                   "Anhinga" = "Anhinga",
                                   "American White Pelican" = "American.White.Pelican",
                                   "Brown Pelican" = "Brown.Pelican",
                                   "Great Blue Heron" = "Great.Blue.Heron",
                                   "Great Egret" = "Great.Egret",
                                   "Snowy Egret" = "Snowy.Egret",
                                   "Little Blue Heron" = "Little.Blue.Heron",
                                   "Tricolored Heron" = "Tricolored.Heron",
                                   "Cattle Egret" = "Cattle.Egret",
                                   "Green Heron" = "Green.Heron",
                                   "Black-crowned Night-Heron" = "Black.crowned.Night.Heron",
                                   "Yellow-crowned Night-Heron" = "Yellow.crowned.Night.Heron",
                                   "White Ibis" = "White.Ibis",
                                   "White-faced Ibis" = "White.faced.Ibis",
                                   "Roseate Spoonbill" = "Roseate.Spoonbill")
                }
            
                if (input$birdgroup == "Buteos, Accipiters, Falcons") {
                    
                    name <- input$species3
                    
                    yvar <- switch(input$species3,
                                   "Black Vulture" = "Black.Vulture",
                                   "Turkey Vulture" = "Turkey.Vulture",
                                   "Osprey" = "Osprey",
                                   "Northern Harrier" = "Northern.Harrier",
                                   "Sharp-shinned Hawk" = "Sharp.shinned.Hawk",
                                   "Cooper's Hawk" = "Cooper.s.Hawk",
                                   "Bald Eagle" = "Bald.Eagle",
                                   "Red-shouldered Hawk" = "Red.shouldered.Hawk",
                                   "Harris's Hawk" = "Harris.s.Hawk",
                                   "Red-tailed Hawk" = "Red.tailed.Hawk",
                                   "Ferruginous Hawk" = "Ferruginous.Hawk",
                                   "Crested Caracara" = "Crested.Caracara",
                                   "American Kestrel" = "American.Kestrel",
                                   "Merlin" = "Merlin",
                                   "Peregrine Falcon" = "Peregrine.Falcon",
                                   "Prairie Falcon" = "Prairie.Falcon")
                }      
                
                if (input$birdgroup == "Rails, Coots, Cranes, Plovers, Sandpipers") {
                    
                    name <- input$species4
                    
                    yvar <- switch(input$species4,
                                   "Virginia Rail" = "Virginia.Rail",
                                   "Sora" = "Sora",
                                   "Common Gallinule" = "Common.Gallinule",
                                   "American Coot" = "American.Coot",
                                   "Sandhill Crane" = "Sandhill.Crane",
                                   "Black-necked Stilt" = "Black.necked.Stilt",
                                   "American Avocet" = "American.Avocet",
                                   "Black-bellied Plover" = "Black.bellied.Plover",
                                   "Semipalmated Plover" = "Semipalmated.Plover",
                                   "Killdeer" = "Killdeer",
                                   "Greater/Lesser Yellowlegs" = "Greater.Lesser.Yellowlegs",
                                   "Spotted Sandpiper" = "Spotted.Sandpiper",
                                   "Solitary Sandpiper" = "Solitary.Sandpiper",
                                   "Willet" = "Willet",
                                   "Ruddy Turnstone" = "Ruddy.Turnstone",
                                   "Sanderling" = "Sanderling",
                                   "Western Sandpiper" = "Western.Sandpiper",
                                   "Least Sandpiper" = "Least.Sandpiper",
                                   "Dunlin" = "Dunlin",
                                   "Short-billed/Long-billed Dowitcher" = "Short.billed.Long.billed.Dowitcher",
                                   "Wilson's Snipe" = "Wilson.s.Snipe")
                }      
                
                if (input$birdgroup == "Gulls, Terns, Pigeons, Doves") {
                    
                    name <- input$species5
                    
                    yvar <- switch(input$species5,
                                   "Laughing Gull" = "Laughing.Gull",
                                   "Bonaparte's Gull" = "Bonaparte.s.Gull",
                                   "Franklin's Gull" = "Franklin.s.Gull",
                                   "Ring-billed Gull" = "Ring.billed.Gull",
                                   "California Gull" = "California.Gull",
                                   "Herring Gull" = "Herring.Gull",
                                   "Lesser Black-backed Gull" = "Lesser.Black.backed.Gull",
                                   "Caspian Tern" = "Caspian.Tern",
                                   "Royal Tern" = "Royal.Tern",
                                   "Forster's Tern" = "Forster.s.Tern",
                                   "Black Skimmer" = "Black.Skimmer",
                                   "Rock Pigeon" = "Rock.Pigeon",
                                   "Eurasian Collared-Dove" = "Eurasian.Collared.Dove",
                                   "Inca Dove" = "Inca.Dove",
                                   "Common Ground-Dove" = "Common.Ground.Dove",
                                   "White-winged Dove" = "White.winged.Dove",
                                   "Mourning Dove" = "Mourning.Dove")
                }
                
                if (input$birdgroup == "Roadrunners, Owls, Swifts, Hummingbirds") {
                    
                    name <- input$species6
                    
                    yvar <- switch(input$species6,
                                   "Greater Roadrunner" = "Greater.Roadrunner",
                                   "Barn Owl" = "Barn.Owl",
                                   "Eastern Screech-Owl" = "Eastern.Screech.Owl",
                                   "Great Horned Owl" = "Great.Horned.Owl",
                                   "Barred Owl" = "Barred.Owl",
                                   "Long-eared Owl" = "Long.eared.Owl",
                                   "Common Pauraque" = "Common.Pauraque",
                                   "White-throated Swift" = "White.throated.Swift",
                                   "Anna's Hummingbird" = "Anna.s.Hummingbird",
                                   "Broad-tailed Hummingbird" = "Broad.tailed.Hummingbird",
                                   "Rufous Hummingbird" = "Rufous.Hummingbird")
                }
                
                if (input$birdgroup == "Kingfishers, Woodpeckers, Parakeets") {
                    
                    name <- input$species7
                    
                    yvar <- switch(input$species7,
                                   "Ringed Kingfisher" = "Ringed.Kingfisher",
                                   "Belted Kingfisher" = "Belted.Kingfisher",
                                   "Green Kingfisher" = "Green.Kingfisher",
                                   "Golden-fronted Woodpecker" = "Golden.fronted.Woodpecker",
                                   "Acorn Woodpecker" = "Acorn.Woodpecker",
                                   "Red-bellied Woodpecker" = "Red.bellied.Woodpecker",
                                   "Yellow-bellied Sapsucker" = "Yellow.bellied.Sapsucker",
                                   "Ladder-backed Woodpecker" = "Ladder.backed.Woodpecker",
                                   "Downy Woodpecker" = "Downy.Woodpecker",
                                   "Northern Flicker" = "Northern.Flicker",
                                   "Pileated Woodpecker" = "Pileated.Woodpecker",
                                   "Monk Parakeet" = "Monk.Parakeet")
                }  
                
                if (input$birdgroup == "Flycatchers, Shrikes, Vireos, Corvids") {
                    
                    name <- input$species8
                    
                    yvar <- switch(input$species8,
                                   "Eastern Phoebe" = "Eastern.Phoebe",
                                   "Black Phoebe" = "Black.Phoebe",
                                   "Say's Phoebe" = "Say.s.Phoebe",
                                   "Vermilion Flycatcher" = "Vermilion.Flycatcher",
                                   "yellow-bellied kingbird sp." = "yellow.bellied.kingbird.sp.",
                                   "Scissor-tailed Flycatcher" = "Scissor.tailed.Flycatcher",
                                   "Loggerhead Shrike" = "Loggerhead.Shrike",
                                   "White-eyed Vireo" = "White.eyed.Vireo",
                                   "Blue-headed Vireo" = "Blue.headed.Vireo",
                                   "Blue Jay" = "Blue.Jay",
                                   "Western Scrub-Jay" = "Western.Scrub.Jay",
                                   "American Crow" = "American.Crow",
                                   "Common Raven" = "Common.Raven")
                }
                
                if (input$birdgroup == "Swallows, Chickadees, Titmice, Nuthatches, Wrens") {
                    
                    
                    name <- input$species9
                    
                    yvar <- switch(input$species9,
                                   "Tree Swallow" = "Tree.Swallow",
                                   "Cliff/Cave Swallow" = "Cliff.Cave.Swallow",
                                   "Carolina Chickadee" = "Carolina.Chickadee",
                                   "Tufted Titmouse" = "Tufted.Titmouse",
                                   "Black-crested Titmouse" = "Black.crested.Titmouse",
                                   "Verdin" = "Verdin",
                                   "Red-breasted Nuthatch" = "Red.breasted.Nuthatch",
                                   "White-breasted Nuthatch" = "White.breasted.Nuthatch",
                                   "Brown Creeper" = "Brown.Creeper",
                                   "Rock Wren" = "Rock.Wren",
                                   "Canyon Wren" = "Canyon.Wren",
                                   "House Wren" = "House.Wren",
                                   "Winter Wren" = "Winter.Wren",
                                   "Sedge Wren" = "Sedge.Wren",
                                   "Marsh Wren" = "Marsh.Wren",
                                   "Carolina Wren" = "Carolina.Wren",
                                   "Bewick's Wren" = "Bewick.s.Wren",
                                   "Cactus Wren" = "Cactus.Wren")
                }
                
                if (input$birdgroup == "Gnatcatchers, Kinglets, Bluebirds, Thrushes, Thrashers, Mockingbirds") {
                    
                    name <- input$species10
                    
                    yvar <- switch(input$species10,
                                   "Blue-gray Gnatcatcher" = "Blue.gray.Gnatcatcher",
                                   "Golden-crowned Kinglet" = "Golden.crowned.Kinglet",
                                   "Ruby-crowned Kinglet" = "Ruby.crowned.Kinglet",
                                   "Eastern Bluebird" = "Eastern.Bluebird",
                                   "Western Bluebird" = "Western.Bluebird",
                                   "Hermit Thrush" = "Hermit.Thrush",
                                   "American Robin" = "American.Robin",
                                   "Gray Catbird" = "Gray.Catbird",
                                   "Curve-billed Thrasher" = "Curve.billed.Thrasher",
                                   "Brown Thrasher" = "Brown.Thrasher",
                                   "Long-billed Thrasher" = "Long.billed.Thrasher",
                                   "Crissal Thrasher" = "Crissal.Thrasher",
                                   "Northern Mockingbird" = "Northern.Mockingbird")
                }
                
                if (input$birdgroup == "Starlings, Pipits, Waxwings, Warblers") {
                    
                    name <- input$species11
                    
                    yvar <- switch(input$species11,
                                   "European Starling" = "European.Starling",
                                   "American Pipit" = "American.Pipit",
                                   "Sprague's Pipit" = "Sprague.s.Pipit",
                                   "Cedar Waxwing" = "Cedar.Waxwing",
                                   "Phainopepla" = "Phainopepla",
                                   "Black-and-white Warbler" = "Black.and.white.Warbler",
                                   "Orange-crowned Warbler" = "Orange.crowned.Warbler",
                                   "Common Yellowthroat" = "Common.Yellowthroat",
                                   "Yellow Warbler" = "Yellow.Warbler",
                                   "Pine Warbler" = "Pine.Warbler",
                                   "Yellow-rumped Warbler" = "Yellow.rumped.Warbler")
                }
                
                
                if (input$birdgroup == "Sparrows, Towhees, Cardinals") {
                    
                    name <- input$species12
                    
                    yvar <- switch(input$species12,
                                   "Grasshopper Sparrow" = "Grasshopper.Sparrow",
                                   "Le Conte's Sparrow" = "Le.Conte.s.Sparrow",
                                   "Seaside Sparrow" = "Seaside.Sparrow",
                                   "Chipping Sparrow" = "Chipping.Sparrow",
                                   "Black-chinned Sparrow" = "Black.chinned.Sparrow",
                                   "Brewer's Sparrow" = "Brewer.s.Sparrow",
                                   "Black-throated Sparrow" = "Black.throated.Sparrow",
                                   "Field Sparrow" = "Field.Sparrow",
                                   "Lark Sparrow" = "Lark.Sparrow",
                                   "Lark Bunting" = "Lark.Bunting",
                                   "Fox Sparrow" = "Fox.Sparrow",
                                   "Dark-eyed Junco" = "Dark.eyed.Junco",
                                   "White-crowned Sparrow" = "White.crowned.Sparrow",
                                   "Harris's Sparrow" = "Harris.s.Sparrow",
                                   "White-throated Sparrow" = "White.throated.Sparrow",
                                   "Vesper Sparrow" = "Vesper.Sparrow",
                                   "Savannah Sparrow" = "Savannah.Sparrow",
                                   "Song Sparrow" = "Song.Sparrow",
                                   "Lincoln's Sparrow" = "Lincoln.s.Sparrow",
                                   "Swamp Sparrow" = "Swamp.Sparrow",
                                   "Rufous-crowned Sparrow" = "Rufous.crowned.Sparrow",
                                   "Canyon Towhee" = "Canyon.Towhee",
                                   "Green-tailed Towhee" = "Green.tailed.Towhee",
                                   "Spotted Towhee" = "Spotted.Towhee",
                                   "Eastern Towhee" = "Eastern.Towhee",
                                   "Pyrrhuloxia" = "Pyrrhuloxia",
                                   "Northern Cardinal" = "Northern.Cardinal")
                }
                
                if (input$birdgroup == "Blackbirds, Finches, Weaverfinches") {
                    
                    name <- input$species13
                    
                    yvar <- switch(input$species13,
                                   "Red-winged Blackbird" = "Red.winged.Blackbird",
                                   "Western/Eastern Meadowlark" = "Western.Eastern.Meadowlark",
                                   "Yellow-headed Blackbird" = "Yellow.headed.Blackbird",
                                   "Rusty Blackbird" = "Rusty.Blackbird",
                                   "Brewer's Blackbird" = "Brewer.s.Blackbird",
                                   "Common Grackle" = "Common.Grackle",
                                   "Boat-tailed Grackle" = "Boat.tailed.Grackle",
                                   "Great-tailed Grackle" = "Great.tailed.Grackle",
                                   "Brown-headed Cowbird" = "Brown.headed.Cowbird",
                                   "Hooded Oriole" = "Hooded.Oriole",
                                   "House Finch" = "House.Finch",
                                   "Pine Siskin" = "Pine.Siskin",
                                   "Lesser Goldfinch" = "Lesser.Goldfinch",
                                   "American Goldfinch" = "American.Goldfinch",
                                   "House Sparrow" = "House.Sparrow")
                }
                
                
     
            family = paste("Family:",birdlook$Family[birdlook == name], sep = " " )
            
            sscinames = paste(birdlook$Scientific.Name[birdlook$Standardized.Common.Name == name], "    ", family, sep = " ")
            
            speciestitle <- paste(name, sscinames, sep = "\n")
            

            if (input$type == "Line graph") {
                
                if (input$trend == TRUE) {
                    
                    ggplotly(ggplot(bdata, aes_string(x = bdata$year, y = yvar)) +
                                 geom_line (color = input$color, size = 2) +
                                 labs(x = "Year", y = "Number of Birds")+
                                 geom_smooth(method = "lm", aes(weight = 1.5), color = trendcolor, se = ster) +
                                 ggtitle(speciestitle) +
                                 scale_x_continuous(breaks = c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)) +
                                 theme_minimal ())
                } else {
                
                ggplotly(ggplot(bdata, aes_string(x = bdata$year, y = yvar)) +
                   geom_line (color = input$color, size = 2) +
                   labs(x = "Year", y = "Number of Birds")+
                   scale_x_continuous(breaks = c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)) +
                   ggtitle(speciestitle) +
                   theme_minimal ()) }
                
                }   else {     #close line graph if open else for bar graph
                

                if (input$trend == TRUE) {
                    ggplotly(ggplot(bdata, aes_string(x = bdata$year, y = yvar)) +
                                 geom_bar (stat = "identity", color = "white", fill = input$color) +
                                 labs(x = "Year", y = "Number of Birds") +
                                 ggtitle(speciestitle) +
                                 geom_smooth(method = "lm", aes(weight = 1.5), color = trendcolor, se = ster) +
                                 scale_x_continuous(breaks = c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)) +
                                 theme_minimal())   
                    
                } else {
                    
                ggplotly(ggplot(bdata, aes_string(x = bdata$year, y = yvar)) +
                    geom_bar (stat = "identity", color = "white", fill = input$color) +
                    labs(x = "Year", y = "Number of Birds") +
                    ggtitle(speciestitle) +
                    scale_x_continuous(breaks = c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)) +
                    theme_minimal()) 
            }}
            
            
            }  else {   
                
                yaxis <- switch(input$countstatvar,
                                    "Number of Participants" = "participants", 
                                    "Number of Species" = "species",
                                    "Number of Hours" = "hours")
                
                counttitle <- switch(input$countstatvar,
                                     "Number of Participants" = "Number of Participants by City per Year", 
                                     "Number of Species" = "Number of Species by City per Year",
                                     "Number of Hours" = "Number of Hours by City per Year")
                
             
                
                ggplotly(ggplot(countstats, aes_string(x = as.factor(countstats$year), y = yaxis, fill = "location")) +
                             geom_bar (stat = "identity", position = "dodge", color = "black", width = 0.8) +
                             labs(x = "Year", y = input$countstatvar) +
                             ggtitle(counttitle) +
                             theme_minimal()) 
            
            }  #close countstats if
            
        }) #close output$plot




    } #close server



# Run the application 
shinyApp(ui = ui, server = server)
