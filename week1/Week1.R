
# Coursera
# Course9 Developing Data Products

# Week1
################################################################################
# Shiny gadget1
library(shiny)
library(miniUI)

myFirstGadget <- function() {
    ui <- miniPage(
        gadgetTitleBar("My First Gadget")
    )
    server <- function(input, output, session) {
        #the done button closes the app
        observeEvent(input$done, {
            stopApp()
        })
    }
    runGadget(ui, server)
}

myFirstGadget()

################################################################################
# Shiny gadget2
library(shiny)
library(miniUI)

multiplyNumbers <- function(numbers1, numbers2) {
    ui <- miniPage(
        gadgetTitleBar("Multiply Two Numbers"),
        miniContentPanel(
            selectInput("num1", "First Number", choices = numbers1),
            selectInput("num2", "Second Number", choices = numbers2)
        )
    )
    server <- function(input, output, session) {
        observeEvent(input$done, {
            num1 <- as.numeric(input$num1)
            num2 <- as.numeric(input$num2)
            stopApp(num1 * num2)
        })
    }
    runGadget(ui, server)
}

multiplyNumbers(1:10, 1:10)

################################################################################
# Shiny gadget3
library(shiny)
library(miniUI)

pickTrees <- function() {
    ui <- miniPage(
        gadgetTitleBar("Select Points by Dragging Your Mouse"),
        miniContentPanel(
            plotOutput("plot", height = "100%", brush = "brush")
        )
    )
    server <- function(input, output, session) {
        output$plot <- renderPlot({
            plot(trees$Girth, trees$Volume, main = "Trees!",
                 xlab = "Girth", ylab = "Volume")
        })
        observeEvent(input$done, {
            stopApp(brushedPoints(trees, input$brush,
                                  xvar = "Girth", yvar = "Volume"))
        })
    }
    runGadget(ui, server)
}

treesPicked = pickTrees()
treesPicked
#useful for picking outliers

################################################################################
# googleVis
# motion chart
# example
suppressPackageStartupMessages(library(googleVis))
M <- gvisMotionChart(Fruits, "Fruit", "Year", 
                     options = list(width = 600, height = 400))
plot(M) #get it look right, and then grad the jave codes
print(M) #jave script for the html page
print(M, "chart")

# plots on maps
head(Exports)
G <- gvisGeoChart(Exports, locationvar = "Country",
                  colorvar = "Profit", 
                  options = list(width = 600, height = 400))
plot(G)
# zoom in on one region
G2 <- gvisGeoChart(Exports, locationvar = "Country",
                  colorvar = "Profit", 
                  options = list(width = 600, height = 400, region = "150"))

plot(G2)

# setting more options (jave script)
df <- data.frame(label=c("US", "GB", "BR"), val1=c(1,3,4), val2=c(23,12,32))
df
Line <- gvisLineChart(df, xvar="label", yvar=c("val1","val2"),
                      options=list(title="Hello World", legend="bottom",
                                   titleTextStyle="{color:'red', fontSize:18}",                  
                                   vAxis="{gridlines:{color:'red', count:3}}",
                                   hAxis="{title:'My Label', titleTextStyle:{color:'blue'}}",
                                   series="[{color:'green', targetAxisIndex: 0}, 
                                   {color: 'blue',targetAxisIndex:1}]",
                                   vAxes="[{title:'Value 1 (%)', format:'##,######%'}, 
                                   {title:'Value 2 (\U00A3)'}]",                          
                                   curveType="function", width=500, height=300))
plot(Line)

# combine multiple plots together
G <- gvisGeoChart(Exports, "Country", "Profit",options=list(width=200, height=100))
T1 <- gvisTable(Exports,options=list(width=200, height=270))
M <- gvisMotionChart(Fruits, "Fruit", "Year", options=list(width=400, height=370))
GT <- gvisMerge(G,T1, horizontal=FALSE)
GTM <- gvisMerge(GT, M, horizontal=TRUE,tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")
plot(GTM)

################################################################################
# Plotly
library(plotly)
data("mtcars")

# scatterplot
plot_ly(mtcars, x = mtcars$wt, y = mtcars$mpg, mode = "markers")

# scatterplot color
plot_ly(mtcars, x = mtcars$wt, y = mtcars$mpg, 
        mode = "markers", color = as.factor(mtcars$cyl))

# coninuous color
plot_ly(mtcars, x = mtcars$wt, y = mtcars$mpg, 
        mode = "markers", color = mtcars$disp)

# scatterplot sizing (four dimensions in one plot)
plot_ly(mtcars, x = mtcars$wt, y = mtcars$mpg, 
        mode = "markers", color = as.factor(mtcars$cyl), size = mtcars$hp)

# 3D scatterplot
set.seed(2016-07-21)
temp <- rnorm(100, mean = 30, sd = 5)
pressure <- rnorm(100)
dtime <- 1:100
plot_ly(x = temp, y = pressure, z = dtime,
        type = "scatter3d", mode = "markers", color = temp)

# line graph
library(plotly)
data("airmiles")
airmiles
time(airmiles)
plot_ly(x = time(airmiles), y = airmiles, mode = "line")

# multiple line graph
library(plotly)
library(tidyr)
library(dplyr)
data("EuStockMarkets")
head(EuStockMarkets)
is.data.frame(EuStockMarkets)
stocks <- as.data.frame(EuStockMarkets) %>% #convert to a data frame
    gather(index, price) %>% #wide to long
    mutate(time = rep(time(EuStockMarkets), 4)) #create the time variable
plot_ly(stocks, x = ~time, y = ~price, color = ~index)

# histogram
plot_ly(x = precip, type = "histogram")

# boxplot
plot_ly(iris, y = ~Petal.Length, color = ~Species, type = "box")

# heatmap
terrain1 <- matrix(rnorm(100 * 100), nrow = 100, ncol = 100)
plot_ly(z = terrain1, type = "heatmap")

# 3D surface (smoothing is automatically done)
terrain2 <- matrix(sort(rnorm(100 * 100)), nrow = 100, ncol = 100)
plot_ly(z = terrain2, type = "surface")

# choropleth maps
# create data frame
state_pop <- data.frame(State = state.abb, Pop = as.vector(state.x77[, 1]))
head(state_pop)
# create hover text
state_pop$hover <- with(state_pop, paste(State, '<br>', "Population:", Pop))
# make state borders red
borders <- list(color = toRGB("red"))
# set up some mapping options
map_options <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
)

plot_ly(state_pop, z = ~Pop, text = ~hover, locations = ~State,
        type = 'choropleth', locationmode = 'USA-states',
        color = ~Pop, colors = 'Blues', maker = list(line = borders)) %>%
    layout(title = 'US Population in 1975', geo = map_options)
# if it doesn't display in the R console, save it as a web page and open in the IE browser

# ggplot
set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]
head(d)
p <- ggplot(data = d, aes(x = carat, y = ~rice)) +
    geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
    geom_smooth(aes(color = cut, fill = cut) + facet_wrap(~cut))
(gg <- ggplotly(p))

# post to the plotly website from R
plotly_POST(gg)
