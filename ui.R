
library(shiny)
library(rvest)

shinyUI(bootstrapPage(
	selectInput(inputId = "categories",
		label = "Select category you wish you describe",
		choices = c("overall", "art-and-collectibles", "home-and-living", "jewelry", "clothing", "vintage", "weddings", "craft-supplies-and-tools", "books-movies-and-music", "bags-and-purses", "shoes")
	),
	#plotOutput(outputId = "main_plot", height = "300px")
	plotOutput('main_plot')
)
)




#"mobile-accessories",














