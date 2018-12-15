
library(shiny)
library(rvest)

shinyServer(function(input, output){
		
	

	
	 output$main_plot <- renderPlot({	
		#try Catch function in case of 404 error from URLs
		tryCatch.W.E <- function(expr){
			W <- NULL
		    w.handler <- function(w){ # warning handler
		 		W <<- w
		 		invokeRestart("muffleWarning")
		 	}
		    list(value = withCallingHandlers(tryCatch(expr, error = function(e) e), warning = w.handler), warning = W)
		}
		#for plot of overall etsy breakdown
		if(input$categories == "overall"){
			choices = c("art-and-collectibles", "home-and-living", "jewelry", "clothing", "vintage", "weddings", "craft-supplies-and-tools", "books-movies-and-music", "bags-and-purses", "shoes")
			numofitems <- rep(NA, length(choices))
			etsystring <- "https://www.etsy.com/"
			for(i in 1:length(choices)){
				ety11 <- paste(etsystring, "c/", choices[i], sep = "")
				ety1 <- read_html(ety11)
				num <- ety1 %>%
					html_nodes(".text-smaller span") %>%
					html_text()
				if(i == 5){
					num <- num[1]	
				}
				else{	
					num <- num[2]
				}		
				num <- gsub("\\(", "", num)
				num <- gsub("\n", "", num)
				num <- gsub(",", "", num)
				num <- gsub(" items", "", num)	
				num <- gsub(")", "", num)
				num <- gsub(" ", "", num)
				num <- as.numeric(num)
				
				numofitems[i] <- num
			}	
			#lbls <- paste(choices, "\n", numofitems, sep = "")
			cat2 <- choices
		}	
		else{
			etsystring <- "https://www.etsy.com/"
			ety11 <- paste(etsystring, "c/", input$categories, sep = "")
			ety1 <- read_html(ety11)
			#iterate through to scrape sub categories
			if(input$categories == 'vintage'){
				cat2 <- ety1 %>%
					html_node(".pl-xs-2:nth-child(2) .text-smaller") %>%
					html_text()
				cat22 <- rep(NA, 20)
				
				for(i in 2:20){
					tcot <- tryCatch.W.E(ety1 %>%
						html_node(paste(".pl-xs-2:nth-child(", i, ") .text-smaller", sep = "")) %>%
						html_text()		
					)		
					tcot <- as.character(tcot$value)	
					tsub <- substr(tcot, 1, 5)
					if(tsub == "Error" || is.na(tcot)){
						cat22[i] <- NA
					}else{
						cat22[i] <- ety1 %>%
							html_node(paste(".pl-xs-2:nth-child(", i, ") .text-smaller", sep = "")) %>%
							html_text()		
					}	
				}
			}	
			else{
				cat2 <- ety1 %>%
					html_node(".pl-xs-4:nth-child(2) .text-smaller") %>%
					html_text()
				cat22 <- rep(NA, 20)
				#iterate through to scrape sub categories
				for(i in 2:20){
					tcot <- tryCatch.W.E(ety1 %>%
						html_node(paste(".pl-xs-4:nth-child(", i, ") .text-smaller", sep = "")) %>%
						html_text()		
					)		
					tcot <- as.character(tcot$value)	
					tsub <- substr(tcot, 1, 5)
					if(tsub == "Error" || is.na(tcot)){
						cat22[i] <- NA
					}else{
						cat22[i] <- ety1 %>%
							html_node(paste(".pl-xs-4:nth-child(", i, ") .text-smaller", sep = "")) %>%
							html_text()		
					}	
				}	
			}
			cat22 <- cat22[!is.na(cat22)]
		
			cat22 <- substr(cat22, 0, (nchar(cat22)-1))
		
			cat2 <- cat22
			
			numofitems <- rep(NA, length(cat2))
			#reformat subcategories
			for(j in 1:length(cat2)){
				gp <- grepl("&", cat2[j])
				if(gp == TRUE){
					cat2[j] <- gsub(" & ", "-and-", cat2[j])	
				}
				gp <- grepl(" ", cat2[j])
				if(gp == TRUE){
					cat2[j] <- gsub(" ", "-", cat2[j])	
				}	
				gp <- grepl(",", cat2[j])
				if(gp == TRUE){
					cat2[j] <- gsub(",", "", cat2[j])	
				}	
				gp <- grepl("é", cat2[j])
				if(gp == TRUE){
					cat2[j] <- gsub("é", "e", cat2[j])	
				}	
				gp <- grepl("'", cat2[j])
				if(gp == TRUE){
					cat2[j] <- gsub("'", "", cat2[j])	
				}	
				gp <- grepl("<U+00E9>", cat2[j])
				if(gp == TRUE){
					cat2[j] <- gsub("<U+00E9>", "e", cat2[j])	
				}			
			}		
			cat2 <- tolower(cat2)
			#iterate through subcategories to get number of items
			for(i in 1:length(cat2)){
				print(i)
				if(i == 2 & input$categories == 'home-and-living'){
					cat2[i] <- "home-decor"	
				}	
				if(cat2[i] == "cuff-links-and-tie-clips"){
					cat2[i] <- 	"cufflinks-and-tie-tacks"
				}	
				if(cat2[i] == "outdoor-and-gardening"){
					cat2[i] <- "outdoor-and-garden"	
				}	
				if(cat2[i] == "sports-and-outdoor-recreation"){
					cat2[i] <- "sports-and-outdoor-games"	
				}	
				ety22 <- paste(ety11, "/", cat2[i], sep = "")	
				print(ety22)
				ety2 <- read_html(ety22)
			
				if(input$categories == 'vintage'){
					num <- ety2 %>%
						html_nodes(".text-smaller span") %>%
						html_text()
					num <- num[2]
					num <- gsub("\\(", "", num)
					num <- gsub("\n", "", num)
					num <- gsub(",", "", num)
					num <- gsub(" items", "", num)	
					num <- gsub(")", "", num)
					num <- gsub(" ", "", num)
					num <- as.numeric(num)
				
					

					numofitems[i] <- num
			
		
				}
				else{	
					num <- ety2 %>%
						html_nodes("span:nth-child(6)") %>%
						html_text()
				
					num <- num[4]	
					num <- gsub("\\(", "", num)
					num <- gsub("\n", "", num)
					num <- gsub(",", "", num)
					num <- gsub(" items", "", num)	
					num <- gsub(")", "", num)
					num <- gsub(" ", "", num)
					num <- as.numeric(num)

					numofitems[i] <- num

				}
			}

		}	
		pie(numofitems, labels = cat2)


	})	
	
	
	
	
	}
)	






















