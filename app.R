# Load libraries
library(shiny)
library(DT)
library(tidyverse)

# Load data (evidence tables)
load("data/evidenceTables.Rdata")


# Source scripts


# Define UI
ui <- fluidPage(
	titlePanel("CMOR Evidence Table and Preference Ranking for OA Interventions"),
	sidebarLayout(
		sidebarPanel(width = 3,
			fluidRow(
				column(4, 
					radioButtons("diseaseStage", "Disease Stage",
											 choices = list("Overall" = "Overall", "Early" = "Early", "Mid" = "Mid", "Late" = "Late"),
											 selected = "Overall")
				),
				column(4,
					checkboxGroupInput("evidence", "Source of Evidence:", choices = list("RACGP" = 1), selected = 1)
				),
				column(4,
					checkboxGroupInput("interventions", "Interventions:", choices = list("All" = 1), selected = c(1))
				)
			),
			hr(),
			checkboxGroupInput("cost_effectiveness", "Cost-effectiveness results:",
												 choices = list("NZ health system perspective" = 1, "NZ societal perspective" = 2)),
			hr(),
			fluidRow(
				column(6, strong("Preference Weights:")),
				column(6,
					actionButton("resetWeights", "Reset"),
					actionButton("normaliseWeights", "Normalise")
				)
			), br(),
			fluidRow(
				column(4, br(), strong(names(attributeNames)[[1]])),
				column(8, sliderInput("wgt_rec", NULL, 0, 100, attributeWeights[[names(attributeNames)[[1]]]], 0.1))
			),
			fluidRow(
				column(4, style="padding-top:2%;", strong(names(attributeNames)[[2]])),
				column(8, sliderInput("wgt_qua", NULL, 0, 100, attributeWeights[[names(attributeNames)[[2]]]], 0.1))
			),
			fluidRow(
				column(4, br(), strong(names(attributeNames)[[3]])),
				column(8, sliderInput("wgt_cos", NULL, 0, 100, attributeWeights[[names(attributeNames)[[3]]]], 0.1))
			),
			fluidRow(
				column(4, style="padding-top:2%;", strong(names(attributeNames)[[4]])),
				column(8, sliderInput("wgt_dur", NULL, 0, 100, attributeWeights[[names(attributeNames)[[4]]]], 0.1))
			),
			fluidRow(
				column(4, br(), strong(names(attributeNames)[[5]])),
				column(8, sliderInput("wgt_acc", NULL, 0, 100, attributeWeights[[names(attributeNames)[[5]]]], 0.1))
			),
			fluidRow(
				column(4, strong(names(attributeNames)[[6]])),
				column(8, sliderInput("wgt_rmi", NULL, 0, 100, attributeWeights[[names(attributeNames)[[6]]]], 0.1))
			),
			fluidRow(
				column(4, style="padding-top:2%;", strong(names(attributeNames)[[7]])),
				column(8, sliderInput("wgt_rse", NULL, 0, 100, attributeWeights[[names(attributeNames)[[7]]]], 0.1))
			),
			fluidRow(
				column(4, style="padding-top:2%;", strong(names(attributeNames)[[8]])),
				column(8, sliderInput("wgt_eff", NULL, 0, 100, attributeWeights[[names(attributeNames)[[8]]]], 0.1))
			),
			fluidRow(
				column(4, style="padding-top:2%;", strong(names(attributeNames)[[9]])),
				column(8, sliderInput("wgt_fun", NULL, 0, 100, attributeWeights[[names(attributeNames)[[9]]]], 0.1))
			)
		),
		mainPanel(width = 9,
			tabsetPanel(type="tabs",
									tabPanel("Evidence Table", DTOutput("selectedEvidenceTable")),
									tabPanel("Plot", plotOutput("preferencePlot")))
		)
	)
)


# Define server logic
server <- function(input, output, session) {
	# Reactive expressions (based on input values)
	evidence <- reactive({
		as.integer(input$evidence)
	})
	sourceTables <- reactive({
		abind::abind(evidenceTables[[input$diseaseStage]][evidence()], 
								 rev.along = 0)[interventionTypes %in% input$interventions, , , drop = FALSE]
	})
	preferenceTables <- reactive({
		out <- apply(sourceTables(), 3,
								 function(t) sapply(1:length(attributeNames), function(d) attributeLevels[[d]][t[, d]]))
		dim(out) <- dim(sourceTables())
		dimnames(out) <- dimnames(sourceTables())
		
		out
	})
	preferenceWeights <- reactive({
		out <- setNames(c(input$wgt_rec, input$wgt_qua, input$wgt_cos, input$wgt_dur, input$wgt_acc, 
											input$wgt_rmi, input$wgt_rse, input$wgt_eff, input$wgt_fun), names(attributeNames))
		
		out / sum(out) * 100
	})
	preferenceScores <- reactive({
		setNames(rowMeans(apply(preferenceTables(), 3, function(x) x %*% preferenceWeights())),
						 interventionNames[interventionTypes %in% input$interventions])
	})
	
	# Output values (based on reactive expressions)
	output$selectedEvidenceTable <- renderDT({
		out <- round(apply(sourceTables(), c(1, 2), weighted.mean, w = evidenceTablesWeight[evidence()]))
		out <- cbind(interventionNames[interventionTypes %in% input$interventions],
								 sapply(1:ncol(out), function(d) attributeNames[[d]][out[, d]]),
								 round(preferenceScores(), 1))
		colnames(out) <- c("Intervention", names(attributeNames), "Preference score")
		
		out[order(preferenceScores(), decreasing = TRUE), ]
	},
	rownames = as.character(1:(dim(sourceTables())[[1]])),
	autoHideNavigation = TRUE,
	options = list(pageLength = 20))
	
	output$preferencePlot <- renderPlot({
		apply(preferenceTables(), c(1, 2), weighted.mean, w = evidenceTablesWeight[evidence()]) %>%
			plyr::aaply(1, function(x) x * preferenceWeights()) %>%
			as_tibble(rownames = "Intervention") %>%
			gather("attribute", "value", -1) %>%
			mutate(Intervention = fct_reorder(factor(Intervention), value, sum),
						 attribute = factor(attribute, levels = names(attributeNames))) %>%
			ggplot(aes(Intervention, value, fill = attribute)) + 
				geom_col(colour = "white") + 
				coord_flip() +
				scale_fill_brewer("Attribute", type = "qual", palette = "Paired") +
				scale_y_continuous(NULL, limits = c(0, 100), expand = c(0, 0))
	},
	height = function() 12 * dim(preferenceTables())[[1]])
	
	output$preferenceWeightsPlot <- renderPlot({
		ggplot(tibble(att = factor(names(preferenceWeights()), levels = rev(names(preferenceWeights()))), 
									wgt = preferenceWeights()), 
					 aes(0, wgt, fill = att)) + 
			geom_col(position = "stack") +
			scale_y_continuous(NULL, expand = c(0, 0), breaks = seq(0, 100, 5), minor_breaks = NULL) +
			scale_fill_brewer(NULL, type = "qual", palette = "Paired", guide = FALSE) +
			coord_flip() +
			theme_minimal() + 
			theme(axis.text.y = element_blank(), axis.title.y = element_blank(), 
						axis.line.x = element_line(colour = "black"), axis.ticks.x = element_line(colour = "black"), 
						panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
						panel.grid.major.x = element_line(colour = "black"),
						plot.background = element_rect(fill = "#FFFFFF00", linetype = 0), 
						panel.background = element_blank(), panel.ontop = TRUE, 
						plot.margin = margin(0, 0, 0, 0))
	})
	
	# Update input widgets
	observe({
		input$resetWeights
		
		isolate(updateSliderInput(session, "wgt_rec", value = attributeWeights[[1]]))
		isolate(updateSliderInput(session, "wgt_qua", value = attributeWeights[[2]]))
		isolate(updateSliderInput(session, "wgt_cos", value = attributeWeights[[3]]))
		isolate(updateSliderInput(session, "wgt_dur", value = attributeWeights[[4]]))
		isolate(updateSliderInput(session, "wgt_acc", value = attributeWeights[[5]]))
		isolate(updateSliderInput(session, "wgt_rmi", value = attributeWeights[[6]]))
		isolate(updateSliderInput(session, "wgt_rse", value = attributeWeights[[7]]))
		isolate(updateSliderInput(session, "wgt_eff", value = attributeWeights[[8]]))
		isolate(updateSliderInput(session, "wgt_fun", value = attributeWeights[[9]]))
	})
	
	observe({
		input$normaliseWeights
		wgt <- isolate(preferenceWeights())
		
		updateSliderInput(session, "wgt_rec", value = wgt[[1]])
		updateSliderInput(session, "wgt_qua", value = wgt[[2]])
		updateSliderInput(session, "wgt_cos", value = wgt[[3]])
		updateSliderInput(session, "wgt_dur", value = wgt[[4]])
		updateSliderInput(session, "wgt_acc", value = wgt[[5]])
		updateSliderInput(session, "wgt_rmi", value = wgt[[6]])
		updateSliderInput(session, "wgt_rse", value = wgt[[7]])
		updateSliderInput(session, "wgt_eff", value = wgt[[8]])
		updateSliderInput(session, "wgt_fun", value = wgt[[9]])
	})
}


# Run app
shinyApp(ui = htmlTemplate("www/index.html"), server = server)
