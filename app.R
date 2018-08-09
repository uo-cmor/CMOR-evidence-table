# Load libraries
library(shiny)
library(DT)
library(tidyverse)

# Load data (evidence tables)
interventionNames <- c("Exercise", "NSAIDs", "Surgery", "Education")
attributeNames <- list(Effectiveness = c("Low", "Medium", "High"),
											 "Risk of Harm" = c("Low", "Medium", "High"))
evidenceTables <- list(
	"Default evidence table" = matrix(c(2,1,3,2,1,2,3,2), ncol = 2, 
																		dimnames = list("Intervention" = interventionNames,
																										"Attribute" = names(attributeNames))),
	"Alternative evidence table" = matrix(c(1,1,3,3,1,2,3,3), ncol = 2,
																				dimnames = list("Intervention" = interventionNames,
																												"Attribute" = names(attributeNames))))
interventionTypes <- c(1, 2, 3, 4)
evidenceTablesWeight <- c(1, 0.5)
attributeLevels <- list(Effectiveness = c(0, 0.5, 1),
												"Risk of Harm" = c(1, 0.5, 0))


# Source scripts


# Define UI
ui <- fluidPage(
	titlePanel("CMOR Evidence Table and Preference Ranking for OA Interventions"),
	
	sidebarLayout(
		sidebarPanel(checkboxGroupInput("evidence", "Evidence:", 
																		choices = list("Default evidence table" = 1,
																									 "Alternative evidence table" = 2), 
																		selected = 1),
								 checkboxGroupInput("interventions", "Interventions:", 
								 									  choices = list("Physical" = 1,
								 									 							   "Pharmacological" = 2,
								 									 							   "Surgical" = 3,
								 									 							   "Other" = 4), 
								 									  selected = c(1, 2, 3, 4)),
								 strong("Preference Weights:"),
								 sliderInput("wgt_eff", "Effectiveness", 0, 100, 50, 1),
								 sliderInput("wgt_harm", "Risk of harm", 0, 100, 50, 1),
								 checkboxGroupInput("cost_effectiveness", "Cost-effectiveness results:",
								 									  choices = list("NZ health system perspective" = 1,
								 									  							 "NZ societal perspective" = 2))),
		mainPanel(
			tabsetPanel(type="tabs",
									tabPanel("Evidence Table", DTOutput("selectedEvidenceTable", width = "60%")),
									tabPanel("Plot", plotOutput("preferencePlot")))
		)
	)
)


# Define server logic
server <- function(input, output) {
	# Reactive expressions (based on input values)
	evidence <- reactive({
		as.integer(input$evidence)
	})
	sourceTables <- reactive({
		abind::abind(evidenceTables[evidence()], rev.along = 0)[interventionTypes %in% input$interventions, , ,
																														drop = FALSE]
	})
	preferenceTables <- reactive({
		out <- apply(sourceTables(), 3, function(t) sapply(1:2, function(d) attributeLevels[[d]][t[, d]]))
		dim(out) <- dim(sourceTables())
		dimnames(out) <- dimnames(sourceTables())
		
		out
	})
	preferenceWeights <- reactive({
		out <- setNames(c(input$wgt_eff, input$wgt_harm),
										names(attributeNames))
		out / sum(out) * 100
	})
	preferenceScores <- reactive({
		setNames(rowMeans(apply(preferenceTables(), 3, function(x) x %*% preferenceWeights())),
						 interventionNames[interventionTypes %in% input$interventions])
	})
	
	# Output values (based on reactive expressions)
	output$selectedEvidenceTable <- renderDT({
		tbls <- sourceTables()
		out <- round(apply(sourceTables(), c(1, 2), weighted.mean, w = evidenceTablesWeight[evidence()]))
		out <- cbind(interventionNames[interventionTypes %in% input$interventions],
								 sapply(1:ncol(out), function(d) attributeNames[[d]][out[, d]]),
								 preferenceScores())
		colnames(out) <- c("Intervention", names(attributeNames), "Preference score")
		
		out[order(preferenceScores(), decreasing = TRUE), ]
	},
	rownames = FALSE,
	options = list(pageLength = 20))
	
	output$preferencePlot <- renderPlot({
		apply(preferenceTables(), c(1, 2), weighted.mean, w = evidenceTablesWeight[evidence()]) %>%
			plyr::aaply(1, function(x) x * preferenceWeights()) %>%
			as_tibble(rownames = "Intervention") %>%
			gather("attribute", "value", 2:3) %>%
			mutate(Intervention = fct_reorder(factor(Intervention), value, sum),
						 attribute = factor(attribute, levels = names(attributeNames))) %>%
			ggplot(aes(Intervention, value, fill = attribute)) + 
				geom_col() + 
				coord_flip() +
				scale_fill_brewer("Attribute", type = "qual") +
				scale_y_continuous(NULL, limits = c(0, 100))
	})
}


# Run app
shinyApp(ui = ui, server = server)
