# Load libraries
library(shiny)
library(DT)
library(plyr)
library(tidyverse)
library(abind)

# Load data (evidence tables)
load("data/evidenceTables.Rdata")


# Source scripts
source("scripts/plot-utils.R")

# Define UI
ui <- fluidPage(
	titlePanel("CMOR Evidence Table and Preference Ranking for OA Interventions", windowTitle = "CMOR Evidence Table"),
	h3(style="background-color: yellow; text-align: center;", strong("***PRELIMINARY RESULTS: NOT FOR CITATION***")),
	h3("Introduction"),
	p("This application provides access to evidence tables for osteoarthritis interventions used in the project",
		em("The impact and management of rising osteoarthritis burden"), "(Prof Haxby Abbott, Centre for Musculoskeletal",
		"Outcomes Research, University of Otago)."), 
	p("To get started, select the desired options in the",
		span(class="hidden-xs hidden-sm hidden-md", "options panel to the left,"),
		span(class="hidden-lg", "Options, Interventions, and Preference Weights drop-down tabs above, "),
		"and view the 'Evidence Table' or 'Plot' tabs to see the results."),
	h3("How it works:"),
	p("In this application, interventions for the clinical management of knee osteoarthritis are characterised according",
		"to eight attributes: recommendation (appropriateness for the stage of the disease), quality of evidence, cost,",
		"duration of effect, accessibility, risk of mild/moderate harm, risk of serious harm, and effectiveness. Each",
		"attribute describes between 3 and 5 levels of performance, in increasing order of desirability (for the details",
		"of the levels described by each attribute, click the help icon", a(icon("info-circle")), "next to the attribute",
		"heading in the Preference Weights panel)."),
	p("In our study, the performance of each intervention according to these eight attributes was informed by the Royal",
		"Australian College of General Practitioners' Clinical Practice Guidelines for osteoarthritis, supplemented where",
		"necessary by additional sources including (in decreasing order of preference) systematic reviews and",
		"meta-analyses, other published data, and research team judgement. This performance scoring can differ at",
		"different stages of the disease course (for example, total joint replacement has little evidence and is",
		"generally not recommended/appropriate for early-stage disease, but is strongly recommended for patients with",
		"severe late-stage disease); to incorporate this in the ranking model, evidence tables can be constructed for",
		"early, mid, or late-stage disease using the 'Disease Stage' option. In future iterations of this evidence table,",
		"we will incorporate the option to select different sources of evidence to inform the table and resulting",
		"preference weights."),
	p("To avoid potential biases in stakeholder evaluations of the interventions due to prior beliefs or pre-conceived",
		"ideas about different treatments, preference weights, or the relative values assigned to each intervention, were",
		"derived indirectly by obtaining preference weights for the eight attributes, which can then be summed to derive",
		"an aggregate preference score for each intervention. The preference weights presented here were derived from a",
		"study of the preferences of key New Zealand stakeholder groups (consumers/OA patients,", HTML("M&amacr;ori"), 
		"health advocates, health care providers, health policy makers, and content area experts). To investigate the",
		"effect on preference rankings using any other set of preference weights, move the sliders/numeric inputs in the",
		"Preference Weights panel."),
	p("The Evidence Table can be sorted by intervention name, attribute, or preference score by clicking on the column",
		"headers, and searched for specific interventions or attribute levels using the search box"),
	p("You can also click on the bar for an intervention in the Plot to provide a summary of the attribute levels and",
		"associated preference weights for that intervention. Click elsewhere on the plot to remove the summary."),
	h3("Contact"),
	p("For enquiries, contact Dr Ross Wilson, Centre for Musculoskeletal Outcomes Research, University of Otago",
		a(href="mailto:ross.wilson@otago.ac.nz?Subject=CMOR%20Evidence%20Table", "(ross.wilson@otago.ac.nz)")),
	sidebarLayout(
		sidebarPanel(width = 3,
			fluidRow(
				column(4, 
					strong("Disease Stage:"), actionLink("helpDiseaseStage", NULL, icon("info-circle")),
					radioButtons("diseaseStage", NULL,
											 choices = list("Overall", "Early", "Mid", "Late"),
											 selected = "Overall"),
					strong("Source of Evidence:"), actionLink("helpEvidence", NULL, icon("info-circle")),
					checkboxGroupInput("evidence", NULL, choices = list("RACGP" = 1), selected = 1)
				),
				column(8,
							 strong("Interventions:"), actionLink("helpInterventions", NULL, icon("info-circle")), br(),
							 actionLink("allInterventions", "(select all)"), actionLink("noInterventions", "(select none)"),
							 checkboxGroupInput("interventionTypes", NULL, 
							 									 choices = unique(interventionTypes), selected = unique(interventionTypes)),
							 fluidRow(
						column(8, strong("Filter:")), column(2, actionLink("clearFilter", "(Clear)"))
					),
					selectInput("interventions", NULL, multiple = TRUE, 
											choices = c("(All)" = "", interventionList))
				)
			),
			hr(),
			fluidRow(
				column(7, strong("Preference Weights:"), actionLink("helpPreferenceWeights", NULL, icon("info-circle")),
							 br(), actionLink("resetWeights", "(Reset)")),
				column(5, style = "text-align:right",
					actionLink("normaliseWeights", HTML("Normalise Weights<br>(sum to 100)"))
				)
			), br(),
			fluidRow(
				column(9, 
					strong("Recommendation"), actionLink("helpRec", NULL, icon("info-circle")),
					sliderInput("wgt_rec", NULL, 0, 100, attributeWeights[["Recommendation"]], 0.1)
				),
				column(3, br(), numericInput("wgt_rec_val", NULL, attributeWeights[["Recommendation"]], 0, 100, 0.1))
			),
			fluidRow(
				column(9,
					strong("Quality of Evidence"), actionLink("helpQua", NULL, icon("info-circle")),
					sliderInput("wgt_qua", NULL, 0, 100, attributeWeights[["Quality of Evidence"]], 0.1)
				),
				column(3, br(), numericInput("wgt_qua_val", NULL, attributeWeights[["Quality of Evidence"]], 0, 100, 0.1))
			),
			fluidRow(
				column(9, 
					strong("Cost"), actionLink("helpCos", NULL, icon("info-circle")),
					sliderInput("wgt_cos", NULL, 0, 100, attributeWeights[["Cost"]], 0.1)
				),
				column(3, br(), numericInput("wgt_cos_val", NULL, attributeWeights[["Cost"]], 0, 100, 0.1))
			),
			fluidRow(
				column(9,
					strong("Duration of Effect"), actionLink("helpDur", NULL, icon("info-circle")),
					sliderInput("wgt_dur", NULL, 0, 100, attributeWeights[["Duration of Effect"]], 0.1)
				),
				column(3, br(), numericInput("wgt_dur_val", NULL, attributeWeights[["Duration of Effect"]], 0, 100, 0.1))
			),
			fluidRow(
				column(9,
					strong("Accessibility"), actionLink("helpAcc", NULL, icon("info-circle")),
					sliderInput("wgt_acc", NULL, 0, 100, attributeWeights[["Accessibility"]], 0.1)
				),
				column(3, br(), numericInput("wgt_acc_val", NULL, attributeWeights[["Accessibility"]], 0, 100, 0.1))
			),
			fluidRow(
				column(9, 
					strong("Risk of Mild/Moderate Harm"), actionLink("helpRmi", NULL, icon("info-circle")),
					sliderInput("wgt_rmi", NULL, 0, 100, attributeWeights[["Risk of Mild/Moderate Harm"]], 0.1)
				),
				column(3, 
					br(), numericInput("wgt_rmi_val", NULL, attributeWeights[["Risk of Mild/Moderate Harm"]], 0, 100, 0.1)
				)
			),
			fluidRow(
				column(9, 
					strong("Risk of Serious Harm"), actionLink("helpRse", NULL, icon("info-circle")),
					sliderInput("wgt_rse", NULL, 0, 100, attributeWeights[["Risk of Serious Harm"]], 0.1)
				),
				column(3, br(), numericInput("wgt_rse_val", NULL, attributeWeights[["Risk of Serious Harm"]], 0, 100, 0.1))
			),
			fluidRow(
				column(9,
					strong("Effectiveness"), actionLink("helpEff", NULL, icon("info-circle")),
					sliderInput("wgt_eff", NULL, 0, 100, attributeWeights[["Effectiveness"]], 0.1)
				),
				column(3, br(), numericInput("wgt_eff_val", NULL, attributeWeights[["Effectiveness"]], 0, 100, 0.1))
			)
		),
		mainPanel(width = 9,
			tabsetPanel(type="tabs",
									tabPanel("Evidence Table", DTOutput("selectedEvidenceTable")),
									tabPanel("Plot", plotOutput("preferencePlot", click = clickOpts("plot_click", clip = FALSE))))
		)
	)
)


# Define server logic
server <- function(input, output, session) {
	# Reactive expressions (based on input values)
	evidence <- reactive({
		as.integer(input$evidence)
	})
	selected <- reactive({
		if (isTruthy(input$interventions)) {
			rownames(evidenceTables[[input$diseaseStage]][[1]]) %in% input$interventions
		} else interventionTypes %in% input$interventionTypes
	})
	
	sourceTables <- reactive({
		abind(evidenceTables[[input$diseaseStage]][evidence()], rev.along = 0)[selected(), , , drop = FALSE]
	})
	sourceTablesDetails <- reactive({
		abind(evidenceTablesDetails[[input$diseaseStage]][evidence()], rev.along = 0)[selected(), , , drop = FALSE]
	})
	
	preferenceTables <- reactive({
		sourceTables <- req(sourceTables())
		
		out <- apply(sourceTables, 3,
								 function(t) sapply(1:length(attributeNames), function(d) attributeLevels[[d]][t[, d]]))
		dim(out) <- dim(sourceTables)
		dimnames(out) <- dimnames(sourceTables)
		
		out
	})
	preferenceWeights <- reactive({
		out <- setNames(c(input$wgt_rec, input$wgt_qua, input$wgt_cos, input$wgt_dur, input$wgt_acc, 
											input$wgt_rmi, input$wgt_rse, input$wgt_eff), names(attributeNames))
		
		out / sum(out) * 100
	})
	preferenceScores <- reactive({
		pref <- apply(preferenceTables(), 3, function(x) x %*% preferenceWeights())
		dim(pref) <- dim(preferenceTables())[c(1, 3)]
		
		setNames(rowMeans(pref), interventionNames[selected()])
	})
	
	printTableValues <- reactive({
		sourceTables <- req(sourceTables())
		
		out <- as.data.frame(round(apply(sourceTables, c(1, 2), weighted.mean, w = evidenceTablesWeight[evidence()]))) %>%
			rownames_to_column("Intervention")
		l_ply(names(attributeNames), function(nm) out[[nm]] <<- factor(out[[nm]], levels = seq_along(attributeNames[[nm]]), 
																																	 labels = attributeNames[[nm]]))
		
		out <- out %>%
			mutate("Preference Score" = preferenceScores()) %>%
			arrange(desc(`Preference Score`)) %>%
			mutate("Preference Score" = round(`Preference Score`, 1))
		
		out
	})
	printTableDetails <- reactive({
		sourceTablesDetails <- req(sourceTablesDetails())
		
		if (dim(sourceTablesDetails)[[3]] == 1) {
			out <- drop(sourceTablesDetails)
		} else {
			out <- apply(sourceTablesDetails, c(1, 2), function(x) paste0(names(x), ": ", x, collapse = "\n"))
		}
		
		dim(out) <- dim(sourceTablesDetails)[1:2]
		dimnames(out) <- dimnames(sourceTablesDetails)[1:2]
		
		arrange(as.data.frame(out), desc(preferenceScores()))
	})
	printTable <- reactive({
		printTableValues <- req(printTableValues())
		printTableDetails <- req(printTableDetails())
		
		as_tibble(setNames(lapply(names(printTableValues), 
															function(x) {
																if (!is.null(printTableDetails[[x]])) {
																	paste0("<span title = \"", printTableDetails[[x]], "\"> ", 
																				 printTableValues[[x]], "</span>")
																	} else printTableValues[[x]] 
																}),
											 names(printTableValues)))
	})
	
	plotdata <- reactive({
		req(preferenceTables())
		
		apply(preferenceTables(), c(1, 2), weighted.mean, w = evidenceTablesWeight[evidence()]) %>%
			plyr::aaply(1, function(x) x * preferenceWeights(), .drop = FALSE) %>%
			as_tibble(rownames = "Intervention") %>%
			gather("attribute", "value", -1) %>%
			mutate(Intervention = fct_reorder(factor(Intervention), value, sum),
						 attribute = factor(attribute, levels = names(attributeNames)))
	})
	
	selectedIntervention <- reactiveValues( # For storing which point to show label for
		name = character(0), 
		label = character(0), 
		x = numeric(0), 
		y = numeric(0)
	)
	
	observeEvent(plotdata(), {
		selectedIntervention$name <- character(0)
		selectedIntervention$label <- character(0)
		selectedIntervention$x <- numeric(0)
		selectedIntervention$y <- numeric(0)
	})
	
	observeEvent(input$plot_click, {
		sel <- nearBars(plotdata() %>% spread(attribute, value) %>% 
											transmute(Intervention, wgt = rowSums(select(., names(attributeNames)))), 
										input$plot_click, xvar = "wgt",
										threshold = 0, maxpoints = 1)$Intervention
		labels <- printTableValues() %>% filter(Intervention %in% sel)
		values <- plotdata() %>% filter(Intervention %in% sel) %>% spread(attribute, value)
		if (length(sel)>0) values <- values %>% mutate_at(names(attributeNames), round, 1)
		
		selectedIntervention$name <- sel
		selectedIntervention$label <- 
			paste0(
				"Recommendation = ", labels$Recommendation, " (", values$Recommendation, ")",
				"\nQuality of Evidence = ", labels$`Quality of Evidence`, " (", values$`Quality of Evidence`, ")",
				"\nCost = ", labels$Cost, " (", values$Cost, ")",
				"\nDuration of Effect = ", labels$`Duration of Effect`, " (", values$`Duration of Effect`, ")",
				"\nAccessibility = ", labels$Accessibility, " (", values$Accessibility, ")",
				"\nRisk of Mild/Moderate Harm = ", 
				labels$`Risk of Mild/Moderate Harm`, " (", values$`Risk of Mild/Moderate Harm`, ")",
				"\nRisk of Serious Harm = ", labels$`Risk of Serious Harm`, " (", values$`Risk of Serious Harm`, ")",
				"\nEffectiveness = ", labels$Effectiveness, " (", values$Effectiveness, ")"
			)
		selectedIntervention$x <- input$plot_click$y
		selectedIntervention$y <- input$plot_click$x
	}, ignoreNULL = TRUE)

	# Output values (based on reactive expressions)
	output$selectedEvidenceTable <- renderDT({
		printTableValues <- req(printTableValues())
		printTableDetails <- req(printTableDetails())
		printTable <- bind_cols(req(printTable()), printTableValues)
		
		indices_disp <- 1:(ncol(printTable) / 2)
			# which(sapply(names(printTable), function(x) !is.null(printTableDetails[[x]])))
		indices_sort <- (ncol(printTable) / 2 + 1):ncol(printTable)
		
		datatable(printTable,
							autoHideNavigation = TRUE,
							escape = c(TRUE, sapply(names(printTable), function(x) is.null(printTableDetails[[x]]))),
							class = "display compact",
							options = list(
								pageLength = 25,
								columnDefs = c(
									unname(lapply(indices_disp, function(x) list(targets = x, orderData = x + ncol(printTable) / 2))),
									unname(lapply(indices_sort, function(x) list(targets = x, visible = FALSE, searchable = FALSE)))
								)
							))
	})
	
	output$preferencePlot <- renderPlot({
		plotdata <- req(plotdata())
		labdata <- tibble(x = selectedIntervention$x,
											y = selectedIntervention$y,
											name = selectedIntervention$name,
											label = selectedIntervention$label)

		ggplot(plotdata, aes(Intervention, value)) + 
			geom_col(aes(fill = attribute), colour = "white") + 
			geom_col(aes(group = attribute), fill = NA, size = 1, colour = "black",
							 data = filter(plotdata, Intervention %in% labdata$name), show.legend = FALSE) +
			geom_label(aes(x, y, label = label), data = labdata, show.legend = FALSE,
								 hjust = "inward", vjust = "inward") +
			coord_flip() +
			scale_fill_brewer(NULL, type = "qual", palette = "Paired") +
			scale_y_continuous(NULL, limits = c(0, 100), expand = c(0, 0), sec.axis = dup_axis(), 
												 breaks = seq(0, 90, 10), minor_breaks = NULL) +
			scale_x_discrete(NULL) +
			guides(fill = guide_legend(reverse = TRUE, label.position = "bottom", 
																 byrow = ifelse(session$clientData$output_preferencePlot_width >= 768, FALSE, TRUE),
																 nrow = ifelse(session$clientData$output_preferencePlot_width >= 1000, 1, 
																 							 ifelse(session$clientData$output_preferencePlot_width >= 768, 2, 4)),
																 keywidth = unit(ifelse(session$clientData$output_preferencePlot_width >= 1050, 3, 1), 
																 								"lines"))) +
			theme(legend.position = "bottom",
						legend.justification = c(0, 0),
						legend.background = element_rect(fill = NA, linetype = 0),
						legend.key = element_rect(fill = NA, linetype = 0),
						plot.margin = margin(0, 0, 0, 5, "pt"))
	},
	height = function() max(280, 12 * dim(preferenceTables())[[1]]))

	# Update input widgets
	observe({
		input$resetWeights
		
		updateSliderInput(session, "wgt_rec", value = attributeWeights[[1]])
		updateSliderInput(session, "wgt_qua", value = attributeWeights[[2]])
		updateSliderInput(session, "wgt_cos", value = attributeWeights[[3]])
		updateSliderInput(session, "wgt_dur", value = attributeWeights[[4]])
		updateSliderInput(session, "wgt_acc", value = attributeWeights[[5]])
		updateSliderInput(session, "wgt_rmi", value = attributeWeights[[6]])
		updateSliderInput(session, "wgt_rse", value = attributeWeights[[7]])
		updateSliderInput(session, "wgt_eff", value = attributeWeights[[8]])
	})
	
	observeEvent(input$normaliseWeights, {
		wgt <- preferenceWeights()
		
		updateSliderInput(session, "wgt_rec", value = wgt[[1]])
		updateSliderInput(session, "wgt_qua", value = wgt[[2]])
		updateSliderInput(session, "wgt_cos", value = wgt[[3]])
		updateSliderInput(session, "wgt_dur", value = wgt[[4]])
		updateSliderInput(session, "wgt_acc", value = wgt[[5]])
		updateSliderInput(session, "wgt_rmi", value = wgt[[6]])
		updateSliderInput(session, "wgt_rse", value = wgt[[7]])
		updateSliderInput(session, "wgt_eff", value = wgt[[8]])
	})
	
	observe({ updateSliderInput(session, "wgt_rec", value = input$wgt_rec_val) })
	observe({ updateSliderInput(session, "wgt_qua", value = input$wgt_qua_val) })
	observe({ updateSliderInput(session, "wgt_cos", value = input$wgt_cos_val) })
	observe({ updateSliderInput(session, "wgt_dur", value = input$wgt_dur_val) })
	observe({ updateSliderInput(session, "wgt_acc", value = input$wgt_acc_val) })
	observe({ updateSliderInput(session, "wgt_rmi", value = input$wgt_rmi_val) })
	observe({ updateSliderInput(session, "wgt_rse", value = input$wgt_rse_val) })
	observe({ updateSliderInput(session, "wgt_eff", value = input$wgt_eff_val) })

	observeEvent(input$interventionTypes, { 
		updateSelectInput(session, "interventions", 
											choices = if (isTruthy(input$interventionTypes)) {
												c("(All)" = "", interventionList[input$interventionTypes]) 
											} else c("None Selected!" = ""),
											selected = "")},
		ignoreInit = TRUE, ignoreNULL = FALSE)
	observeEvent(input$clearFilter, { updateSelectInput(session, "interventions", selected = "")},
							 ignoreInit = TRUE)
	
	observeEvent(input$allInterventions, {
		updateCheckboxGroupInput(session, "interventionTypes", selected = unique(interventionTypes))
	})
	
	observeEvent(input$noInterventions, {
		updateCheckboxGroupInput(session, "interventionTypes", selected = "")
	})
}


# Run app
shinyApp(ui = htmlTemplate("www/index.html"), server = server)
