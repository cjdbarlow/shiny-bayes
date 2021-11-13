# Load Packages
library(shiny)

# Define the UI
ui <- fluidPage(
  
  # Load custom CSS
  includeCSS("bayes.css"),
  
  # Title
  hr(),
  
  sidebarLayout(

    # Inputs
    sidebarPanel(
      width = 6,

      # Contingency Table
      ## Use a fluid-page layout to line up the boxes nicely
      ## numericInput used because input validation is better than with shinyMatrix
      fluidPage(
        
        fluidRow(strong("Contingency Table")),
        fluidRow(helpText("Fill in the cells of the table with the results the trial under evaluation.")),
        
        fluidRow(column(width = 3),
                 column(h5("Event Occurred"), width = 3),
                 column(h5("No Event Occurred"), width = 3),
                 column(h5("Total"), width = 3)),
        
        fluidRow(column(h5("Intervention"), width = 3),
                 column(numericInput(inputId = "int_pos",
                                     label = NULL, 
                                     value = 117,
                                     min = 0),
                        width = 3),
                 column(numericInput(inputId = "int_neg",
                                     label = NULL, 
                                     value = 138,
                                     min = 0),
                        width = 3),
                 column(verbatimTextOutput("int_tot"),
                        width = 3)),
        
        fluidRow(column(h5("Control"), width = 3),
                 column(numericInput(inputId = "con_pos",
                                     label = NULL, 
                                     value = 109,
                                     min = 0),
                        width = 3),
                 column(numericInput(inputId = "con_neg",
                                     label = NULL, 
                                     value = 145,
                                     min = 0),
                        width = 3),
                 column(verbatimTextOutput("con_tot"),
                        width = 3)),
        
        fluidRow(column(h5("Total"), width = 3),
                 column(verbatimTextOutput("tot_pos"), width = 3),
                 column(verbatimTextOutput("tot_neg"), width = 3),
                 column(verbatimTextOutput("tot_tot"), width = 3))
      ),
    
      br(),

      # Significance
      strong("Statistical Signifiance"),
      helpText("Select whether the trial reported a significant or non-significant result."),
      radioButtons("sig", label = NULL,
                   choices = list("Statistically significant" = 1,
                                  "Statistically non-significant" = 2),
                   selected = 2),
      hr(),
            
      # Pre-test Probability
      strong("Pre-Test Probability"),
      helpText("Use the slider to adjust the pre-test probability of a true effect being present."),
      sliderInput(inputId = "preprob", label = NULL,
                  min = 0.001,
                  max = 0.999,
                  value = 0.5),
      hr(),
      
      # Sampling Method
      strong("Sampling Method"),
      helpText("Select the sampling method most appropriate for recruitment strategy used."),
      selectInput(inputId = "method",
                  label = NULL,
                  choices = list("Independent Multinomial",
                                 "Joint Multinomial",
                                 "Poisson",
                                 "Hypergeometric"),
                  selected = "Independent Multinomial"),
      
      # Some information about each sampling method
      conditionalPanel(
        condition = "input.method == 'Independent Multinomial'",
        helpText("Independent multinomial sampling is used when either the row totals or column
                 totals are fixed.",
                 br(), br(),
                 "This is the recommended option for most randomised trials, including block-randomised trials."),
        br(),
        strong("Fixed row or fixed columns?"),
        helpText("Select whether the groups were balanced by assignment to the intervention or by outcome."),
        selectInput(inputId = "fixedMargin",
                    label = NULL,
                    choices = list("Intervention",
                                   "Outcome"),
                    selected = "Intervention"),
        conditionalPanel(
          condition = "input.fixedMargin == 'Intervention'",
          helpText("Fixed intervention (rows) is required for prospective trials, such as randomised trials.")
        ),
        conditionalPanel(
          condition = "input.fixedMargin == 'Outcome'",
          helpText("Fixed outcome (columns) is used when the outcome data is fixed, such as retrospective case-control studies.")
        )
      ),
      
      conditionalPanel(
        condition = "input.method == 'Joint Multinomial'",
        helpText(HTML("Joint multinomial sampling is used when the total number of patients is fixed,
                 but the row and column totals can vary. <em>e.g.</em> A convenience sample of the first 30 patients seen on a given day."))
      ),
      
      conditionalPanel(
        condition = "input.method == 'Poisson'",
        helpText("Poisson sampling is most appropriate when both the total number of patients, and the row
                 and column totals are random."),
        br(),
        helpText(HTML("This may be most appropriate for convenience samples of non-randomised data, <em>e.g.</em> 
                      Mortality of patients receiving spinal vs. general anaesthesia for hip replacement in November."))
      ),
      
      conditionalPanel(
        condition = "input.method == 'Hypergeometric'",
        helpText("Hypergeometric sampling is most appropriate when both row and column margins are fixed."),
        br(),
        helpText(HTML("Practical uses of this option are rare, and usually occur when a group is defined by an intrinsic property of that group,
                      <em>e.g.</em> by median split."))
      )
    ),

    # Outputs
    mainPanel(
      width = 6,
      conditionalPanel(
        condition = "input.int_pos > 0 & input.int_neg > 0 & input.con_pos > 0 & input.con_neg > 0",
        fluidPage(
          
          #Plots
          
          # Bayes Factor
          fluidRow(
            htmlOutput("bayes_factor"),
            br(),
            htmlOutput("bayes_desc1"),
            br(),
            htmlOutput("bayes_desc2")
            
          ),
          
          br(),
          
          # Predictive Values and False risks
          fluidRow(
            conditionalPanel(
              condition = "input.sig == 1", # Can't have a PPV/FPR if the result is negative
              
              strong("False Positive Risk: "),
              htmlOutput("fpr", inline = TRUE),
              
              helpText(HTML("The probability that the hypothesis is false (<em>i.e.</em> no effect), despite the statistically significant result.")),
              
              br(),
              
              strong("Positive Predictive Value: "),
              textOutput("ppv", inline = TRUE),
              
              helpText("Probability that the therapy is actually beneficial."),
  
            ),
            
            conditionalPanel(
              condition = "input.sig == 2",
  
              strong("False Negative Risk: "),
              htmlOutput("fnr", inline = TRUE),
              
              helpText(HTML("Probability that the hypothesis is true, despite the statistically non-significant result.")),
              
              br(),
                          
              strong("Negative Predictive Value: "),
              textOutput("npv", inline = TRUE),
              
              helpText("Probability that the intervention is actually not beneficial"),
  
            )
          )
        )
      )
    )
  ),
  
  hr(),
  
  helpText("From: Sidebotham D, Barlow CJD.", em("The False Positive and False Negative Risk for Multicentre Trials in Critical Care...")),
  br(),
  helpText(HTML("Bayes Factors calculated using the methods described in: Gunel, Erdogan, and James Dickey. 
  <em>Bayes Factors for Independence in Contingency Tables.</em> Biometrika, vol. 61, no. 3, [Oxford University Press, Biometrika Trust], 1974, pp. 545–57. 
  <a href=\"https://doi.org/10.2307/2334738\">https://doi.org/10.2307/2334738.</a>")),
  helpText(HTML("Example data from: COIITSS Study Investigators, Djillali Annane, Alain Cariou, Virginie Maxime, Elie Azoulay, Gilles D’honneur, Jean François Timsit, et al. 
  <em>Corticosteroid Treatment and Intensive Insulin Therapy for Septic Shock in Adults: A Randomized Controlled Trial</em>. 
  JAMA 303, no. 4 (27 January 2010): 341–48. <a href=\"https://doi.org/10.1001/jama.2010.2\">https://doi.org/10.1001/jama.2010.2</a>.
"))
)

