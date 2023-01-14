# Load packages
library(BayesFactor) # For Gunel-Dickey
library(abtest) # For Kass-Vaidyanathan

# Define server-side variables
thresh_low = 0.05
thresh_high = 0.2

## Calculate Bayes factor
### Generic function that does light pre-processing and passes it off to a specific function
bf_fun = function(con_neg, con_pos, int_neg, int_pos, bftype, sample, fixed) {
  
  if (bftype == "Gunel-Dickey"){
    bf = bf_gd(con_neg, con_pos, int_neg, int_pos, sample, fixed)
  } else if (bftype == "Kass-Vaidyanathan"){
    bf = bf_kv(con_neg, con_pos, int_neg, int_pos)
  }

  bf
}

bf_gd = function(con_neg, con_pos, int_neg, int_pos, sample, fixed){
  
  # Matrix expected by contingencyTableBF
  bayes_matrix = matrix(c(int_pos, con_pos, int_neg, con_neg), 2,  2)
  
  # Determine sampletype
  if (sample == "Independent Multinomial") {
    method = "indepMulti"
  } else if (sample == "Joint Multinomial") {
    method = "jointMulti"
  } else if (sample == "Poisson") {
    method = "poisson"
  } else if (sample == "Hypergeometric") {
    method = "hypergeom"
  }
  
  # Fixed rows or columns?
  if (fixed == "Intervention") {
    margin = "rows"
  } else if (fixed == "Outcome") {
    margin = "cols"
  }
  
  # Calculate, extract, return
  BFM_10 = contingencyTableBF(bayes_matrix,
                              sampleType = method,
                              fixedMargin = margin,
                              priorConcentration = 1)
  
  extractBF(BFM_10)$bf
  
}

bf_kv = function(con_neg, con_pos, int_neg, int_pos){
  # Put numbers into list with nomenclature expected for ab_test (total n rather than non-event n)
  data = list(y1 = con_pos,
              n1 = con_pos + con_neg,
              y2 = int_pos,
              n2 = int_pos + int_neg)
  
  # Calculate BF
  bf = ab_test(data = data,
               prior_par = list(mu_psi = 0, sigma_psi = 1, mu_beta = 0, sigma_beta = 1))
  
  # Extract and return BF10
  bf = bf[8]$bf
  bf10 = bf[[1]]
  bf10
}

  
# Define server logic
server <- function(input, output, session) {

  #Row and column totals
  output$int_tot = renderText({
    input$int_neg + input$int_pos
  })
  
  output$con_tot = renderText({
    input$con_neg + input$con_pos
  })
  
  output$tot_pos = renderText({
    input$int_pos + input$con_pos
  })
  
  output$tot_neg= renderText({
    input$int_neg + input$con_neg
  })
  
  output$tot_tot = renderText({
    input$int_neg + input$con_neg + input$int_pos + input$con_pos
  })
  
  #Reactive functions
  ## Calculate raw Bayes Factor (supporting the alternative over the null, i.e. BF 1:0)
  bf_raw = reactive({
    bf_fun(input$con_neg, input$con_pos, input$int_neg, input$int_pos, input$bfmethod, input$samplemethod, input$fixedMargin)
  })
  
  ##BF pretty (reports BF as either BF 1:0 or BF 0:1 so we don't have to deal with numbers < 1)
  ##This needs its output$ given via htmlOutput rather than text output
  bf_pretty = reactive({
    ifelse(bf_raw() < 1,
           round(1/bf_raw(), 2),
           round(bf_raw(), 2))
  })
  
  bf_pretty_full = reactive({
    ifelse(bf_raw() < 1,
           paste("<strong>Bayes Factor<sub>0:1</sub>:</strong>", round(1/bf_raw(), 2)),
           paste("<strong>Bayes Factor<sub>1:0</sub>:</strong>", round(bf_raw(), 2)))
  })
  
  bf_class = reactive({
    value = cut(bf_pretty(),
                breaks = c(0,1,3,10,30,100,+Inf),
                labels = c("no",
                           "only anecdotal",
                           "moderate",
                           "strong",
                           "very strong",
                           "extreme"),
                right = TRUE,
                include.lowest = TRUE)
    as.character(value)
  })
  
  bf_class_css = reactive({
    value = cut(bf_pretty(),
                breaks = c(0,1,3,10,30,100,+Inf),
                labels = c("nil",
                           "anec",
                           "mod",
                           "str",
                           "vstr",
                           "extr"),
                right = TRUE,
                include.lowest = TRUE)
    as.character(value)
  })
  
  bf_desc1 = reactive({
    ifelse(bf_raw() < 1,
           paste("The BF<sub>0:1</sub> quantifies the extent to which the null hypothesis is favoured over the alternative hypothesis."),
           paste("The BF<sub>1:0</sub> quantifies the extent to which the alternative hypothesis is favoured over the null hypothesis."))
  })
  
  bf_desc2 = reactive({
    ifelse(bf_raw() < 1,
           
           paste("In this case, the BF<sub>0:1</sub> of ", bf_pretty(), " indicates that <em>the data are ", bf_pretty(), " times more likely under the null
                 hypothesis</em> than the alternate hypothesis.<br><br>
                 <em>This indicates <span class =\"", bf_class_css(), "\">", bf_class(), " support </span> for the null hypothesis</em> over the alternate hypothesis.",
                 sep = ""),
           
           paste("In this case, the BF<sub>1:0</sub> of ", bf_pretty(), " indicates that <em>the data are ", bf_pretty(), " times more likely under the alternate
           hypothesis</em> than the null hypothesis.<br><br>
                 <em>This indicates <span class =\"", bf_class_css(), "\">", bf_class(), " support </span> for the alternate hypothesis</em> over the null hypothesis.",
                 sep = ""))
  })
  
  ## Calculate prior odds
  prior_odds = reactive({
    input$preprob / (1 - input$preprob)
  })
  
  ## Calculate false positive risk
  fpr = reactive({
    1 / (prior_odds() * bf_raw() + 1)
  })
  
  ## Calculate false negative risk
  fnr = reactive({
    prior_odds() * bf_raw() /
    (prior_odds() * bf_raw() + 1)
  })
  
  ## Rounded outputs of BF, PPV, NPV, FPR, NPR
  output$bayes_factor = renderText({ 
    bf_pretty_full()
  })

  output$bayes_desc1 = renderText({ 
    bf_desc1()
  })
  
  output$bayes_desc2 = renderText({ 
    bf_desc2()
  })
    
  output$fpr = renderUI({
    fpr_v = round(fpr(), 2)
    
    if(fpr_v < thresh_low) {
      return(HTML(paste("<span class=low>", fpr_v*100, "%</span>", sep = "")))
    } else if (fpr_v >= thresh_low && fpr_v <= thresh_high) {
      return(HTML(paste("<span class=med>", fpr_v*100, "%</span>", sep = "")))
    } else if (fpr_v > thresh_high) {
      return(HTML(paste("<span class=high>", fpr_v*100, "%</span>", sep = "")))
    }
  })
  
  output$fnr = renderUI({
    fnr_v = round(fnr(), 2)
    
    if(fnr_v < thresh_low){
      return(HTML(paste("<span class=low>", fnr_v*100, "%</span>", sep = "")))
    } else if (fnr_v >= thresh_low && fnr_v <= thresh_high) {
      return(HTML(paste("<span class=med>", fnr_v*100, "%</span>", sep = "")))
    } else if (fnr_v > thresh_high) {
      return(HTML(paste("<span class=high>", fnr_v*100, "%</span>", sep = "")))
    }
  })
  
  output$ppv = renderText({
    round(
      1 - fpr(), #Calculate positive predictive value
      2)
  })
  
  output$npv = renderText({
    round(
      1 - fnr(), #Calculate negative predictive value
      2)
  })
  
}