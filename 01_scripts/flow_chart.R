# flow_chart.R
# create a flow chart for the analysis and screening process including pilot analysis
# January 2025

# load in the DiagrammeR package
library(DiagrammeR)


# create the graph
grViz("digraph {
  graph[layout = dot, 
        rankdir = TB,
        overlap = true,
        fontsize = 10]
  node [shape = rectangle,
  fixedsize = true,
  width = 4]
  
  'Develop statistical code/methods (EuroSCORE)'
  'Test statistical code/methods (Framingham)'
  'Complete search'
  'Screen articles'
  'Data collection'
  'Pilot analysis on neurology'
  'Potential changes to protocol'
  'Complete final analysis'
  
  'Develop statistical code/methods (EuroSCORE)' -> 'Test statistical code/methods (Framingham)' ->
  'Complete search' ->'Screen articles' ->'Data collection' ->'Pilot analysis on neurology' ->'Potential changes to protocol' ->
  'Complete final analysis'

      }")

