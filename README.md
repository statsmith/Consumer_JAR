# Consumer Just About Right (JAR) Analysis
The objective of this web app is to provide basic analysis and recommendations based on consumer responses to JAR diagnostics.

# Background
In the course of developing consumer preferred products, researchers often ask consumers to rate product attributes on "just about right" scales.  Usually JAR diagnostics are 5-point scales where 1 = Far Too Little, 2 = Too Little, 3 = Just About Right, 4 = Too Much, and 5 = Far Too Much.  There are many variations on this scale, but they are functionally equivalent, providing product development teams valuable feedback on how to improve their products.  JAR diagnostics are even more powerful when linked to consumer liking in the form of "penalty analysis".  

# Execute / Run Code
library(shiny) <br>
runGitHub("Consumer_JAR", "statsmith")

Or simply go to https://haveaniceday.shinyapps.io/JAR_Diagnostics/

# Directions
The web app loads with random data for demo purposes.
* To use your data, save it in a tidy csv or xlsx format to your system
  * Rows = observations
  * Columns = variables
  * Note: web app will autoload JAR diagnostics named as MyAttribute.JAR
* Click the links on the right side of the web app to open and close user options
* Click the "Use My Data" link (right side of web app) and click the "Choose File" button.  Browse to find and select your data.
* Click the "Column Inputs" link to open column input options
  * Select column in your data set that ID's samples
  * Choose the sample you wish to analyze
  * Select the appropriate hedonic (liking) column
  * Select columns corresponding to your JAR attributes
* Click the "Value Labels" link if you need to change the defaults (1,2 = Too Little, 3 = JAR, 4,5 = Too Much)
* Click the "Graph Options" link to modify the appearance of the plot and/or display penalty analysis
* Click the links below the plot for additional output options, including recommendation for improving your product

# Interpretation
* The green bars represent % JAR scores.  If they extend beyond the left of the dotted line, they exceed your benchmark.
* The orange bars (too little) and purple bars (too much) are aligned at zero and extend in opposite directions to make it easy to see directional guidance
* Penalty = difference in liking score for participants who indicated too little (or too much) vs liking score for participants who marked JAR
* Weighted Penalty = penalty * proportion of respondents marking too little (or too much)
* Recommendations (below the plot) algorithmically provide direct suggestions for improvement

