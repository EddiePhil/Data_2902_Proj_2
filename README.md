This dashboard is an interactive shiny app made to help with visualisation and understanding of select variables from the DATA2902 class survey. It allows for data exploration, 
visualisation and statistical analysis to be easily performed.

There are 3 tabs - data, plots and tests. The data tab allows the user to see the (cleaned) data in a nice layout. Note, not all of the variables here were used in the app
The plots tab allows you to create visualisations. The user can choose a histogram (single numerical variable), box (single numerical, with optional categorical variable for side-by-side 
comparison) or scatter plot (two numerical variables, with an option to colour by a categorical and size by a numerical).

Finally, the tests tab allows the user to conduct either a t-test, where they can also select if they'd like a regular student's t test, welch t test or permutation t test, or a chi-square
test for independence. For both tests, they pick variables, and the the output will be as follows: Hypotheses (null and alternate), assumption testing (ie normality), and results, including
whether to accept or reject the null. A graph will also appear - a side-by-side box plot for the t-test, and a double bar chart for the chi-suqared test.
Users may also decide on the significance level the tests should base on.
