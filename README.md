This folder is the new app I created to test the qaqc. It is based on the CHRL hydromet page Alex created.
Careful: DO NOT PUSH THIS TO GITHUB AS CHRL-GRAPH! Otherwise, this will kill the whole webpage!

The changes to the app can simply be tested by opening R studio and running the app. Then, if wanting to make those
changes visible to more than just your local machine, you need to push the app to a public webpage via shinyapp.io.

I have an account at shinyapp.io and token access. In R Studio, just click on the blue circle button next to the Run button
(top right) to connect R Studio with Shinyapp. You will need to enter two commands to download the libraries
 etc (see ShinyApp for comprehensive help). Then, you press the blue button again and press 'public' which will
initiate R studio to publish it. The app should be: https://qaqc-miniapp.shinyapps.io/chrl-graph/