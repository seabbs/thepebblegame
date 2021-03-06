
The Pebble Game
===============

[The Pebble Game](http://seabbs.co.uk/shiny/thepebblegame) ([alternative link](https://seabbs.shinyapps.io/thepebblegame/)) is a shiny application that simulates the pebble game. This is a simple game that has been developed by [BIDD](http://www.bristol.ac.uk/social-community-medicine/research/groups/bidd/) at the University of Bristol, to help a general audience understand the role of vaccination in preventing the onward transmission of infectious disease. It involves repeatedly drawing pebbles from a bag, which contains two distinct pebble types (to represent vaccinated and unvaccinated individuals). The number of pebbles that are drawn each round is dependent on the number of infect-able (i.e unvaccinated) cases drawn in the previous round, and the infectiousness of the simulated disease. For a more detailed explanation of the mechanics of the game, examples of using the app, suggested questions, and instructions for local installation continue reading. Otherwise enjoy exploring a simple system with complex dynamics!

Using the app
-------------

Whilst the Pebble Game has been designed to be user friendly, I've written a [blog post](https://www.samabbott.co.uk/post/using-the-pebble-game/) that takes you through using the app in more detail. If in using the app you discover something that you think is worth sharing then please contact me either via [twitter](https://twitter.com/seabbs) or [email](https://www.samabbott.co.uk/#contact).

Some suggested questions
------------------------

-   Look up some real world disease reproduction numbers (the number that each diseased pebble infects). What effect does varying this have?
-   For a given reproduction number what effect does varying the proportion of vaccinated pebbles have?
-   What happens if you repeat the game using the same parameters multiple times? Can you explain this?
-   How does population size effect the spread of the disease?

Playing the game for yourself
-----------------------------

Whilst the web app fully captures the dynamics of the pebble game it may be useful to physically play the game for yourself. To do that follow the instructions below:

### What you will need

-   A bag or box (it is important that the pebbles cannot be seen).
-   A set of pebbles/counters etc. of 2 different colours (these represent those who are vaccinated versus unvaccinated).
-   Some paper to note down your results.

### Set up

To play the game you must pick some basic parameters that define your pebble population and the disease. These are:

-   The total number of pebbles (for best results it is suggested to use at least 50). The pebble population should be made up of;
    -   A number of vaccinated pebbles (choose a colour to represent this)
    -   A number of unvaccinated pebbles (using your other colour)
-   The number of pebbles in the first generation of the disease (for the simplest game pick 1)
-   The number of pebbles that each diseased pebble subsequently infects (in epidemiology this is known as the basic reproduction number).

### Playing the game

1.  Pick pebbles from the bag equal to the number in the first generation times the number that each diseased pebble infects.
2.  Count the number of pebbles that are unvaccinated that you have picked, these are the next generation of infected pebbles (note this number down).
3.  For the new generation multiply the number in the new generation against the number that each diseased pebble infects and pick this many pebbles from the bag.
4.  Repeat steps 2. and 3. until you pick no unvaccinated pebbles or until the bag is empty.

Installing the shiny app locally
--------------------------------

To install and run the shiny app locally on your own computer you will need to first install [R](https://www.r-project.org/), it is also suggested that you install [Rstudio](https://www.rstudio.com/products/rstudio/download/). After downloading the source code from [this repository](https://www.github.com/seabbs/thepebblegame) click on the `thepebblegame.Rprof` file, this will open an Rstudio window. Type the following code into the command line;

``` r
install.packages("shiny")
install.packages("shinydashboard")
install.packages("shinyBS")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tibble")
install.packages("purrr")
install.packages("ggplot2")
install.packages("rmarkdown")
```

To run the app open the `ui.R` file and press run, depending on your computer this may take some time. Enjoy playing the pebble game!
