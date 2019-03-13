library("shiny")
library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")

our_ui <- fluidPage(
  # Content on top of the app
  titlePanel(title = "Fast Food Restaurants and Obesity App"),
  p(
    em("Authors:"),
    "Riley Lin, Mariam Gadzhimagomedova, Peyton Blackmer, Cheryl Wu"
  ),
  p(em("03/13/19")),
  textOutput(outputId = "introduction"),
  
  # Our questions and interactive data
  tabsetPanel(
    type = "tabs",
    
    #documentation tab
    
    tabPanel(title = "Documentation",
             h3("Fast Food Restaurants and Obesity App"),
             br(),
             h4("Summary"),
             br(),
             p("We will be using 2 different data sets: fast food restaurants in different states and questionnaire results about obesity and nutrition.
The fast food restaurants data set lists 10,000 restaurants all across the United States. 
The obesity set has statistics on weight of different age groups in the United States. 
We will combine and compare these two data sets and ask critical questions about the number of fast food restaurants and the obesity percentage in different states. 
"),
             br(),
             h4("Guiding Questions"),
             br(),
             p("What was the difference in obesity percentage from 2011 - 2016 by state?
               "),
             br(),
            p("How many fast food restaurants on average do states have? (To see if it’s an even distribution)"),
             br(),
            p("What is the relationship between the number of fast food restaurants and the obesity in a certain area?"),
            br(),
            p("Are fast food restaurants targeting people in the age group of 18-24 more 
              than 65 and older? (Find the average percent of obese people in those age groups and compare)"),
            br(),
             
             h4("References"),
             p("Definition of Fast Foods https://www.livestrong.com/article/49366-definition-fast-foods/

"),
             br(),
             p("Definition of Obesity https://www.cdc.gov/obesity/adult/defining.html

               "),
             br(),
             p("Definition of Obesity https://stanfordhealthcare.org/medical-conditions/healthy-living/obesity.html

               "),
             br(),
             p("Obesity Data Frame https://www.kaggle.com/adu47249/obesity-stats/home"),
             br(),
             p("Fast Food Restaurants Data Frame https://www.kaggle.com/datafiniti/fast-food-restaurants/home"),
             br(),
             p("Tennessee Health News https://bettertennessee.com/smoking-fast-food-babies-sleep-habits-affect-overall-health-in-tennessee/"),
             br(),
            p("Fast Food Targets:  http://www.fastfoodmarketing.org/media/fastfoodfacts_targetedmarketing.pdf"),
            br(),
            p("American Council of Science and Health: https://www.acsh.org/news/2018/10/03/1-3-adults-has-fast-food-any-given-day-13471"),
            br(),
            p("Huffington Post Adults Can’t Cook: https://www.huffingtonpost.com/2011/09/09/cooking-survey_n_955600.html"),
            br(),
            p("Fast Food Marketing Strategies of 2018: https://www.shopkick.com/partners/blog/the-top-3-fast-food-marketing-strategies-for-2018/"),
            br(),
            p("World Atlas : https://www.worldatlas.com/articles/us-states-by-population.html"),
            br(),
            p("Taco Bell Starburst: https://www.eater.com/2014/10/10/6956623/taco-bell-strawberry-starburst-flavored-slurpee"),
            br(),
            p("Taco Bell Cap’n Crunch: https://www.eater.com/2015/2/27/8120467/taco-bell-capn-crunch-delights-doughnuts-donuts-cereal-deep-fried"),
            br()
            
             ),
    
    
    
    
    
    # Peyton's Q: text analysis, slider for years, interactive table
    tabPanel(title = "Changes in Obessed Population",
             
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   inputId = "slide_key",
                   label = "Select years to show data from:",
                   step = 1,
                   min = 2011,
                   max = 2016,
                   value = c(2011:2016),
                   sep = ""
                 ),
                 tableOutput(outputId = "difference_table")
               ),
               mainPanel(
                 #textOutput(outputId = "range_difference_mean_analysis")
                 p(
                   "Looking at this table, there are some arbitrary things going on in the inner workings of
                   the numerical data. Within the original data set, there are many surveys that took place for the same location. The
                   Difference of means column takes into conideration all surveys for the state in mention and takes the average of those values. That is how we
                   are getting a mean in this case. If a value is negative that implies that in the year range that the user has chosen with the slider, the
                   percentage of the population that partook in those surveys had gone down. Why would a number that is presumably going up constantly, switch sides?
                   These are questions that the data set alone cannot answer. "
                 ),
                 br(),
                 p(
                   "It is a combination of participations bias (only people who chose to take the survey are included in this data set. we
                   may not have the whole picture.) and other societal trends that were going on at the time of the year range. Because this data table relies on
                   a mean for its output, population is another thing to consider aomongst the outcomes. For example, in the data table provided by world atlas we can see that California had a population of
                   39,536,653 in 2018 while Wyoming had a population of 579,315. This means that the mean in Wyoming can be swung positively or negatively by a relatively small group of
                   people and their answers to the surveys in a particular year while in California a shift like that would take quite a large number of people. In an example of participation bias,
                   if you look at Kentucky's (KY) Difference of means between 2015 and 2016, you will see that the difference is -0.01 percent. Kentucky is ranked as the 26th most populated state (worldatlas.com), and it experienced relatively no change.
                   The small change it did experience was most likely caused by the lack of obese participants and not actually a difference in their obese population."
                 )
                 
                 )
                 ))
    ,
    
    # Mariam's Q: text analysis, and drop down menu, table, interactive summary
    tabPanel(
      title = "Restaurants by states",
      sidebarLayout(sidebarPanel(tableOutput(outputId = "mariam_table"),
                                 selectInput(
                                   inputId = "mariam_states",
                                   label = "Select the state for data:",
                                   choices = unique(obesity_df$LocationDesc)
                                 )),
                    
                    mainPanel(h5(textOutput(outputId = "mariam_text")),
                              h4(textOutput(outputId = "mariam_state_text")))
    )
    ),
    
    # Cheryl's Q: text analysis, static table
    tabPanel(
      title = "(Fast) Food For Thoughts",
      tableOutput(outputId = "cheryl_table"),
      #verbatimTextOutput(outputId = "cheryl_text")
      p(
        "According to this data table, a lot of questions rises up as we examine the different
        averages for each age group. After doing some research to possibly answer why these averages
        came to be, we start to paint a picture of the situation. We can see a significant rise as the
        ages get older and a small decrease in as we get to 65 or older. There's a significant different
        of 18-24. This may be the fact that more people are advocating for teenagers and young adults to
        eat healthier. You might have seen an increase of young adults becoming vegan/vegetarians and are
        becoming more aware about what might be in their food. It seems popular to eat healthier and to be
        conscious about what's in their food. 25-34 is bigger than 18-24. The reason might be due to the fact
        that these ages are when people are starting to work and have a family. With little time to cook or
        to do some research on their food, it's reasonable for them to quickly pick up fast food in order to
        satisfy themselves. For age groups 35-44, 45-54, and 55-64, we start to question why these averages are
        similar and higher than the other age groups."
      ),
      br(),
      p(
        "These age groups are mostly middle-aged adults. According
        to the American Council of Science and Health, it's found that 1 in 3 adults has fast food on any given
        day. They even provided a bar chart of the percentage of adults who comsumed fast food on a given day by
        age groups. What's haunts us is that their graph's distribution is strikingly similar to our table's distribution.
        Mostly the middle-aged people were eating more fast food and have obesity. Along with work, family, and
        other events getting in the way of these people's lives, another reason may be because of cooking. Or
        the lack of cooking. Huffington Post stated that 28%, which is almost a third, of American adults don't
        know how to cook. The top reason being that their spouse does most of the cooking and the more notable
        reasons (related to our topic) are not having enough time, don't want to clean up afterwards, and the
        trouble of going to a grocery store. That percent is about the same percentage of of Americans eating
        fast food and it's about the same number for our age groups too. Therefore, there may be some kind of
        interesting correlation. We can understand the convience of fast food. It's cheap and quick. With new
        fast food marketing strategies like mobile ordering and mobile app rewards, it's hard for people to
        not get away from fast food. these resturants are also doing some strange gimicks and co-branding in
        order to appeal to their audiences. For example, many fast food resturants are making some intersting
        food concotions."
      ),
      br(),
      p(
        "Taco Bell has several co-branded items like Doritos Locos Tacos, Starbust Slushies, &
        Cap'n Crunch Delights. Carl's Jr also have a Froot Loops mini Donuts collab. These brights colors and
        unexpected collabs sound appealing to eaters. We also may think that this is especially appealing to
        adults since these products may bring in a nostalgic factor. These fast food brands along with their
        collaborators have been here for a long time. These brands can be deemed as chilhood brands for our
        middle-aged adults. With two brands collaborating for a single product, it's very appealing for everyone!
        Overall, these are some factors/reasons for why the middle-aged group is being targeted over others or it
        may not be fast foods that are targetting these age groups, but some personal reasons may be the reason why
        obesity and more fast food eating is apparent for these groups. It's certainly something to think about
        and (fast) food for thought."
      )
      
      ),
    
    # Riley's Q: text analysis, interactive map
    tabPanel(title = "Average obesity and fast food restaurants",
             sidebarLayout(
               # Sidebar panel for inputs ----
               sidebarPanel(
                 # Input: Slider for the number of observations to generate ----
                 sliderInput(
                   "year",
                   "Select year of observation for average obesity:",
                   value = 2011,
                   min = 2011,
                   max = 2016
                 )
                 
               ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 #Output: text describing the data and information represented by the graph.
                 
                 plotlyOutput(outputId = "riley_map"),
                 textOutput(outputId = "riley_text")
                 
               )
             ))
    ,
    tabPanel(title = "Behind the Information",
             br(),
             p("During our process of designing our app, we've encountered some obstacles in making our app be user friendly
      as accessible as it can be. We were also concerned about the accuracy of our data sets and we wanted to adddress
      this to our readers in order to inform them."),
             br(),
      p("First off, with the designing, we tried to make our widgets as simple and easy as we could. For starters, we
      thought about having the user input the state name themselves, but we realized that not everyone may know all
      of the states and/or how to spell them correctly. We also made sure that the appearance of the app looked easy
        enough to digest for the reader. We did this by foucusing on the spacing of text and figured out the placement of text
        and visuals."),
      br(),
      p("Going to the accuracy of our data, we fear that there may be a possibility of having a participation bias. For the
      fast food data set, there only wrote down 10,000 resturants across the United States. However, according to Google Maps,
      there's about 50,000 fast food resturants in the U.S. The number of fast food resturants in each state may not be accurate
      with the other states since we don't know how our data gathered its data and how the data was distributed inside of itself.
      They may also be some bias in the obesity data set since most people didn't put their age group and race when they were surveyed
      and there were other NAs in the data set. This is something to keep in mind as we discuss our questions and its answers since
      there may be some factors affecting the validity of our analysis and the data set.
      Overall, we wanted to make sure that our users are informed of the concerning thoughts that we had to better our users."
      ))

  
  # # References
  # h3("References"),
  # textOutput(outputId = "references")
  
      )
)
