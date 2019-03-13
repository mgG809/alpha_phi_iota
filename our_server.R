library(plotly)
library("shiny")
library("dplyr")
library("tidyr")
library("ggplot2")
library("jsonlite")

our_server <- function(input, output) {

  d <- reactive({
    fast_food_restaurants_df <-
      read.csv(file = 'Datafiniti_Fast_Food_Restaurants.csv')
    weight_data_by_state_df <-
      read.csv(file = 'original_weight_data.csv')
    
    
    fast_food_restaurants_df <- fast_food_restaurants_df %>%
      mutate(Links = gsub(",", "/n", websites))
    
    weight_data_by_state_df <- weight_data_by_state_df %>%
      filter(!is.na(Data_Value)) %>%
      filter(LocationAbbr != "US") %>%
      filter(QuestionID == "Q036")
    obesity_restaurant_state_2016 <-
      function(obesity_data, restaurant_data) {
        obesity_data <- obesity_data %>%
          filter(YearStart == input$year)
        restaurant_data <- restaurant_data %>%
          group_by(province) %>%
          summarise(num_fast_foods = n())
        obesity_data <- obesity_data %>%
          group_by(LocationAbbr) %>%
          summarise(avg_obesity = mean(Data_Value)) %>%
          select(LocationAbbr,
                 avg_obesity)
        names(obesity_data)[1] = "province"
        restaurant_data
        obesity_and_restaurants <-
          inner_join(obesity_data, restaurant_data, "province")
        #%>%
        #   filter(
        #     num_fast_foods > 400 | num_fast_foods < 80
        #   ) %>%
        #   head(8)
        obesity_and_restaurants
      }
    df <-
      obesity_restaurant_state_2016(weight_data_by_state_df, fast_food_restaurants_df)
    df$hover <-
      with(
        df,
        paste(
          province,
          '<br>',
          "average obesity",
          avg_obesity,
          '<br>',
          "num of fast food restaurants",
          num_fast_foods,
          "<br>"
        )
      )
    
    df
  })
  
  #Mariam's code
  
  output$mariam_text <- renderText({
    return(
      "The fast food restaurant data set has a lot of information: names of restaurants, their locations by
      state, the websites. But we have asked outselves a question that helped us summarize the data set in a
      meaningful way. \n The small table below this text shows the minimum, the average and the maximum amount of
      fast food restaurants in each state. This information is useful because we can see how large the range
      of the number of restaurants per state is."
    )
  })
  
  output$mariam_table <- renderTable({
    average_fast_food_per_state <- fast_food_restaurants_df %>%
      group_by(province) %>%
      summarise(num = n()) %>%
      summarise(average = mean(num),
                min = min(num),
                max = max(num))
    return(average_fast_food_per_state)
  })
  
  output$mariam_state_text <- renderText({
    data_for_state <- fast_food_restaurants_df %>%
      filter(province == state.abb[which(state.name == input$mariam_states)]) %>%
      summarise(num = n())
    return(
      paste0(
        input$mariam_states,
        " has ",
        data_for_state$num,
        " fast food restaurants."
      )
    )
  })
  
  
  #Cheryl's code
  
  output$cheryl_text <- renderText({
    return(
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
      similar and higher than the other age groups.
      These age groups are mostly middle-aged adults. According
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
      food concotions.
      Taco Bell has several co-branded items like Doritos Locos Tacos, Starbust Slushies, &
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
  })
  
  output$cheryl_table <- renderTable({
    # most_obese_people_by_age <- function(data_for_obesity) {
    most_obese_age <- obesity_df %>%
      flatten() %>%
      filter(Age.years. != "") %>%
      group_by(Age.years.) %>%
      summarise(average_per_age_gap = mean(Data_Value))
    return(most_obese_age)
    # }
    # most_obese_people_by_age()
  })
  
  output$ux_text <- renderText({
    return(
      "During our process of designing our app, we've encountered some obstacles in making our app be user friendly
      as accessible as it can be. We were also concerned about the accuracy of our data sets and we wanted to adddress
      this to our readers in order to inform them.
      First off, with the designing, we tried to make our widgets as simple and easy as we could. For starters, we
      thought about having the user input the state name themselves, but we realized that not everyone may know all
      of the states and/or how to spell them correctly. In order to reduce search time and fustration, we decided to
      provide all of the states in a dropdown. We also decided to have our questions static in the page as the user
      changes the tabs. At first, we were going to have our questions in a single tab but that would be a little annoying
      switching back and forth if the user wanted to compare the question and the visualization with each other. By having
      the questions nearby, users don't need to go through the trouble between switching and slightly memorizing in order
      to compare the questions and the visuals.
      Going to the accuracy of our data, we fear that there may be a possibility of having a participation bias. For the
      fast food data set, there only wrote down 10,000 resturants across the United States. However, according to Google Maps,
      there's about 50,000 fast food resturants in the U.S. The number of fast food resturants in each state may not be accurate
      with the other states since we don't know how our data gathered its data and how the data was distributed inside of itself.
      They may also be some bias in the obesity data set since most people didn't put their age group and race when they were surveyed
      and there were other NAs in the data set. This is something to keep in mind as we discuss our questions and its answers since
      there may be some factors affecting the validity of our analysis and the data set.
      Overall, we wanted to make sure that our users are informed of the concerning thoughts that we had to better our users."
    )
  })
  
  
  #Peyton's text
  
  output$range_difference_mean_analysis <- renderText({
    return(
      "Looking at this table, there are some arbitrary things going on in the inner workings of
      the numerical data. Within the original data set, there are many surveys that took place for the same location. The
      Difference of means column takes into conideration all surveys for the state in mention and takes the average of those values. That is how we
      are getting a mean in this case. If a value is negative that implies that in the year range that the user has chosen with the slider, the
      percentage of the population that partook in those surveys had gone down. Why would a number that is presumably going up constantly, switch sides?
      These are questions that the data set alone cannot answer.
      
      It is a combination of participations bias (only people who chose to take the survey are included in this data set. we
      may not have the whole picture.) and other societal trends that were going on at the time of the year range. Because this data table relies on
      a mean for its output, population is another thing to consider aomongst the outcomes. For example, in the data table provided by world atlas we can see that California had a population of
      39,536,653 in 2018 while Wyoming had a population of 579,315. This means that the mean in Wyoming can be swung positively or negatively by a relatively small group of
      people and their answers to the surveys in a particular year while in California a shift like that would take quite a large number of people. In an example of participation bias,
      if you look at Kentucky's (KY) Difference of means between 2015 and 2016, you will see that the difference is -0.01 percent. Kentucky is ranked as the 26th most populated state (worldatlas.com), and it experienced relatively no change.
      The small change it did experience was most likely caused by the lack of obese participants and not actually a difference in their obese population."
    )
  })
  
  output$difference_table <- renderTable({
    data_for_obesity <- obesity_df %>%
      filter(LocationAbbr != "US") %>%
      filter(YearStart == input$slide_key[1] |
               YearStart == input$slide_key[2])
    grouping <- data_for_obesity %>%
      group_by(LocationAbbr,
               YearStart) %>%
      summarise(mean_state_year = mean(Data_Value))
    data_2011 <- grouping %>%
      filter(YearStart == input$slide_key[1])
    data_2016 <- grouping %>%
      filter(YearStart == input$slide_key[2])
    difference_data <- left_join(data_2016, data_2011, "LocationAbbr")
    difference_data <- difference_data %>%
      flatten() %>%
      summarise(difference_mean_2011_2016 = mean_state_year.x - mean_state_year.y)
    names(difference_data) <-
      c(
        "Location",
        paste0(
          "Difference of means between ",
          input$slide_key[1],
          " and ",
          input$slide_key[2]
        )
      )
    return(difference_data)
  })
  
  
  
  ## Riley's code
  
  output$riley_map <- renderPlotly({
    df <- d()
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    p <- plot_geo(df, locationmode = 'USA-states') %>%
      add_trace(
        z = ~ num_fast_foods,
        text = ~ hover,
        locations = ~ province,
        color = ~ num_fast_foods,
        colors = 'Purples'
      ) %>%
      colorbar(title = "Number of Fast Food Restaurants") %>%
      layout(title = 'The relationship between average obesity and number of fast food restaurants in states)',
             geo = g)
    
    # # Create a shareable link to your chart
    # # Set up API credentials: https://plot.ly/r/getting-started
    # chart_link = api_create(p, filename="choropleth-ag")
    # chart_link
    p
  })
  
  output$riley_text <- renderText({
    df <- d()
    paste0(
      "What is the relationship between the average obesity in states and the
      number of fast food restaurants in states? Does the average obesity goes up if there are more
      fast food restaurants in the state? The comparison between two data sets and the graph can
      help us better answer this question." ,
      "The U.S map above shows all 51 states with different
      colors on each state, which deeper color represents larger number of fast food
      restaurants in this state. If you hover the mouse on a certain state, you can
      get the specifc number of fast food restaurants for the specific state and also the
      average obesity for this particular state. For example, California has the deepest color
      among all states. It has ",
      filter(df, province == "CA")$num_fast_foods,
      " fast food restaurants and
      the average obesity in ",
      input$year,
      " is ",
      filter(df, province == "CA")$avg_obesity,
      "
      Based on our findings, there are no direct relationship between the number of fast food
      restaurants in a state and the average obesity in a state. Because even though California has the
      most number of fast food restaurants among all states, its average obesity is not the
      highest and is similar to some of the states with much fewer fast food restaurants
      such as Mountana"
    )
    
    
  })
  
  
  
  
  }