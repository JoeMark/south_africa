# Load the gapminder package
install.packages("gapminder")
library(gapminder)

# Load the dplyr package
library(dplyr)

# Load the ggplot2 package
library(ggplot2)

# Look at the gapminder dataset
gapminder

# Filter the gapminder dataset for the year 2007
gapminder %>%
  filter(year == 2007)

# Filter for BRICS minus Russia
brics <- gapminder %>%
  filter(country %in% c("South Africa", "China", "Brazil", "India")) %>%
  group_by(year, country) %>%
  summarise(avgLifeExp = mean(lifeExp), medianGdpPercap = median(gdpPercap))
brics

# Sort in ascending order of lifeExp for BRICS minus Russia
gapminder %>%
  filter(country %in% c("South Africa", "China", "Brazil", "India")) %>%
  arrange(lifeExp)


# Sort in descending order of lifeExp
gapminder %>%
  arrange(desc(lifeExp))

# FILTER for the year 1957, AND ARRANGE in descending order of population
gapminder %>%
  filter(year == 1957) %>%
  arrange(desc(pop))

# Use mutate to change lifeExp to be in months
gapminder %>%
  mutate(lifeExp = lifeExp * 12)

# Use mutate to create a new column called lifeExpMonths
gapminder %>%
  mutate(lifeExpMonths = lifeExp * 12)

# Filter, mutate, and arrange the gapminder dataset
gapminder %>%
  filter(year == 2007) %>%
  mutate(lifeExpMonths = 12 * lifeExp) %>%
  arrange(desc(lifeExpMonths))

# COMPARE POPULATION AND GDP per capita
# Create gapminder_1952
gapminder_1952 <- gapminder %>% filter(year == 1952)

# Change to put lifeExp on the y-axis and gdpPercap on the x-axis
ggplot(gapminder_1952, aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

# Create a scatter plot with pop on the x-axis and lifeExp on the y-axis
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point()

# Change this plot to put the x-axis on a log scale
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point() +
  scale_x_log10()

# Scatter plot comparing pop and gdpPercap, with both axes on a log scale
ggplot(gapminder_1952, aes(x = pop, y = gdpPercap)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

# Scatter plot comparing pop and lifeExp, with color representing continent
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, colour = continent)) +
  geom_point() +
  scale_x_log10()

# Add the size aesthetic to represent a country's gdpPercap
ggplot(brics, aes(x = year, y = medianGdpPercap, colour = country)) +
  geom_line() 


# Scatter plot comparing pop and lifeExp, faceted by continent
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  scale_x_log10() +
  geom_point() +
  facet_wrap( ~ continent)

# Scatter plot comparing gdpPercap and lifeExp, with color representing continent
# and size representing population, faceted by year
ggplot(brics, aes(x = medianGdpPercap, y = avgLifeExp, colour = country)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap( ~ year)

# Summarize to find the median life expectancy
gapminder %>% summarise(medianLifeExp = median(lifeExp))

# Filter for 1957 then summarize the median life expectancy
gapminder %>% filter(year == 1957) %>% summarise(medianLifeExp = median(lifeExp))

# (Summarising multiple variables in 1957)
# Filter for 1957 then summarize the median life expectancy and the maximum GDP per capita
gapminder %>% filter(year == 1957) %>% summarise(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

# (SUMMARISING BY YEAR) Find median life expectancy and maximum GDP per capita in EACH YEAR
# This calls for group_by() function
gapminder %>% group_by(year) %>% summarise(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

# SUMMARISE BY CONTINENT
# Find median life expectancy and maximum GDP per capita in each continent in 1957
gapminder %>% filter(year == 1957) %>% group_by(continent) %>% summarise(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

# SUMMARISE BY CONTINENT and YEAR
# Find median life expectancy and maximum GDP per capita in each year/continent combination
gapminder %>% group_by(continent, year) %>% summarise(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

# Visualising median life expectancy over time
by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

# Create a scatter plot showing the change in medianLifeExp over time
ggplot(by_year, aes(x = year, y = medianLifeExp)) +
  geom_point() +
  expand_limits(y = 0)

# Visualising median GDP per capita per continent
# per year
# Summarize medianGdpPercap within each continent within each year: by_year_continent
by_year_continent <- gapminder %>%
  group_by(continent, year) %>%
  summarise(medianGdpPercap = median(gdpPercap))

# Plot the change in medianGdpPercap in each continent over time
ggplot(by_year_continent, aes(x = year, y = medianGdpPercap, colour = continent)) +
  geom_point() +
  expand_limits(y = 0)

# Comparing median life expectancy and median GDP per continent in 2007
# Summarize the median GDP and median life expectancy per continent in 2007
by_continent_2007 <- gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(medianLifeExp = median(lifeExp), medianGdpPercap = median(gdpPercap))

# Use a scatter plot to compare the median GDP and median life expectancy
ggplot(by_continent_2007, aes(x = medianGdpPercap, y = medianLifeExp, colour = continent)) +
  geom_point()

# Visualizing median GDP per capita over time
# Summarize the median gdpPercap by year, then save it as by_year
by_year <- gapminder %>%
  group_by(year) %>%
  summarise(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap over time
ggplot(by_year, aes(x = year, y = medianGdpPercap)) +
  geom_line() +
  expand_limits(y = 0)

# Visualizing median GDP per capita by continent over time
# Summarize the median gdpPercap by year & continent, save as by_year_continent
by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarise(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap by continent over time
ggplot(by_year_continent, aes(x = year, medianGdpPercap, colour = continent)) +
  geom_line() +
  expand_limits(y = 0)

