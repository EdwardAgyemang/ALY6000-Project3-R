# Name: Edward Agyemang | Date: 2024 | Class: ALY 6000
# ============================================================
# ALY 6000 - Project 3
# ============================================================

rm(list = ls())
setwd("C:/Users/elitebook folio/Documents/EDWARD PROJECTS/ALY6000/Project 3")

library(pacman)
p_load(tidyverse, janitor, lubridate)

# ============================================================
# PROBLEM 1 - Load books.csv
# ============================================================

# read_csv loads the file as a tibble
books <- read_csv("books.csv")
head(books)
glimpse(books)

# ============================================================
# CLEANING 1 - Standardize column names with janitor
# ============================================================

# clean_names() converts "First Publish Date" to "first_publish_date"
books <- clean_names(books)
names(books)

# ============================================================
# CLEANING 2 - Convert first_publish_date handling multiple formats
# ============================================================

# parse_date_time() tries multiple date formats automatically
# "mdy" = Month Day Year (January 1 2005)
# "ymd" = Year Month Day (2005-01-01)
# "dmy" = Day Month Year (1 January 2005)
books <- books %>%
  mutate(first_publish_date = parse_date_time(
    first_publish_date,
    orders = c("mdy", "ymd", "dmy")
  ))

head(books$first_publish_date)

# ============================================================
# CLEANING 3 - Extract year from first_publish_date into new column
# ============================================================

# year() from lubridate pulls just the year number from a date
books <- books %>%
  mutate(year = year(first_publish_date))
head(books$year)

# ============================================================
# CLEANING 4 - Keep only books published 1990 to 2020
# ============================================================

# filter() with >= and <= keeps rows within the range (inclusive)
books <- books %>%
  filter(year >= 1990 & year <= 2020)
nrow(books)

# ============================================================
# CLEANING 5 - Remove 7 unnecessary columns
# ============================================================

# select() with minus sign removes those specific columns
books <- books %>%
  select(-publish_date, -edition, -characters,
         -price, -genres, -setting, -isbn)
names(books)

# ============================================================
# CLEANING 6 - Keep only books with fewer than 700 pages
# ============================================================

# filter() keeps only books where pages is less than 700
books <- books %>%
  filter(pages < 700)
nrow(books)

# ============================================================
# CLEANING 7 - Remove any rows that contain NA (missing values)
# ============================================================

# drop_na() removes any row that has at least one missing value
books <- books %>%
  drop_na()
nrow(books)   # should show 8490

# ============================================================
# PROBLEM 8 - Glimpse the cleaned dataset
# ============================================================

glimpse(books)

# ============================================================
# PROBLEM 9 - Summary statistics of the cleaned dataset
# ============================================================

summary(books)

# ============================================================
# PROBLEM 10 - Histogram of book ratings
# ============================================================

ggplot(books, aes(x = rating)) +
  geom_histogram(fill = "red", color = "darkred", binwidth = 0.25) +
  labs(title = "Histogram of Book Ratings",
       x = "Rating", y = "Number of Books") +
  theme_classic() +
  theme(
    plot.background  = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white"),
    plot.title       = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 11, color = "black")
  )
ggsave("rating_histogram.png", width = 8, height = 6, dpi = 300, bg = "white")


# ============================================================
# PROBLEM 11 - Horizontal boxplot of page counts
# ============================================================

ggplot(books, aes(x = pages)) +
  geom_boxplot(fill = "red", color = "darkred") +
  labs(title = "Box Plot of Page Counts", x = "Pages") +
  theme_classic() +
  theme(
    plot.background  = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white"),
    plot.title       = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 11, color = "black")
  )
ggsave("pages_boxplot.png", width = 8, height = 6, dpi = 300, bg = "white")

# ============================================================
# PROBLEM 12 - Count books per year
# ============================================================

# group_by(year) groups data by year
# summarise() counts books in each year group
by_year <- books %>%
  group_by(year) %>%
  summarise(total_books = n())
by_year

# ============================================================
# PROBLEM 13 - Line plot with points of books per year
# ============================================================

ggplot(by_year, aes(x = year, y = total_books)) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(color = "black", size = 2) +
  labs(title = "Total Number of Books Rated Per Year",
       x = "Year", y = "Total Books") +
  theme_classic() +
  theme(
    plot.background  = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white"),
    plot.title       = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 11, color = "black")
  )
ggsave("books_per_year.png", width = 8, height = 6, dpi = 300, bg = "white")

# ============================================================
# PROBLEM 14 - Count books per publisher
# ============================================================

# group_by(publisher) then count rows per publisher
book_publisher <- books %>%
  group_by(publisher) %>%
  summarise(book_count = n())
book_publisher

# ============================================================
# PROBLEM 15 - Keep only publishers with 125 or more books
# ============================================================

# filter() removes publishers with fewer than 125 books
book_publisher <- book_publisher %>%
  filter(book_count >= 125)
nrow(book_publisher)

# ============================================================
# PROBLEM 16 - Sort publishers by book count highest first
# ============================================================

book_publisher <- book_publisher %>%
  arrange(desc(book_count))
book_publisher

# ============================================================
# PROBLEM 17 - Add cumulative sum column
# ============================================================

# cumsum() adds a running total of book_count
book_publisher <- book_publisher %>%
  mutate(cum_counts = cumsum(book_count))
book_publisher

# ============================================================
# PROBLEM 18 - Add relative frequency column
# ============================================================

# rel_freq = each publisher's count divided by total books
# This shows what PROPORTION of all books each publisher has
book_publisher <- book_publisher %>%
  mutate(rel_freq = book_count / sum(book_count))
book_publisher

# ============================================================
# PROBLEM 19 - Add cumulative frequency column
# ============================================================

# cum_freq = running total of relative frequencies
# When complete, the last row should equal 1.0 (100%)
book_publisher <- book_publisher %>%
  mutate(cum_freq = cumsum(rel_freq))
book_publisher

# ============================================================
# PROBLEM 20 - Convert publisher column to a factor
# ============================================================

# factor() locks the order of publisher names
# This ensures the Pareto chart bars appear in the RIGHT order
book_publisher <- book_publisher %>%
  mutate(publisher = factor(publisher, levels = publisher))
book_publisher

# ============================================================
# PROBLEM 21 - Pareto Chart with cumulative ogive line
# ============================================================

# This is the most complex chart - it has bars AND a line on the same plot
# sec_axis creates a second y-axis on the right for cumulative counts
ggplot(book_publisher, aes(x = publisher)) +
  geom_bar(aes(y = book_count),
           stat = "identity", fill = "cyan", color = "black") +
  geom_line(aes(y = cum_counts, group = 1),
            color = "black", linewidth = 1) +
  geom_point(aes(y = cum_counts),
             color = "black", size = 2.5) +
  labs(title = "Book Counts (1990 - 2020)",
       x = "Publisher", y = "Number of Books") +
  theme_classic() +
  theme(
    plot.background  = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white"),
    plot.title       = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 10, color = "black"),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )
ggsave("pareto_chart.png", width = 10, height = 7, dpi = 300, bg = "white")

# ============================================================
# PROBLEM 22 - Additional visualization: Average Rating by Year
# ============================================================

# Calculate average rating per year
ggplot(avg_rating_by_year, aes(x = year, y = avg_rating)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkblue", size = 2.5) +
  labs(title = "Average Book Rating by Year (1990 - 2020)",
       x = "Year", y = "Average Rating") +
  theme_classic() +
  theme(
    plot.background  = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white"),
    plot.title       = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 11, color = "black")
  )
ggsave("avg_rating_by_year.png", width = 8, height = 6, dpi = 300, bg = "white")
