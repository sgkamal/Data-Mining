#  examine various 1-d and 2-d tables with the Titanic data

data(Titanic)    # R just knows about these data
dimnames(Titanic) # the variables and their categorizations
print(Titanic)   # R plots all possible tables for each *fixed* value of last two variables (slicing)
print(Titanic["1st","Female",,])   # table of 1st class female by Age and Survial  (slice)
print(apply(Titanic, c("Sex","Survived"),sum));  # table on  Sex and Survived
stopp;
print(apply(Titanic["1st",,,],c("Sex","Survived"),sum))   # table on Sex and Survived for the 1st class


# Interesting Analysis:  Observing 2-way tables of each of "Class","Sex","Age"  with "Survived
# suggests that "Female", "Child", and "1st" all had greater survial rates.
# But note that "Female" is more heavily weighted in the higher classes.
# Could it be that "1st"-class passengers just appear to have been favored because they
# contained more "Female"s?
# To consider this, look at the 2-way "Class"x"Survived table separately for both "Female" and "Male".
# Even doing this it appears that "1st" class was was prefered:  


# What if the table had 20 or 100 variables?  How could we look for interesting 2-d or 1-d tables
# that would gives insight or suggest action?  This is goal of OLAP



