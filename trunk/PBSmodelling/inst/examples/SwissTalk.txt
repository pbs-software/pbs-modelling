# The function "presentTalk" in this version has a number of bugs, including
# - a comment sometimes sometimes requires "#" in column 1
# - a final unused segment must appear at the end of the description file
# - a single file can have only one <talk>

<talk name="Swiss" button=FALSE>

# SECTION 1. The "summary" method
<section name="Methods" button=TRUE>

# State the talk's purpose in text
<text> 
This short talk examines the "summary" method
and applies it to the "swiss" dataset.
The talk itself comes from a talk description file ...

# Show the description file
<file name="swissTalk" button=TRUE>
  swissTalk.txt

# Discuss "summary"
<text break=F>
"summary" is a function (class function):
<code break=print>
isWhat(summary) # isWhat() from PBSmodelling
<text break=F> "summary" is generic:
<code break=print> summary
<text break=F> "summary" has many methods:
<code break=print> methods(summary)

# SECTION 2. The "swiss" data
<section name="Data" button=TRUE>
<text break=F> "swiss" is a data frame (class data.frame):
<code> isWhat(swiss)
<text break=F> You can read about the data here:
<code> help(swiss) # open the help file
<text break=F> Apply "summary" to Swiss:
<code break=print> summary(swiss)
<text break=F> Print the first 3 records:
<code break=print> head(swiss,3)
<text break=F> Display the data with the "plot" method . . .
<code print=F> plot(swiss,gap=0)
<text> THE END .. THANKS FOR WATCHING!

# Due to a current bug, the file needs a final unused block
<text> ---need this as a throw away---
