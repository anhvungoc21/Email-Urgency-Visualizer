## Email Urgency Visualizer

A program which analyzes and create a visualization of the urgency level of an email

### Description

The program takes in a text file of an email (or any other text), count the number
of "urgent" words (provided in urgent-words.txt), count the number of sentences,
then calculate the ratio of urgent words to sentences. The ratio then determines the level of 
urgency (100% urgency corresponds with 2 urgent words per sentence). An image of a "urgency battery"
is finally created to visualize the urgency level.

### Dependencies

* Program must be downloaded and stored along with the urgent-words.txt file.
* Program should be run on platforms which support the Racket language (e.g. DrRacket) and the libraries mentioned at the top of the .rkt file must be installed

### Executing program

```
(urgency-level name_of_text_file)
