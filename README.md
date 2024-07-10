Most Probable Next Word(s) Predictor

Welcome to the Most Probable Next Word(s) Predictor app! This application predicts the most probable next words based on input of sequences of words(n-grams). Below is a brief overview of how to use this app effectively:

How to Use:
Enter Ngram: In the sidebar, enter an n-gram (e.g., "takes two" or "happy mother day to") in the text box provided. This defines the context from which the app predicts the next word(s).

Click to Render: Click the "Click to Render" button to generate predictions based on your input.

Probability Table: The app displays a table showing up to the top 10 most probable next words sorted by decreasing probability.

Plot: Additionally, a bar plot is generated to visualize the probabilities of these next words.

This app was trained on a curated corpus sourced from Blogs, Twitter, and News sources totaling approximately 55.62 MB. For input up to five words (n=5), it predicts the next most probable words. If a word can't be predicted using a five-word context, it performs a line search using contexts of decreasing length (n=4, 3, ..., 1) until the best result is found. If no suitable prediction is found up to n=1, it defaults to the most frequent words in the training set. For inputs longer than five words (n >= 5), it considers only the last five words for prediction.
Documentation and Resources:

Documentation Tab: For further details and instructions, refer to the documentation tab.

Pitch Presentation: View our Pitch Presentation for an overview of the project.

