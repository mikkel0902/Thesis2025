import pandas as pd
import re
import multiprocessing
from collections import Counter

# Step 1: Load the Loughran-McDonald Sentiment Dictionary
def load_sentiment_dictionary(file_path):
    df = pd.read_csv(file_path)
    sentiment_dict = {}
    for _, row in df.iterrows():
        word = row['Word'].lower()
        sentiment_dict[word] = {
            "Negative": row['Negative'],
            "Positive": row['Positive'],
            "Uncertainty": row['Uncertainty'],
            "Litigious": row['Litigious'],
            "Constraining": row['Constraining']
        }
    return sentiment_dict

# Step 2: Preprocess Text
def preprocess_text(text):
    text = text.lower()
    text = re.sub(r'[^a-z\s]', '', text)  # Remove punctuation
    words = text.split()
    return words

# Step 3: Compute Sentiment Scores in Parallel
def compute_sentiment(text, sentiment_dict):
    words = preprocess_text(text)
    word_counts = Counter(words)
    
    sentiment_scores = {"Negative": 0, "Positive": 0, "Uncertainty": 0, "Litigious": 0, "Constraining": 0}
    total_words = len(words)
    
    for word, count in word_counts.items():
        if word in sentiment_dict:
            for category in sentiment_scores:
                sentiment_scores[category] += sentiment_dict[word][category] * count
    
    # Normalize sentiment scores
    if total_words > 0:
        for category in sentiment_scores:
            sentiment_scores[category] /= total_words
    
    return sentiment_scores

# Step 4: Parallel Processing
def parallel_sentiment_analysis(texts, sentiment_dict):
    with multiprocessing.Pool(processes=multiprocessing.cpu_count()) as pool:
        results = pool.starmap(compute_sentiment, [(text, sentiment_dict) for text in texts])
    return results

# Example Usage
sentiment_dict = load_sentiment_dictionary("/mnt/data/Loughran-McDonald_MasterDictionary_1993-2023 Dataset.csv")
example_texts = [
    "The company faced uncertainty and legal constraints after the data breach but showed resilience.",
    "Market performance declined due to financial instability and regulatory concerns."
]
sentiment_results = parallel_sentiment_analysis(example_texts, sentiment_dict)

print("Sentiment Analysis Results:", sentiment_results)
