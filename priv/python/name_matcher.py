"""
Name matching utilities for normalizing scraped entity names
Ported from TypeScript outrights-sst/packages/outrights-data/src/lib/name-matcher.ts
"""

import re
import json


def clean_text(text, pattern=r"'|-|\.|,|:|;|\(|\)|/", reject=None):
    """
    Clean text by removing punctuation and filtering tokens
    Don't use \W here as it will remove accented characters
    """
    if reject is None:
        reject = ["Real"]

    regex = re.compile(pattern)
    tokens = [regex.sub("", tok) for tok in text.split()]
    tokens = [tok for tok in tokens if tok != '' and tok not in reject and not tok.isdigit()]

    # No QPR filter - keep all tokens including important abbreviations
    return " ".join(tok.lower() for tok in tokens)


def is_abbrev(abbrev, text):
    """
    Check if a string is a possible abbreviation for a name
    https://stackoverflow.com/questions/7331462/check-if-a-string-is-a-possible-abbrevation-for-a-name
    """
    words = text.split()

    if not abbrev:
        return True

    if abbrev and not text:
        return False

    if abbrev[0] != text[0]:
        return False

    # Check if we can match abbreviation against words
    rest_of_words = " ".join(words[1:])
    if is_abbrev(abbrev[1:], rest_of_words):
        return True

    # Check if we can match abbreviation within first word
    for i in range(len(words[0])):
        if is_abbrev(abbrev[1:], text[i + 1:]):
            return True

    return False


def token_match(x, y):
    """Calculate token match score between two strings"""
    x_tokens = x.split()
    y_tokens = y.split()
    count = 0

    for x_tok in x_tokens:
        for y_tok in y_tokens:
            if x_tok == y_tok:
                count += 1

    return count / (len(x_tokens) * len(y_tokens)) if x_tokens and y_tokens else 0


def levenshtein(a, b):
    """
    Calculate Levenshtein distance between two strings
    http://hetland.org/coding/python/levenshtein.py
    """
    n, m = len(a), len(b)

    if n > m:
        a, b = b, a
        n, m = m, n

    current = list(range(n + 1))

    for i in range(1, m + 1):
        previous, current = current, [i] + [0] * n

        for j in range(1, n + 1):
            add = previous[j] + 1
            delete = current[j - 1] + 1
            change = previous[j - 1]

            if a[j - 1] != b[i - 1]:
                change = change + 1

            current[j] = min(add, delete, change)

    return current[n]


def run_matcher(matcher_func, text, entities):
    """Run a matcher function against a text and entity list"""
    clean_text_val = clean_text(text)

    for entity in entities:
        entity_names = [entity['name']]
        if 'altNames' in entity:
            entity_names.extend(entity['altNames'])

        for entity_name in entity_names:
            clean_entity_name = clean_text(entity_name)
            if matcher_func(clean_text_val, clean_entity_name):
                return entity['name']

    return None


# Matcher functions in order of preference
MATCHERS = [
    ("exact", lambda x, y: clean_text(x) == clean_text(y)),
    ("levenshtein", lambda x, y: levenshtein(x, y) <= 2),
    ("abbrev0", lambda x, y: is_abbrev(x, y)),
    ("abbrev1", lambda x, y: is_abbrev(y, x)),
    ("token", lambda x, y: token_match(x, y) >= 0.5)
]


def match_entity(text, entities):
    """Match a single entity name against a list of entities"""
    for matcher_name, matcher in MATCHERS:
        entity_name = run_matcher(matcher, text, entities)
        if entity_name:
            return entity_name
    return None


def match_entity_matchup(text, teams):
    """
    Match an entity matchup string (e.g., "Team A vs Team B") against teams
    Returns normalized matchup string or None
    """
    # Split by 'vs' or 'v' with spaces
    parts = re.split(r'\s+vs?\s+', text, flags=re.IGNORECASE)

    if len(parts) != 2:
        return None

    team_names = [match_entity(part, teams) for part in parts]

    # Check if both teams matched and they're different
    if None in team_names or len(set(team_names)) == 1:
        return None

    return f"{team_names[0]} vs {team_names[1]}"


def match_matchup(matchup_text, league_code, teams_data):
    """
    Main entry point for matching a matchup from Erlang

    Args:
        matchup_text: str - The matchup string like "Team A vs Team B"
        league_code: str - League code like "ENG1" or "ENG2"
        teams_data: dict - Dictionary mapping league codes to team lists

    Returns:
        str or None - Normalized matchup string or None if no match
    """
    if league_code not in teams_data:
        return None

    teams = teams_data[league_code]
    return match_entity_matchup(matchup_text, teams)
