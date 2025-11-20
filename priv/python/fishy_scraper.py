"""
The Fishy Results Parser
Ported from TypeScript outrights-sst/packages/outrights-data/src/parsers/the-fishy-results.ts
Fetches match results from thefishy.co.uk
"""

import re
from datetime import datetime


def clean_text(text):
    """Clean text by removing extra whitespace"""
    tokens = [tok for tok in text.split() if tok != '']
    return ' '.join(tokens)


def parse_date(date_str):
    """Parse date from DD-MMM-YYYY format to YYYY-MM-DD"""
    try:
        date = datetime.strptime(date_str, '%d-%b-%Y')
        return date.strftime('%Y-%m-%d')
    except:
        return None


def remove_duplicates(results):
    """
    Remove duplicate results with same name and score
    The Fishy has a habit of duplicating results with the same score on adjacent days
    Group by name and score but not date, then take the earliest date
    """
    groups = {}

    # Group by name and score
    for result in results:
        key = f"{result['name']}/{result['score']}"
        if key not in groups:
            groups[key] = []
        groups[key].append(result)

    # For each group, take the earliest date
    filtered = []
    for group_results in groups.values():
        # Sort by date in reverse order and take the last (earliest) one
        sorted_results = sorted(group_results, key=lambda x: x['date'], reverse=True)
        filtered.append(sorted_results[-1])

    # Sort final results by date and name
    return sorted(filtered, key=lambda x: (x['date'], x['name']))


def filter_results(html):
    """Parse HTML and extract results"""
    results = []
    current_date = None

    # Find table with table-results class
    table_match = re.search(r'<table[^>]*class="[^"]*table-results[^"]*"[^>]*>(.*?)</table>', html, re.DOTALL)
    if not table_match:
        raise Exception("matches table not found")

    table_html = table_match.group(1)

    # Find all tr elements
    tr_matches = re.findall(r'<tr[^>]*>(.*?)</tr>', table_html, re.DOTALL)

    for tr_html in tr_matches:
        # Handle both single and double quotes for class attribute
        class_match = re.search(r"class=['\"]([^'\"]*)['\"]", tr_html)
        if not class_match:
            continue

        class_name = class_match.group(1)

        if class_name == "success":
            # Extract date from this row
            text_content = re.sub(r'<[^>]*>', ' ', tr_html)
            clean_content = clean_text(text_content)
            parts = clean_content.split(' ')
            if len(parts) > 1:
                try:
                    current_date = parse_date(parts[1])
                except:
                    continue

        elif class_name == "cats" and current_date:
            # Extract team names and scores
            team_matches = re.findall(r"<a[^>]*class=['\"]cats['\"][^>]*>(.*?)</a>", tr_html)
            if len(team_matches) != 2:
                continue

            team_names = [clean_text(re.sub(r'<[^>]*>', '', match)) for match in team_matches]

            # Look for score in catsc class
            score_match = re.search(r"<td[^>]*class=['\"]catsc['\"][^>]*><a[^>]*>(.*?)</a>", tr_html)
            if not score_match:
                score_match = re.search(r"<td[^>]*class=['\"]catsc['\"][^>]*>([^<]*)", tr_html)

            if score_match and score_match.group(1):
                raw_score = score_match.group(1).strip()
                if raw_score and '<' not in raw_score:  # Skip in-play matches with spans
                    # Decode HTML entities (specifically handle the non-breaking hyphen &#8209;)
                    raw_score = re.sub(r'&#8209;', '-', raw_score)
                    raw_score = re.sub(r'&#(\d+);', lambda m: chr(int(m.group(1))), raw_score)

                    # Clean score and join with dash
                    score = re.sub(r'[^\w\s-]', ' ', raw_score)
                    score = '-'.join([s for s in score.split() if s])

                    if score:
                        results.append({
                            'date': current_date,
                            'name': f"{team_names[0]} vs {team_names[1]}",
                            'score': score
                        })

    return remove_duplicates(results)


def parse_fishy_results(html_content):
    """
    Parse The Fishy results from HTML content

    Args:
        html_content: str - HTML content from The Fishy page

    Returns:
        list - List of dicts with keys: date, name, score
    """
    return filter_results(html_content)


def parse_fishy_html(html_content):
    """
    Main entry point for parsing Fishy HTML from Erlang

    Args:
        html_content: str - HTML content from The Fishy page

    Returns:
        list - List of result dicts or empty list on error
    """
    try:
        return parse_fishy_results(html_content)
    except Exception as e:
        print(f"Error parsing Fishy HTML: {e}")
        return []
