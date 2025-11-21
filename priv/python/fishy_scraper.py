"""
The Fishy Results Parser
Ported from TypeScript outrights-sst/packages/outrights-data/src/parsers/the-fishy-results.ts
Fetches match results from thefishy.co.uk
"""

from lxml import html as lxml_html
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


def filter_results(html_content):
    """Parse HTML and extract results using lxml"""
    results = []

    # Parse HTML with lxml
    tree = lxml_html.fromstring(html_content)

    # Find table with table-results class
    tables = tree.xpath("//table[contains(@class, 'table-results')]")
    if not tables:
        raise Exception("matches table not found")

    table = tables[0]
    current_date = None

    # Find all tr elements in the table
    for tr in table.xpath(".//tr"):
        tr_class = tr.get('class', '')

        if tr_class == 'success':
            # Extract date from th element
            th_text = ''.join(tr.xpath(".//th//text()"))
            parts = clean_text(th_text).split()
            if len(parts) >= 1:
                # Extract date (format: "Sun 09-Nov-2025" -> "09-Nov-2025")
                date_str = parts[-1] if len(parts) > 1 else parts[0]
                try:
                    current_date = parse_date(date_str)
                except:
                    continue

        elif tr_class == 'cats' and current_date:
            # Extract team names from a.cats elements
            team_links = tr.xpath(".//a[@class='cats']")
            if len(team_links) != 2:
                continue

            team_names = [clean_text(link.text_content()) for link in team_links]

            # Extract score from td.catsc > a
            score_elements = tr.xpath(".//td[contains(@class, 'catsc')]/a")
            if not score_elements:
                continue

            raw_score = score_elements[0].text_content().strip()
            if raw_score:
                # Clean score - replace various dash characters with standard hyphen
                score = raw_score.replace('‑', '-').replace('–', '-').replace('—', '-')
                # Remove any non-digit/non-dash characters
                score = ''.join(c for c in score if c.isdigit() or c == '-')

                if score and '-' in score:
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
