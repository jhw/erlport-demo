"""
BBC Sport Results Parser
Ported from TypeScript outrights-sst/packages/outrights-data/src/parsers/bbc-results.ts
Parses match results from BBC Sport HTML content
"""

from lxml import html as lxml_html
import json
from datetime import datetime


def parse_bbc_results(html_content):
    """
    Parse BBC results from HTML content containing JSON data
    Extracts JSON data from BBC's initial data script tag and processes results

    Args:
        html_content: str - HTML content from BBC Sport page

    Returns:
        list - List of dicts with keys: date, name, score
    """
    try:
        # Parse HTML with lxml
        tree = lxml_html.fromstring(html_content)

        # Find all script tags and look for the one with __INITIAL_DATA__
        script_elements = tree.xpath('//script')

        initial_data_script = None
        for script in script_elements:
            script_text = script.text_content()
            if 'window.__INITIAL_DATA__' in script_text:
                initial_data_script = script_text
                break

        if not initial_data_script:
            raise Exception('No BBC initial data script found in HTML')

        # Extract JSON from the variable declaration
        # Find the start of the JSON data after "window.__INITIAL_DATA__ = "
        start_marker = 'window.__INITIAL_DATA__'
        start_idx = initial_data_script.find(start_marker)
        if start_idx == -1:
            raise Exception('Could not find __INITIAL_DATA__ marker')

        # Skip past the marker and the " = " part
        json_start = initial_data_script.find('=', start_idx) + 1
        json_string = initial_data_script[json_start:].strip()

        # Remove trailing semicolon if present
        if json_string.endswith(';'):
            json_string = json_string[:-1].strip()

        # If it's a quoted string, remove quotes and unescape
        if json_string.startswith('"') and json_string.endswith('"'):
            json_string = json_string[1:-1]
            json_string = json_string.replace('\\"', '"').replace('\\\\', '\\')

        initial_data = json.loads(json_string)

        # Find the sport-data-scores-fixtures data key
        scores_fixtures_key = None
        if 'data' in initial_data:
            for key in initial_data['data'].keys():
                if 'sport-data-scores-fixtures' in key:
                    scores_fixtures_key = key
                    break

        if not scores_fixtures_key:
            raise Exception('No sport-data-scores-fixtures data found in BBC JSON')

        # Navigate to the event groups
        scores_data = initial_data['data'][scores_fixtures_key].get('data')
        event_groups = scores_data.get('eventGroups') if scores_data else None

        if not isinstance(event_groups, list):
            raise Exception('Invalid eventGroups structure in BBC scores data')

        # Extract results from event groups
        results = []

        for group in event_groups:
            if 'secondaryGroups' in group:
                for secondary_group in group['secondaryGroups']:
                    if 'events' in secondary_group:
                        for event in secondary_group['events']:
                            # Only include completed matches
                            if event.get('status') == 'PostEvent':
                                home_team = event.get('home', {})
                                away_team = event.get('away', {})

                                home_name = home_team.get('fullName') or home_team.get('shortName') or 'Unknown'
                                away_name = away_team.get('fullName') or away_team.get('shortName') or 'Unknown'
                                home_score = home_team.get('score', '0')
                                away_score = away_team.get('score', '0')

                                # Get date
                                date_obj = event.get('date', {})
                                iso_date = date_obj.get('isoDate')
                                if iso_date:
                                    match_date = iso_date.split('T')[0]
                                else:
                                    match_date = datetime.now().strftime('%Y-%m-%d')

                                results.append({
                                    'date': match_date,
                                    'name': f"{home_name} vs {away_name}",
                                    'score': f"{home_score}-{away_score}"
                                })

        return results

    except Exception as e:
        raise Exception(f"Failed to parse BBC results data: {str(e)}")


def parse_bbc_html(json_input):
    """
    Main entry point for parsing BBC HTML from Erlang via ErlCracker

    ErlCracker requires JSON transport, so HTML is wrapped in a JSON object.

    Args:
        json_input: str - JSON string with {"html": "<html content>"}

    Returns:
        str - JSON string with list of result dicts or empty list on error
    """
    try:
        # Unwrap HTML from JSON envelope
        data = json.loads(json_input)
        html_content = data.get('html', '')

        if not html_content:
            return json.dumps([])

        results = parse_bbc_results(html_content)
        return json.dumps(results)
    except Exception as e:
        print(f"Error parsing BBC HTML: {e}")
        return json.dumps([])
