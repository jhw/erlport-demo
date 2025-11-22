#!/usr/bin/env python
"""Test BBC scraper with fixture data"""

import os
import sys
import unittest
import json

# Add priv/python to path so we can import the scraper
script_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(os.path.dirname(script_dir))
priv_python = os.path.join(project_root, 'apps', 'erlport_demo', 'priv', 'python')
sys.path.insert(0, priv_python)

from bbc_scraper import parse_bbc_html


class TestBBCScraper(unittest.TestCase):
    """Test BBC scraper functionality"""

    def setUp(self):
        """Load fixture data"""
        self.fixture_path = os.path.join(script_dir, 'fixtures', 'bbc_eng1.html')
        with open(self.fixture_path, 'r', encoding='utf-8') as f:
            self.html_content = f.read()

    def test_fixture_loads(self):
        """Test that fixture file loads successfully"""
        self.assertIsNotNone(self.html_content)
        self.assertGreater(len(self.html_content), 0)

    def test_parse_returns_json_string(self):
        """Test that parser returns a JSON string"""
        # Wrap HTML in JSON as ErlCracker requires
        json_input = json.dumps({"html": self.html_content})
        json_result = parse_bbc_html(json_input)
        self.assertIsInstance(json_result, str)
        # Should be valid JSON
        results = json.loads(json_result)
        self.assertIsInstance(results, list)

    def test_parse_finds_matches(self):
        """Test that parser finds expected number of matches"""
        json_input = json.dumps({"html": self.html_content})
        json_result = parse_bbc_html(json_input)
        results = json.loads(json_result)
        self.assertEqual(len(results), 20, f"Expected 20 matches, found {len(results)}")

    def test_result_structure(self):
        """Test that each result has required fields"""
        json_input = json.dumps({"html": self.html_content})
        json_result = parse_bbc_html(json_input)
        results = json.loads(json_result)
        self.assertGreater(len(results), 0, "No results found to test structure")

        for result in results:
            self.assertIn('date', result)
            self.assertIn('name', result)
            self.assertIn('score', result)

    def test_result_format(self):
        """Test that results have correct format"""
        json_input = json.dumps({"html": self.html_content})
        json_result = parse_bbc_html(json_input)
        results = json.loads(json_result)

        for result in results:
            # Check date format (YYYY-MM-DD)
            self.assertRegex(result['date'], r'^\d{4}-\d{2}-\d{2}$')

            # Check name contains 'vs'
            self.assertIn(' vs ', result['name'])

            # Check score format (N-N)
            self.assertRegex(result['score'], r'^\d+-\d+$')

    def test_known_matches(self):
        """Test that specific known matches are found"""
        json_input = json.dumps({"html": self.html_content})
        json_result = parse_bbc_html(json_input)
        results = json.loads(json_result)

        # Look for specific known matches
        result_strings = [f"{r['name']} ({r['score']})" for r in results]

        self.assertTrue(
            any('Manchester City vs Liverpool (3-0)' in s for s in result_strings),
            "Expected to find Man City vs Liverpool 3-0"
        )

    def test_empty_html_returns_empty_list(self):
        """Test that empty HTML returns empty JSON list"""
        json_input = json.dumps({"html": ""})
        json_result = parse_bbc_html(json_input)
        results = json.loads(json_result)
        self.assertIsInstance(results, list)
        self.assertEqual(len(results), 0)

    def test_invalid_html_returns_empty_list(self):
        """Test that invalid HTML returns empty JSON list without crashing"""
        json_input = json.dumps({"html": "<html><body>No data here</body></html>"})
        json_result = parse_bbc_html(json_input)
        results = json.loads(json_result)
        self.assertIsInstance(results, list)
        self.assertEqual(len(results), 0)


if __name__ == '__main__':
    unittest.main()
