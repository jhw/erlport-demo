#!/usr/bin/env python
"""Test name matcher with sample data"""

import os
import sys
import unittest
import json

# Add priv/python to path so we can import the matcher
script_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(os.path.dirname(script_dir))
priv_python = os.path.join(project_root, 'apps', 'erlport_demo', 'priv', 'python')
sys.path.insert(0, priv_python)

from name_matcher import match_entity, match_matchups_batch


class TestNameMatcher(unittest.TestCase):
    """Test name matcher functionality"""

    def setUp(self):
        """Set up test data"""
        self.teams = [
            {"name": "Manchester United", "altNames": ["Man Utd", "Man United"]},
            {"name": "Liverpool", "altNames": ["Liverpool FC"]},
            {"name": "Chelsea", "altNames": ["Chelsea FC"]},
            {"name": "Arsenal", "altNames": ["Arsenal FC", "The Gunners"]},
            {"name": "Manchester City", "altNames": ["Man City"]},
            {"name": "Tottenham Hotspur", "altNames": ["Spurs", "Tottenham"]}
        ]
        self.teams_data = {"ENG1": self.teams}

    def test_match_entity_exact_name(self):
        """Test matching with exact team name"""
        result = match_entity("Manchester United", self.teams)
        self.assertEqual(result, "Manchester United")

    def test_match_entity_alt_name(self):
        """Test matching with alternative name"""
        result = match_entity("Man Utd", self.teams)
        self.assertEqual(result, "Manchester United")

        result = match_entity("Spurs", self.teams)
        self.assertEqual(result, "Tottenham Hotspur")

    def test_match_entity_case_insensitive(self):
        """Test that matching is case insensitive"""
        result = match_entity("man utd", self.teams)
        self.assertEqual(result, "Manchester United")

        result = match_entity("LIVERPOOL", self.teams)
        self.assertEqual(result, "Liverpool")

    def test_match_entity_no_match(self):
        """Test that invalid team returns None"""
        result = match_entity("Invalid Team", self.teams)
        self.assertIsNone(result)

    def test_match_matchups_batch_valid(self):
        """Test batch matching with valid matchups"""
        matchups = [
            "Man Utd vs Liverpool",
            "Chelsea vs Arsenal",
            "Man City vs Spurs"
        ]

        json_input = json.dumps({


            "matchup_texts": matchups,


            "league_code": "ENG1",


            "teams_data": self.teams_data


        })


        json_result = match_matchups_batch(json_input)


        result = json.loads(json_result)

        self.assertIn("matched", result)
        self.assertIn("unmatched", result)
        self.assertEqual(len(result["matched"]), 3)
        self.assertEqual(len(result["unmatched"]), 0)

        # Check specific matches
        self.assertEqual(
            result["matched"]["Man Utd vs Liverpool"],
            "Manchester United vs Liverpool"
        )
        self.assertEqual(
            result["matched"]["Man City vs Spurs"],
            "Manchester City vs Tottenham Hotspur"
        )

    def test_match_matchups_batch_mixed(self):
        """Test batch matching with mix of valid and invalid matchups"""
        matchups = [
            "Man Utd vs Liverpool",
            "Invalid Team vs Another Invalid",
            "Chelsea vs Arsenal"
        ]

        json_input = json.dumps({


            "matchup_texts": matchups,


            "league_code": "ENG1",


            "teams_data": self.teams_data


        })


        json_result = match_matchups_batch(json_input)


        result = json.loads(json_result)

        self.assertEqual(len(result["matched"]), 2)
        self.assertEqual(len(result["unmatched"]), 1)
        self.assertIn("Invalid Team vs Another Invalid", result["unmatched"])

    def test_match_matchups_batch_all_invalid(self):
        """Test batch matching with all invalid matchups"""
        matchups = [
            "Invalid vs Team",
            "Another vs NoMatch"
        ]

        json_input = json.dumps({


            "matchup_texts": matchups,


            "league_code": "ENG1",


            "teams_data": self.teams_data


        })


        json_result = match_matchups_batch(json_input)


        result = json.loads(json_result)

        self.assertEqual(len(result["matched"]), 0)
        self.assertEqual(len(result["unmatched"]), 2)

    def test_match_matchups_batch_empty(self):
        """Test batch matching with empty list"""
        json_input = json.dumps({

            "matchup_texts": [],

            "league_code": "ENG1",

            "teams_data": self.teams_data

        })

        json_result = match_matchups_batch(json_input)

        result = json.loads(json_result)

        self.assertEqual(len(result["matched"]), 0)
        self.assertEqual(len(result["unmatched"]), 0)

    def test_match_matchups_batch_different_separators(self):
        """Test batch matching with different separators (vs, v)"""
        matchups = [
            "Man Utd vs Liverpool",
            "Chelsea v Arsenal",
            "Man City vs. Spurs"
        ]

        json_input = json.dumps({


            "matchup_texts": matchups,


            "league_code": "ENG1",


            "teams_data": self.teams_data


        })


        json_result = match_matchups_batch(json_input)


        result = json.loads(json_result)

        # Should handle different separators
        self.assertGreater(len(result["matched"]), 0)

    def test_match_matchups_batch_invalid_league(self):
        """Test batch matching with invalid league"""
        matchups = ["Man Utd vs Liverpool"]

        json_input = json.dumps({


            "matchup_texts": matchups,


            "league_code": "INVALID_LEAGUE",


            "teams_data": self.teams_data


        })


        json_result = match_matchups_batch(json_input)


        result = json.loads(json_result)

        # Should return all unmatched since league doesn't exist
        self.assertEqual(len(result["matched"]), 0)
        self.assertEqual(len(result["unmatched"]), 1)

    def test_match_entity_whitespace_handling(self):
        """Test that extra whitespace is handled correctly"""
        result = match_entity("  Man Utd  ", self.teams)
        self.assertEqual(result, "Manchester United")


if __name__ == '__main__':
    unittest.main()
