#!/usr/bin/env python
"""
Test runner for Python scrapers and utilities
Runs all unittest test cases and reports results
"""

import sys
import unittest
import os

# Add priv/python to path so tests can import modules
script_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(os.path.dirname(script_dir))
priv_python = os.path.join(project_root, 'priv', 'python')
sys.path.insert(0, priv_python)


def main():
    """Run all tests and display results"""
    print("=" * 80)
    print("Python Test Suite")
    print("=" * 80)
    print()

    # Discover and load all test modules
    loader = unittest.TestLoader()
    start_dir = script_dir
    suite = loader.discover(start_dir, pattern='test_*.py')

    # Run tests with detailed output
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)

    # Print summary
    print()
    print("=" * 80)
    print("Test Summary")
    print("=" * 80)
    print(f"Tests run: {result.testsRun}")
    print(f"Successes: {result.testsRun - len(result.failures) - len(result.errors)}")
    print(f"Failures: {len(result.failures)}")
    print(f"Errors: {len(result.errors)}")
    print()

    if result.wasSuccessful():
        print("Result: ALL TESTS PASSED")
        print("=" * 80)
        return 0
    else:
        print("Result: SOME TESTS FAILED")
        print("=" * 80)
        return 1


if __name__ == '__main__':
    sys.exit(main())
