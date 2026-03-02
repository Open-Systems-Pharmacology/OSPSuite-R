#!/usr/bin/env python3
"""
Script to create GitHub issues for performance optimization tasks.
This script reads the issue breakdown from issue-breakdown.json and creates
individual GitHub issues using the GitHub REST API.

Usage:
    python create_issues.py

Requirements:
    - GitHub CLI (gh) must be installed and authenticated
    - The script should be run from the repository root
"""

import json
import subprocess
import sys
import time
from pathlib import Path


def load_issue_data(file_path):
    """Load issue data from JSON file."""
    with open(file_path, 'r') as f:
        return json.load(f)


def create_github_issue(owner, repo, title, body, labels, parent_issue_number):
    """Create a GitHub issue using the gh CLI."""

    # Add parent issue reference to body
    body_with_parent = f"{body}\n\n---\n\n**Parent Issue:** #{parent_issue_number}"

    # Construct the gh command
    cmd = [
        'gh', 'issue', 'create',
        '--repo', f'{owner}/{repo}',
        '--title', title,
        '--body', body_with_parent,
    ]

    # Add labels
    for label in labels:
        cmd.extend(['--label', label])

    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            check=True
        )

        # Extract issue URL from output
        issue_url = result.stdout.strip()
        print(f"✓ Created issue: {title}")
        print(f"  URL: {issue_url}")
        return issue_url

    except subprocess.CalledProcessError as e:
        print(f"✗ Failed to create issue: {title}")
        print(f"  Error: {e.stderr}")
        return None


def main():
    """Main function to create all issues."""

    # Configuration
    owner = "Open-Systems-Pharmacology"
    repo = "OSPSuite-R"
    parent_issue_number = 1765

    # Load issue data
    json_file = Path(__file__).parent / "issue-breakdown.json"

    if not json_file.exists():
        print(f"Error: {json_file} not found!")
        sys.exit(1)

    issues = load_issue_data(json_file)

    print(f"Creating {len(issues)} issues for {owner}/{repo}...")
    print(f"Parent issue: #{parent_issue_number}")
    print("-" * 80)

    created_issues = []
    failed_issues = []

    for i, issue_data in enumerate(issues, 1):
        print(f"\n[{i}/{len(issues)}] Creating issue: {issue_data['title'][:60]}...")

        issue_url = create_github_issue(
            owner=owner,
            repo=repo,
            title=issue_data['title'],
            body=issue_data['body'],
            labels=issue_data['labels'],
            parent_issue_number=parent_issue_number
        )

        if issue_url:
            created_issues.append({
                'number': i,
                'title': issue_data['title'],
                'url': issue_url,
                'priority': issue_data['priority']
            })
        else:
            failed_issues.append({
                'number': i,
                'title': issue_data['title'],
                'priority': issue_data['priority']
            })

        # Rate limiting: sleep between requests
        if i < len(issues):
            time.sleep(1)

    # Print summary
    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)
    print(f"✓ Successfully created: {len(created_issues)} issues")
    print(f"✗ Failed: {len(failed_issues)} issues")

    if created_issues:
        print("\n" + "-" * 80)
        print("CREATED ISSUES:")
        print("-" * 80)

        # Group by priority
        priorities = {}
        for issue in created_issues:
            priority = issue['priority']
            if priority not in priorities:
                priorities[priority] = []
            priorities[priority].append(issue)

        for priority in ['CRITICAL', 'HIGH', 'MEDIUM-HIGH', 'MEDIUM', 'LOW-MEDIUM', 'LOW']:
            if priority in priorities:
                print(f"\n{priority} Priority:")
                for issue in priorities[priority]:
                    print(f"  - {issue['title']}")
                    print(f"    {issue['url']}")

    if failed_issues:
        print("\n" + "-" * 80)
        print("FAILED ISSUES:")
        print("-" * 80)
        for issue in failed_issues:
            print(f"  [{issue['priority']}] {issue['title']}")

    # Save results
    results_file = Path(__file__).parent / "issue-creation-results.json"
    with open(results_file, 'w') as f:
        json.dump({
            'parent_issue': parent_issue_number,
            'created': created_issues,
            'failed': failed_issues
        }, f, indent=2)

    print(f"\nResults saved to: {results_file}")

    return 0 if not failed_issues else 1


if __name__ == "__main__":
    sys.exit(main())
