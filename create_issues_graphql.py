#!/usr/bin/env python3
"""
Script to create GitHub issues using the GitHub GraphQL API.
This uses the GraphQL endpoint which has been whitelisted.
"""

import json
import os
import sys
import time
import urllib.request
import urllib.error

def create_issue_via_graphql(owner, repo, title, body, label_ids, token):
    """Create a GitHub issue using the GraphQL API."""
    url = "https://api.github.com/graphql"

    # First, we need to get the repository ID
    repo_query = """
    query($owner: String!, $repo: String!) {
      repository(owner: $owner, name: $repo) {
        id
      }
    }
    """

    headers = {
        "Authorization": f"Bearer {token}",
        "Content-Type": "application/json"
    }

    # Get repository ID
    repo_request_data = {
        "query": repo_query,
        "variables": {
            "owner": owner,
            "repo": repo
        }
    }

    try:
        request = urllib.request.Request(
            url,
            data=json.dumps(repo_request_data).encode('utf-8'),
            headers=headers,
            method='POST'
        )

        with urllib.request.urlopen(request) as response:
            result = json.loads(response.read().decode('utf-8'))
            if 'errors' in result:
                print(f"GraphQL Error: {result['errors']}", file=sys.stderr)
                return None, None

            repo_id = result['data']['repository']['id']

    except urllib.error.HTTPError as e:
        error_body = e.read().decode('utf-8')
        print(f"HTTP Error {e.code}: {error_body}", file=sys.stderr)
        return None, None
    except Exception as e:
        print(f"Error getting repo ID: {e}", file=sys.stderr)
        return None, None

    # Create the issue
    create_issue_mutation = """
    mutation($repositoryId: ID!, $title: String!, $body: String!) {
      createIssue(input: {repositoryId: $repositoryId, title: $title, body: $body}) {
        issue {
          number
          url
        }
      }
    }
    """

    issue_request_data = {
        "query": create_issue_mutation,
        "variables": {
            "repositoryId": repo_id,
            "title": title,
            "body": body
        }
    }

    try:
        request = urllib.request.Request(
            url,
            data=json.dumps(issue_request_data).encode('utf-8'),
            headers=headers,
            method='POST'
        )

        with urllib.request.urlopen(request) as response:
            result = json.loads(response.read().decode('utf-8'))
            if 'errors' in result:
                print(f"GraphQL Error: {result['errors']}", file=sys.stderr)
                return None, None

            issue_data = result['data']['createIssue']['issue']
            return issue_data['url'], issue_data['number']

    except urllib.error.HTTPError as e:
        error_body = e.read().decode('utf-8')
        print(f"HTTP Error {e.code}: {error_body}", file=sys.stderr)
        return None, None
    except Exception as e:
        print(f"Error creating issue: {e}", file=sys.stderr)
        return None, None


def add_labels_to_issue(owner, repo, issue_number, labels, token):
    """Add labels to an issue using the REST API."""
    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/labels"

    headers = {
        "Accept": "application/vnd.github.v3+json",
        "Authorization": f"Bearer {token}",
        "Content-Type": "application/json"
    }

    data = {
        "labels": labels
    }

    try:
        request = urllib.request.Request(
            url,
            data=json.dumps(data).encode('utf-8'),
            headers=headers,
            method='POST'
        )

        with urllib.request.urlopen(request) as response:
            return True
    except Exception as e:
        print(f"Error adding labels: {e}", file=sys.stderr)
        return False


def main():
    """Main function to create all issues."""

    # Configuration
    owner = "Open-Systems-Pharmacology"
    repo = "OSPSuite-R"
    parent_issue_number = 1765

    # Get token from environment
    token = os.environ.get('GITHUB_TOKEN')
    if not token:
        print("Error: GITHUB_TOKEN environment variable not set!", file=sys.stderr)
        return 1

    # Load issue data
    json_file = "issue-breakdown.json"
    if not os.path.exists(json_file):
        print(f"Error: {json_file} not found!", file=sys.stderr)
        return 1

    with open(json_file, 'r') as f:
        issues = json.load(f)

    print("=" * 80)
    print(f"Creating {len(issues)} GitHub Issues via GraphQL API")
    print("=" * 80)
    print(f"Repository: {owner}/{repo}")
    print(f"Parent Issue: #{parent_issue_number}")
    print("=" * 80)
    print()

    created_issues = []
    failed_issues = []

    for i, issue_data in enumerate(issues, 1):
        title = issue_data['title']
        body = issue_data['body']
        labels = issue_data['labels']
        priority = issue_data['priority']

        # Add parent issue reference to body
        body_with_parent = f"{body}\n\n---\n\n**Parent Issue:** #{parent_issue_number}"

        print(f"[{i}/{len(issues)}] Creating issue...")
        print(f"  Priority: {priority}")
        print(f"  Title: {title[:60]}...")

        issue_url, issue_number = create_issue_via_graphql(
            owner=owner,
            repo=repo,
            title=title,
            body=body_with_parent,
            label_ids=None,
            token=token
        )

        if issue_url and issue_number:
            print(f"  ✓ Created: #{issue_number} - {issue_url}")

            # Try to add labels (REST API call - may fail due to firewall)
            if add_labels_to_issue(owner, repo, issue_number, labels, token):
                print(f"  ✓ Labels added: {', '.join(labels)}")
            else:
                print(f"  ⚠ Could not add labels (need to add manually)")

            created_issues.append({
                'number': issue_number,
                'title': title,
                'url': issue_url,
                'priority': priority,
                'labels': labels
            })
        else:
            print(f"  ✗ Failed to create issue")
            failed_issues.append({
                'title': title,
                'priority': priority
            })

        # Rate limiting - wait between requests
        if i < len(issues):
            time.sleep(2)

        print()

    # Print summary
    print("=" * 80)
    print("SUMMARY")
    print("=" * 80)
    print(f"✓ Successfully created: {len(created_issues)} issues")
    print(f"✗ Failed: {len(failed_issues)} issues")
    print()

    if created_issues:
        print("-" * 80)
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
                    print(f"  #{issue['number']}: {issue['title']}")
                    print(f"    {issue['url']}")
                    if 'labels' in issue:
                        print(f"    Labels: {', '.join(issue['labels'])}")

    if failed_issues:
        print("\n" + "-" * 80)
        print("FAILED ISSUES:")
        print("-" * 80)
        for issue in failed_issues:
            print(f"  [{issue['priority']}] {issue['title']}")

    # Save results
    results_file = "issue-creation-results.json"
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
