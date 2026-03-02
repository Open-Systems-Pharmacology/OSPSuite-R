#!/bin/bash

# Script to create GitHub issues for performance optimization tasks
# This script requires gh CLI to be installed and authenticated

set -e

OWNER="Open-Systems-Pharmacology"
REPO="OSPSuite-R"
PARENT_ISSUE=1765
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JSON_FILE="$SCRIPT_DIR/issue-breakdown.json"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if gh is installed
if ! command -v gh &> /dev/null; then
    echo -e "${RED}Error: GitHub CLI (gh) is not installed.${NC}"
    echo "Please install it from https://cli.github.com/"
    exit 1
fi

# Check if gh is authenticated
if ! gh auth status &> /dev/null; then
    echo -e "${RED}Error: GitHub CLI is not authenticated.${NC}"
    echo "Please run: gh auth login"
    exit 1
fi

# Check if JSON file exists
if [ ! -f "$JSON_FILE" ]; then
    echo -e "${RED}Error: $JSON_FILE not found!${NC}"
    exit 1
fi

echo "======================================================================"
echo "Creating GitHub Issues for Performance Optimization"
echo "======================================================================"
echo "Repository: $OWNER/$REPO"
echo "Parent Issue: #$PARENT_ISSUE"
echo "======================================================================"
echo

# Count total issues
TOTAL_ISSUES=$(jq 'length' "$JSON_FILE")
echo "Total issues to create: $TOTAL_ISSUES"
echo

# Ask for confirmation
read -p "Do you want to proceed? (y/n) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Aborted."
    exit 0
fi

echo
echo "Creating issues..."
echo "======================================================================"

CREATED=0
FAILED=0

# Read JSON and create issues
for i in $(seq 0 $((TOTAL_ISSUES - 1))); do
    TITLE=$(jq -r ".[$i].title" "$JSON_FILE")
    BODY=$(jq -r ".[$i].body" "$JSON_FILE")
    PRIORITY=$(jq -r ".[$i].priority" "$JSON_FILE")
    LABELS=$(jq -r ".[$i].labels | join(\",\")" "$JSON_FILE")

    echo
    echo "[$((i + 1))/$TOTAL_ISSUES] Creating issue..."
    echo "  Priority: $PRIORITY"
    echo "  Title: $TITLE"

    # Add parent issue reference to body
    BODY_WITH_PARENT="$BODY

---

**Parent Issue:** #$PARENT_ISSUE"

    # Create the issue
    if ISSUE_URL=$(echo "$BODY_WITH_PARENT" | gh issue create \
        --repo "$OWNER/$REPO" \
        --title "$TITLE" \
        --body-file - \
        --label "$LABELS" 2>&1); then

        echo -e "  ${GREEN}✓ Created:${NC} $ISSUE_URL"
        ((CREATED++))
    else
        echo -e "  ${RED}✗ Failed to create issue${NC}"
        echo "  Error: $ISSUE_URL"
        ((FAILED++))
    fi

    # Rate limiting - wait between requests
    if [ $i -lt $((TOTAL_ISSUES - 1)) ]; then
        sleep 1
    fi
done

echo
echo "======================================================================"
echo "SUMMARY"
echo "======================================================================"
echo -e "${GREEN}✓ Successfully created: $CREATED issues${NC}"
if [ $FAILED -gt 0 ]; then
    echo -e "${RED}✗ Failed: $FAILED issues${NC}"
fi
echo

exit 0
