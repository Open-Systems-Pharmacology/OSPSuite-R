# Issue #1765 Breakdown

This directory contains scripts and data for breaking down issue #1765 (Performance optimization) into separate, manageable tasks.

## Files

- `issue-breakdown.json` - JSON file containing all 19 performance optimization tasks extracted from #1765
- `create_issues.py` - Python script to create GitHub issues from the breakdown
- `issue-creation-results.json` - (Generated) Results of the issue creation process

## Issue Breakdown Summary

The comprehensive performance optimization analysis in #1765 has been broken down into **19 separate tasks**, organized by priority:

### Critical Priority (2 tasks)
1. Batch covariate retrieval in getOutputValues for population simulations
2. Optimize unit conversion operations to reduce .NET interop calls

### High Priority (3 tasks)
3. Replace rbind.data.frame with data.table for efficient row binding
4. Implement batch entity matching to reduce .NET interop calls
5. Reduce multiple split/apply/bind cycles in unit conversion

### Medium-High Priority (2 tasks)
6. Optimize sequential simulation addition in batch operations
7. Implement batch metadata retrieval for simulation results

### Medium Priority (7 tasks)
8. Cache population count to reduce property access overhead
9. Optimize entity to path conversion with vectorization
10. Implement batch quantity value setting
11. Implement automatic property access caching in DotNetWrapper
12. Optimize unique entity operations with faster duplicate detection
13. Optimize cache operations for individual simulation results
14. Reduce unnecessary memory copies in vector operations
15. Remove unnecessary column creation in unit converter

### Low-Medium Priority (3 tasks)
16. Cache path separator pattern in path operations
17. Use vapply for path extraction in simulation setup
18. Ensure consistent pre-allocation of vectors in loops

### Low Priority (1 task)
19. Optimize cache printing for large caches

## Creating the Issues

### Prerequisites

1. Install and authenticate GitHub CLI:
   ```bash
   # Install gh (if not already installed)
   # macOS: brew install gh
   # Ubuntu: sudo apt install gh
   # Windows: choco install gh

   # Authenticate
   gh auth login
   ```

2. Ensure you have write access to the Open-Systems-Pharmacology/OSPSuite-R repository

### Running the Scripts

**Option 1: Bash Script (Recommended)**

From the repository root:

```bash
./create_issues.sh
```

This script will:
- Check prerequisites (gh CLI installed and authenticated)
- Ask for confirmation before proceeding
- Create all 19 issues with appropriate labels
- Link each issue to parent issue #1765
- Display progress and summary

**Option 2: Python Script**

From the repository root:

```bash
python3 create_issues.py
```

This script provides the same functionality as the bash script with additional JSON output for results tracking.

### Manual Creation

If you prefer to create issues manually or need to create them one at a time, you can use the `gh` CLI directly:

```bash
# Example for creating a single issue
gh issue create \
  --repo Open-Systems-Pharmacology/OSPSuite-R \
  --title "Batch covariate retrieval in getOutputValues for population simulations" \
  --body "$(cat issue-1-body.md)" \
  --label "performance" \
  --label "critical" \
  --label "population-simulation"
```

Or use the GitHub web interface and copy the content from `issue-breakdown.json`.

## Structure of Each Issue

Each created issue includes:

- **Title**: Descriptive title of the optimization task
- **Labels**: Priority level and relevant component tags
- **Body**:
  - Issue Overview
  - Current Problem (with file location and code snippet)
  - Proposed Solution (with example code)
  - Expected Impact (priority, estimated improvement, effort level)
  - Implementation Notes
  - Parent Issue reference (link to #1765)

## After Creation

Once all issues are created:

1. Review each issue for accuracy
2. Assign issues to team members or AI agents as appropriate
3. Close issues that are deemed not worth implementing
4. Track progress through the parent issue #1765
5. Create benchmarks for critical and high-priority optimizations before implementation

## Notes

- The original issue #1765 remains unchanged and serves as the parent issue
- Each sub-issue can be worked on independently
- Some optimizations may require .NET API enhancements
- Benchmarking should be done before and after each optimization
- Test coverage must be maintained or improved
