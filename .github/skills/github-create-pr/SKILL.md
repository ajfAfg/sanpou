---
name: github-create-pr
description: "Use when: creating a GitHub pull request from a pushed branch. Covers gh auth checks, branch/upstream verification, PR title/body preparation, and gh pr create."
---

# Create a GitHub Pull Request

Use this skill when the user asks to create a PR.

## Workflow

### 1. Inspect Branch and Remote

Run:

```sh
git branch --show-current
git --no-pager status --short
git --no-pager remote -v
git --no-pager branch -vv
```

Confirm the current branch is the intended PR head and that it has been pushed. If it has not been pushed and the user wants you to push, run:

```sh
git push -u origin <branch-name>
```

### 2. Check GitHub CLI

Run:

```sh
command -v gh
gh auth status
```

If `gh` is unavailable or unauthenticated, report the blocker and provide the exact command the user can run.

### 3. Summarize the Branch

Gather PR context:

```sh
git --no-pager log --oneline main..HEAD
git --no-pager diff --stat main...HEAD
```

Use `origin/main` instead of `main` if the local base is stale or missing.

### 4. Create the PR

Use a concise title, usually matching the main Conventional Commit:

```sh
gh pr create \
  --base main \
  --head <branch-name> \
  --title "<title>" \
  --body "$(cat <<'EOF'
## Summary
- ...
- ...

## Verification
- ...
EOF
)"
```

The body should include:

- Summary of behavior or documentation changes.
- Verification commands that ran.
- Any checks that were skipped and why.

### 5. Verify

After creation, report the URL. Optionally confirm with:

```sh
gh pr view --json url,title,headRefName,baseRefName
```

## Guardrails

- Do not create a PR from the wrong branch. Confirm the head branch first.
- Do not include secrets in the PR body or command output.
- If the repository uses a different base than `main`, use the base requested by the user or shown by repository context.
