---
name: git-create-branch
description: "Use when: creating or switching to a git branch, especially when work must happen on a separate branch. Covers base branch selection, worktree inspection, and non-destructive branch creation."
---

# Create a Git Branch

Use this skill when the user asks to create a branch or move work to a separate branch.

## Workflow

### 1. Inspect Current State

Run:

```sh
git --no-pager status --short
git branch --show-current
git --no-pager remote -v
```

If local changes may block branch switching, do not stash, move, or discard them without explicit user approval.

### 2. Choose the Base

Use the base implied by the request. If the user says the work should be separate from the current branch, start from the intended upstream base:

```sh
git fetch origin main
git switch -c <branch-name> origin/main
```

If the current branch is already the correct base:

```sh
git switch -c <branch-name>
```

### 3. Name the Branch

Prefer short descriptive names:

```text
feat/<topic>
fix/<topic>
docs/<topic>
chore/<topic>
```

Examples:

```text
feat/procedure-return-values
docs/git-agent-workflow
fix/trace-source-map
```

### 4. Verify

Run:

```sh
git branch --show-current
git --no-pager status --short
```

Report the branch name and any important status notes.

## Guardrails

- Do not use destructive commands such as `git reset --hard` or `git checkout --` unless explicitly requested.
- Do not stash changes unless the user explicitly approves it.
- If branch creation fails because of local changes, explain the blocker and ask how to proceed.
