---
name: git-create-commit
description: "Use when: staging changes or creating a git commit. Covers staged diff review, validation before commit, .gitignore cleanup for local artifacts, and Conventional Commit messages."
---

# Create a Git Commit

Use this skill when the user asks to stage changes or create a commit.

## Workflow

### 1. Inspect the Worktree

Run:

```sh
git --no-pager status --short
git branch --show-current
```

If local tooling artifacts repeatedly appear in status, consider adding narrow `.gitignore` rules. For this repository, common local artifacts include:

```text
.vscode/
states/
tla2tools.jar
```

Review `.gitignore` before editing it.

### 2. Stage the Intended Files

Stage explicit paths:

```sh
git add <file1> <file2> <file3>
```

Review the staged change:

```sh
git --no-pager diff --cached --stat
git --no-pager diff --cached --name-only
git --no-pager diff --cached --check
```

### 3. Validate

Run the narrowest useful validation for the touched area. Common commands in this repository:

```sh
dune runtest
dune exec -- sanpou compile example/factorial.snp -o dist
mise run model-check --config dist/factorial.cfg dist/factorial.tla
```

If validation cannot run because a tool or credential is unavailable, say so in the final response.

### 4. Commit

Use Conventional Commit format:

```text
<type>(optional-scope): <summary>
```

Common types:

- `feat`: user-visible feature or capability
- `fix`: bug fix
- `docs`: documentation-only change
- `test`: test-only change
- `refactor`: behavior-preserving code restructure
- `chore`: tooling, maintenance, metadata

Examples:

```sh
git commit -m "feat: support procedure return values"
git commit -m "docs: add git workflow skills"
git commit -m "fix: annotate TLC trace labels from source maps"
```

### 5. Verify the Commit

Run:

```sh
git --no-pager log -1 --oneline
git --no-pager status --short
```

Report the commit hash, message, branch, and validation result.

## Guardrails

- If the user asks for Conventional Commit, use it exactly.
- If `git commit` is cancelled, wait for clarification or retry only with the corrected requested message.
- Do not amend, squash, or force-push unless the user asks.
- Do not request secrets through chat. If a command needs credentials, ask the user to type them directly in the terminal.
