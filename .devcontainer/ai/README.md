# AI Agent Devcontainer

A VS Code devcontainer pre-configured for autonomous AI agent work (GitHub Copilot agent mode). It runs the same OCaml/MariaDB stack as the regular devcontainer but is intended to operate in parallel with normal development.

## Recommended setup: separate git clone

The workspace folder is bind-mounted **read-write** into the container, so the AI can freely modify files. To prevent the AI from touching your active working tree, **use a dedicated git clone** rather than opening this devcontainer from your main checkout.

```bash
# Clone into a sibling directory
git clone git@github.com:uzh/z-pool-tool.git z-pool-tool-ai

# Open in VS Code, then choose "Reopen in Container → Pool Tool (AI)"
code z-pool-tool-ai
```

With two clones:

|                           | Your clone              | AI clone (`z-pool-tool-ai`)           |
|---------------------------|-------------------------|---------------------------------------|
| Active branch             | your feature branch     | dedicated `ai/<task>` branch          |
| Uncommitted work          | untouched               | AI's changes                          |
| `_build` / `node_modules` | your compiled artefacts | named Docker volumes (not on host FS) |
| `~/.opam`                 | your opam switch        | named Docker volume                   |

Named volumes for `_build`, `node_modules`, and `~/.opam` mean build artefacts never appear in the AI clone's working tree and are preserved across container restarts.

## Starting the AI agent devcontainer

1. Open the `z-pool-tool-ai` folder in VS Code.
2. When prompted, choose **Reopen in Container → Pool Tool (AI)**.
3. `postCreateCommand` runs automatically: it waits for the databases to be ready, then installs OCaml dependencies and Yarn packages (`postCreate.sh`).
4. Run migrations and seed the databases (once, in the container terminal):
   ```bash
   make sihl migrate.root && make sihl seed.root.clean
   make sihl migrate.tenant && make sihl seed.tenant.clean
   ```
5. Open a Copilot Chat panel and switch to **Agent** mode.

> Git SSH forwarding is intentionally **disabled** (`SSH_AUTH_SOCK` is cleared) and `git.autofetch` is off to prevent the agent from silently pushing or fetching.

## Keeping the AI clone up to date

When your main clone has advanced and you want the AI to work from the latest state:

```bash
# In z-pool-tool-ai
git fetch origin
git reset --hard origin/main   # or whichever base branch

# Then in VS Code: Rebuild Container
```

Rebuilding re-runs `postCreateCommand` (dependency install only). Afterwards run migrations manually as in step 4 above.

## Reviewing AI work

The container has no git credentials (`SSH_AUTH_SOCK` is cleared, no gitconfig). All git operations that touch the remote — committing with your identity, pushing, opening PRs — must be done **from the `z-pool-tool-ai` folder on the host** (outside the container), where your gitconfig and SSH agent are available.

```bash
# On the host, in z-pool-tool-ai
git log --oneline          # inspect what the AI committed
git diff main..HEAD        # review changes
git push origin ai/my-task # push once satisfied
```

## Container services

| Service              | Purpose                          | Exposed port           |
|----------------------|----------------------------------|------------------------|
| `ai-agent`           | OCaml build + Copilot agent      | —                      |
| `database-root-ai`   | MariaDB root DB                  | 3316                   |
| `database-tenant-ai` | MariaDB tenant DB                | 3317                   |
| `mailtrap-ai`        | MailHog (catch-all SMTP)         | 1026 (SMTP), 8026 (UI) |

## Installed tooling

- **Extensions:** GitHub Copilot, GitHub Copilot Chat
- **MCP server:** GitKraken (`npx @gitkraken/mcp-server`) — available to Copilot agent for git operations
- **Node / Yarn:** copied from `node:lts` image layer
- **OCaml:** opam switch `5.3.0` (from base image `ghcr.io/uzh/z-pool-tool/ocaml:9-5.3-2.3`)
