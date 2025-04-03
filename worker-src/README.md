# Email Queue Worker

## Overview

The `pool-worker` project is designed to manage an email queue using Celery. It provides a simple interface for sending emails asynchronously and includes a UI for monitoring the status of the email queue.

## Project Structure
```
.
├── pool-worker
│   ├── __init__.py
│   ├── celery_app.py
│   ├── tasks
│   │   ├── __init__.py
│   │   └── email_tasks.py
│   └── utils
│       ├── __init__.py
│       └── json_encoder.py
├── pyproject.toml
├── README.md
└── .env
```

## Usage

1. Create a `.env` file in the root directory and configure your environment variables for the email service and Celery broker.

1. Start the Celery worker:
   ```
   poetry run celery -A pool_worker.celery_app worker --loglevel=info
   ```

1. Access the UI at `http://localhost:5000` to view email queue statistics.

## Development
- To run linting and code quality checks, use:
  ```
  poetry run ruff .
  poetry run bandit -r src
  ```
