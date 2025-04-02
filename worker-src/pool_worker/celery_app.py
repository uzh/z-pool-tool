import os

from celery import Celery
from dotenv import load_dotenv
from kombu import Exchange, Queue

from pool_worker.accounts import populate_accounts_cache
from pool_worker.send_mail import send_email


def make_celery():
    load_dotenv()
    environment = os.getenv("ENVIRONMENT", "development")

    names = populate_accounts_cache()
    queues = []
    task_routes = {}

    celery = Celery(
        "worker",
        broker=os.getenv("CELERY_BROKER_URL"),
    )

    for name in names:
        queue_name = f"{name}_{environment}_queue"
        dlq_routing_key = f"{name}_{environment}_dlq"
        dlx_exchange = Exchange(
            f"{name}_{environment}_dlx",
            type="direct",
            durable=True,
        )
        queue = Queue(
            queue_name,
            exchange_type="direct",
            queue_arguments={
                "x-dead-letter-exchange": dlx_exchange.name,
                "x-dead-letter-routing-key": dlq_routing_key,
                "x-message-ttl": (1000 * 60 * 60 * 24),  # 1 day,
            },
        )
        queues.append(queue)

        dlq = Queue(
            dlq_routing_key,
            exchange=dlx_exchange,
            routing_key=dlq_routing_key,
            durable=True,
        )
        queues.append(dlq)

        task_name = f"pool_worker.send_email_{name}"
        task_routes[task_name] = {"queue": queue_name}

        @celery.task(bind=True, name=task_name)
        def send_email_task(self, data):
            return send_email(self, data, database=name)

    celery.conf.update(
        task_serializer="json",
        accept_content=["json"],
        result_serializer="json",
        task_queues=queues,
        task_routes=task_routes,
    )

    return celery


celery_app = make_celery()
