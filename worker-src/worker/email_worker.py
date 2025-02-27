import json

import pika


class EmailWorker:
    def __init__(self, queue_name):
        self.queue_name = queue_name
        self.connection = pika.BlockingConnection(pika.ConnectionParameters("localhost"))
        self.channel = self.connection.channel()
        self.channel.queue_declare(queue=queue_name)

    def callback(self, ch, method, properties, body):
        job = json.loads(body)
        sender = job.get("sender")
        pool = job.get("pool")
        self.process_job(sender, pool)

    def process_job(self, sender, pool):
        print(f"Processing email job from sender: {sender} in pool: {pool}")

    def start_consuming(self):
        self.channel.basic_consume(queue=self.queue_name, on_message_callback=self.callback, auto_ack=True)
        print(f"[*] Waiting for messages in {self.queue_name}. To exit press CTRL+C")
        self.channel.start_consuming()


if __name__ == "__main__":
    # Example usage for different pools
    pools = ["pool1", "pool2", "pool3", "pool4"]
    for pool in pools:
        worker = EmailWorker(f"email_queue_{pool}")
        worker.start_consuming()
