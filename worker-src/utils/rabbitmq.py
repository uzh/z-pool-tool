import pika


def get_connection():
    return pika.BlockingConnection(pika.ConnectionParameters("localhost"))


def declare_queue(channel, queue_name):
    channel.queue_declare(queue=queue_name)
