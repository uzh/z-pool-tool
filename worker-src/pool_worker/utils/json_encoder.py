import json


def encode_message(data):
    return json.dumps(data)


def decode_message(json_data):
    return json.loads(json_data)
