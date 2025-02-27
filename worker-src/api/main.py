from flask import Flask, jsonify

app = Flask(__name__)


@app.route("/bounces", methods=["GET"])
def get_bounces():
    # Implement logic to get bounce information
    return jsonify({"message": "Bounce information"})


if __name__ == "__main__":
    app.run(debug=True)
