import json
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText

from pool_worker.accounts import find_by_username, find_default


def send_email(self, json_data, database):
    data = json.loads(json_data).get("email")
    sender = data["sender"]
    recipient = data["recipient"]
    subject = data["subject"]
    text_content = data["text"]
    html_content = data["html"]
    cc = data.get("cc", [])
    bcc = data.get("bcc", [])

    msg = MIMEMultipart("alternative")
    msg["From"] = sender
    msg["To"] = recipient
    msg["Subject"] = subject
    msg["Cc"] = ", ".join(cc)
    msg["Bcc"] = ", ".join(bcc)

    msg.attach(MIMEText(text_content, "plain"))
    msg.attach(MIMEText(html_content, "html"))

    recipients = [recipient] + cc + bcc

    account = find_by_username(database, sender)
    if not account:
        account = find_default(database)
        if account:
            msg["From"] = account.smtp_account.username

    if not account:
        raise ValueError(f"Account not found for sender: {sender}")

    with account as connection:
        connection.sendmail(sender, recipients, msg.as_string())

    return f"Email sent to {recipient} with subject '{subject}'"
