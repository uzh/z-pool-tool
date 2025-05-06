from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText

from pool_worker.accounts import find, find_default


def send_email(self, database, **kwargs):
    data = kwargs.get("email")
    if not data:
        raise KeyError("'email' kwarg is required")

    sender = data["sender"]
    recipient = data["recipient"]
    subject = data["subject"]
    text_content = data["text"]
    html_content = data["html"]
    cc = data.get("cc", [])
    bcc = data.get("bcc", [])
    reply_to = data.get("reply_to", sender)

    msg = MIMEMultipart("alternative")
    msg["From"] = sender
    msg["To"] = recipient
    msg["Subject"] = subject
    msg["Cc"] = ", ".join(cc)
    msg["Bcc"] = ", ".join(bcc)

    msg.attach(MIMEText(text_content, "plain"))
    msg.attach(MIMEText(html_content, "html"))

    recipients = [recipient] + cc + bcc

    smtp_auth_id = kwargs.get("smtp_auth_id")
    if smtp_auth_id:
        account = find(database, smtp_auth_id)
    else:
        account = find_default(database)

    if not account:
        raise ValueError(f"Account not found for sender: {sender}")

    msg["Reply-To"] = reply_to
    msg["From"] = account.smtp_account.username

    with account as connection:
        connection.sendmail(sender, recipients, msg.as_string())

    return f"Email sent to {recipient} with subject '{subject}'"
