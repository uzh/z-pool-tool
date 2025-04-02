from __future__ import annotations

import logging
import os
import smtplib
import time
from collections import defaultdict
from datetime import datetime
from typing import List, Optional, Self

from sqlalchemy import Column, create_engine
from sqlalchemy.orm import sessionmaker

from pool_worker.accounts.models import SMTPAccount, Tenant

accounts_cache: defaultdict[str, List[Account]] = defaultdict(list)
connections_cache = {}


class Account:
    def __init__(self: Self, database_label: str, smtp_account: SMTPAccount):
        self.database_label = database_label
        self.smtp_account = smtp_account
        self.label = self.smtp_account.label
        self.connection = None
        self.last_used = None

    def connect(self: Self) -> Self:
        """
        Establish an SMTP connection and update the last used timestamp.
        """
        if not self.connection:
            self.connection = smtplib.SMTP(self.smtp_account.server, self.smtp_account.port)
            if self.smtp_account.use_starttls:
                self.connection.starttls()

            if self.smtp_account.mechanism == "PLAIN":
                if self.smtp_account.username or self.smtp_account.password:
                    raise ValueError(
                        f"Logging in to {self.smtp_account.server} as {self.smtp_account.username} with 'PLAIN' mechanism is not supported (eighter username or password is set)."
                    )
            else:
                self.connection.login(self.smtp_account.username, self.smtp_account.password)

        self.last_used = datetime.now()
        return self

    def close_connection(self: Self) -> Self:
        """
        Close the SMTP connection.
        """
        if self.connection:
            self.connection.quit()
            self.connection = None
            self.last_used = None

        return self

    def __enter__(self: Self) -> smtplib.SMTP:
        """
        Enter the runtime context and return the connection.
        Reconnect only if the connection is broken.
        """
        if self.smtp_account.server in connections_cache:
            cached_account = connections_cache[self.smtp_account.server]
            self.connection = cached_account.connection
            self.last_used = cached_account.last_used

            try:
                self.connection.noop()
            except smtplib.SMTPServerDisconnected:
                self.close_connection()
                self.connect()
                connections_cache[self.smtp_account.server] = self
        else:
            self.connect()
            connections_cache[self.smtp_account.server] = self

        return self.connection

    def __exit__(self: Self, exc_type, exc_value, traceback) -> None:
        """
        Close connection on
        """
        match exc_type:
            case None:
                self.last_used = datetime.now()
            case smtplib.SMTPAuthenticationError | smtplib.SMTPConnectError | smtplib.SMTPServerDisconnected:
                self.close_connection()

                if self.smtp_account.server in connections_cache:
                    del connections_cache[self.smtp_account.server]
                logging.error(f"SMTP error occurred: {exc_value}")
            case _:
                logging.error(f"{exc_type}: {exc_value},\n traceback: {traceback}")

    def get_connection(self: Self, delays=[5, 15, 30, 60, 120]) -> SMTPAccount:
        """
        Establish and cache an SMTP connection.
        """
        account = self.smtp_account
        retries = len(delays)

        if account.server in connections_cache:
            return connections_cache[account.server]

        for attempt in range(retries):
            try:
                return self.connect().smtp_account
            except Exception as e:
                if attempt < retries - 1:
                    delay = delays[attempt]
                    logging.info(
                        f"Retrying connection to {account.server} in {delay} seconds (attempt {attempt + 1}/{retries})..."
                    )
                    time.sleep(delay)
                else:
                    logging.error(f"Failed to connect to {account.server} after {retries} attempts.")
                    raise e


def load_tenants() -> list[tuple[Column[str], str]]:
    Session = sessionmaker(bind=create_engine(os.getenv("DATABASE_URL")))
    session = Session()

    try:
        tenants = session.query(Tenant).all()
        return [(tenant.label, tenant.url) for tenant in tenants]
    finally:
        session.close()


def load_accounts(databae_url: str) -> List[SMTPAccount]:
    Session = sessionmaker(bind=create_engine(databae_url))
    session = Session()

    try:
        return session.query(SMTPAccount).all()
    finally:
        session.close()


def load_account_by_something(tenant_name: str, account_label: str, where_clause) -> Account | None:
    """
    Load an account by tenant name and account label.
    """
    if tenant_name in accounts_cache and account_label in accounts_cache[tenant_name]:
        return accounts_cache[tenant_name][account_label]

    RootSession = sessionmaker(bind=create_engine(os.getenv("DATABASE_URL")))
    root_session = RootSession()

    try:
        tenant: Optional[Tenant] = root_session.query(Tenant).where(Tenant.label == tenant_name).get()
        if not tenant:
            return None
    finally:
        root_session.close()

    TenantSession = sessionmaker(bind=create_engine(tenant.url))
    session = TenantSession()

    try:
        smtp_account: Optional[SMTPAccount] = session.query(SMTPAccount).where(where_clause).get()
    finally:
        session.close()

    if smtp_account:
        account = Account(database_url=tenant.url, smtp_account=smtp_account)
    else:
        return None

    if tenant_name not in accounts_cache:
        accounts_cache[tenant_name] = [account]
    elif account and account not in accounts_cache[tenant_name]:
        accounts_cache[tenant_name].append(account)

    return account


def load_account_by_label(tenant_name: str, account_label: str) -> Account | None:
    load_account_by_something(tenant_name, account_label, SMTPAccount.label == account_label)


def load_account_by_username(tenant_name: str, account_username: str) -> Account | None:
    load_account_by_something(tenant_name, account_username, SMTPAccount.label == account_username)


def find(tenant_name: str, account_label: str) -> Account | None:
    """
    Find an account by tenant name and account label.
    """
    if tenant_name not in accounts_cache:
        return load_account_by_label(tenant_name, account_label)

    for account in accounts_cache[tenant_name]:
        if account.smtp_account.label == account_label:
            return account

    return None


def find_by_username(tenant_name: str, username: str) -> Account | None:
    """
    Find an account by tenant name and username.
    """
    if tenant_name not in accounts_cache:
        return load_account_by_username(tenant_name, username)

    for account in accounts_cache[tenant_name]:
        if account.smtp_account.username == username:
            return account

    return None


def populate_accounts_cache() -> List[str]:
    tenant_db_urls = load_tenants()
    queue_databases = [("root", os.getenv("DATABASE_URL")), *tenant_db_urls]

    for name, database_url in queue_databases:
        try:
            smtp_accounts = load_accounts(database_url)
            accounts_cache[name] = list(
                map(lambda acc: Account(database_label=database_url, smtp_account=acc), smtp_accounts)
            )
        except Exception as e:
            logging.error(f"Error loading accounts for tenant {name}: {e}")
            continue

    return [name for (name, _) in queue_databases]
