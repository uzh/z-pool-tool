import base64
import hashlib
import os
import uuid

from cryptography.hazmat.backends import default_backend
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes
from dotenv import load_dotenv
from sqlalchemy import BINARY, Boolean, Column, Integer, String, TypeDecorator
from sqlalchemy.ext.declarative import declarative_base

# Load environment variables from .env file
load_dotenv()

Base = declarative_base()


def decrypt(encrypted_value: str) -> str:
    """
    Decrypts a Base64-encoded string encrypted using AES in CTR mode.

    :param encrypted_value: The Base64-encoded encrypted string.
    :return: The decrypted string.
    """
    block_size = 16
    secret = os.getenv("AES_SECRET")
    if not secret:
        raise ValueError("AES_SECRET environment variable is not set.")

    # Derive the key from the secret using SHA256
    key = hashlib.sha256(secret.encode()).digest()

    # Derive the counter (CTR nonce) from the length of the secret
    secret_length = len(secret)
    counter_value = secret_length.to_bytes(block_size, byteorder="big")

    # Decode the Base64-encoded encrypted value
    encrypted_bytes = base64.b64decode(encrypted_value)

    # Create the AES cipher in CTR mode
    cipher = Cipher(algorithms.AES(key), modes.CTR(counter_value), backend=default_backend())
    decryptor = cipher.decryptor()

    # Decrypt the value
    decrypted_bytes = decryptor.update(encrypted_bytes) + decryptor.finalize()

    return decrypted_bytes.decode("utf-8")


class Uuid4(TypeDecorator):  # pylint: disable=abstract-method,too-many-ancestors
    # sqlalchemy mysqlclient
    # https://stackoverflow.com/questions/33923914/python-sqlalchemy-binary-column-type-hex-and-unhex
    impl = BINARY

    cache_ok = True

    def __init__(self):
        self.impl.length = 16
        TypeDecorator.__init__(self, length=self.impl.length)

    def process_bind_param(self, value, dialect=None):
        match value:
            case None:
                return None
            case uuid.UUID():
                return value.bytes
            case str():
                return uuid.UUID(value).bytes
            case _:
                raise ValueError(f"value {value} is not a valid uuid.UUID")

    def process_result_value(self, value, dialect=None):
        return uuid.UUID(bytes=value) if value else None

    def is_mutable(self):
        return False

    def create():
        return str(uuid.uuid4())


class Tenant(Base):
    __tablename__ = "pool_tenant_databases"

    label = Column(String, primary_key=True)
    _url = Column("url", String, nullable=False)

    @property
    def url(self):
        """Decrypt and return the URL."""

        return decrypt(self._url)

    def __setattr__(self, key, value):
        """Prevent setting attributes to make the model read-only."""
        if hasattr(self, key):
            raise AttributeError(f"{self.__class__.__name__} is read-only.")
        super().__setattr__(key, value)


class SMTPAccount(Base):
    __tablename__ = "pool_smtp"

    uuid = Column(Uuid4, primary_key=True)
    label = Column(String, nullable=False)
    server = Column(String, nullable=False)
    port = Column(Integer, nullable=False)
    username = Column(String)
    password = Column(String)
    mechanism = Column(String, nullable=False)
    protocol = Column(String, nullable=False)
    default_account = Column(Boolean, default=False)

    def __setattr__(self, key, value):
        """Prevent setting attributes to make the model read-only."""
        if hasattr(self, key):
            raise AttributeError(f"{self.__class__.__name__} is read-only.")
        super().__setattr__(key, value)
