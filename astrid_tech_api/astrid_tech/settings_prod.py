import os

from .settings_base import *

DEBUG = False

ALLOWED_HOSTS = [
    "localhost",
    "::1",
    "127.0.0.1"
]

if os.getenv('ASTRID_TECH_API_HOST') is not None:
    ALLOWED_HOSTS.append(os.getenv('ASTRID_TECH_API_HOST'))

SECRET_KEY = os.getenv('SECRET_KEY')

DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.postgresql_psycopg2',
        'NAME': os.getenv('POSTGRES_DB'),
        'USER': os.getenv('POSTGRES_USER'),
        'PASSWORD': os.getenv('POSTGRES_PASSWORD'),
        'HOST': os.getenv('POSTGRES_HOST'),
        'PORT': os.getenv('POSTGRES_PORT'),
    }
}

pre_chain += (add_service_name('astrid_tech_api'),)

LOGGING = {
    "version": 1,
    "disable_existing_loggers": False,
    "formatters": {
        "json_formatter": {
            "()": structlog.stdlib.ProcessorFormatter,
            "processor": structlog.processors.JSONRenderer(),
            "foreign_pre_chain": pre_chain,
        },
        "plain_console": {
            "()": structlog.stdlib.ProcessorFormatter,
            "processor": structlog.dev.ConsoleRenderer(),
            "foreign_pre_chain": pre_chain,
        },
    },
    "handlers": {
        "console": {
            "class": "logging.StreamHandler",
            "formatter": "plain_console",
            "level": "DEBUG",
        },
        "file": {
            "class": "logging.handlers.TimedRotatingFileHandler",
            'when': 'D',
            "filename": "logs/output.log",
            "formatter": "json_formatter"
        },
        # "logstash": {
        #     "class": "logstash.LogstashHandler",
        #     "host": os.getenv('LOGSTASH_HOST'),
        #     "port": os.getenv('LOGSTASH_PORT'),
        #     "version": 1,
        #     "level": "DEBUG",
        # }
    },
    "loggers": {
        "": {
            "handlers": ["console", "file"],
            "level": "DEBUG",
            'propagate': True,
        }
    }
}

configure_structlog()
