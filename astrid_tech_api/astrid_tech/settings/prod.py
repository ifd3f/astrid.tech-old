import os

from .base import *

DEBUG = False
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
            "level": "INFO",
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
            "handlers": ["console"],
            "level": "DEBUG",
            'propagate': True,
        }
    }
}

configure_structlog()
