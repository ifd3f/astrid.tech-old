FROM python:3.9-slim
RUN pip install pipenv

ARG INSTALL_DEV_DEPENDENCIES
ENV PYTHONUNBUFFERED=1 DJANGO_SETTINGS_MODULE=astrid_tech.settings_prod

WORKDIR /app

COPY Pipfile Pipfile.lock ./
RUN pipenv install

COPY . .
