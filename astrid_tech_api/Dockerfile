FROM python:3
ENV PYTHONUNBUFFERED 1
WORKDIR /app

COPY Pipfile Pipfile.lock ./
RUN pipenv install

COPY . ./