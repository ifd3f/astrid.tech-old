FROM python:3.9-slim
ENV PYTHONUNBUFFERED=1 DJANGO_SETTINGS_MODULE=astrid_tech.settings.prod
WORKDIR /app

COPY requirements.txt .
RUN pip install -r requirements.txt

COPY . .
