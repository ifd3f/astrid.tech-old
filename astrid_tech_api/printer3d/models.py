from django.db.models import Model, DateTimeField, CharField, ImageField, ForeignKey, CASCADE, SET_NULL, FloatField


class Printer(Model):
    name = CharField(50)
    progress = FloatField()
    status = CharField(32)
    current = ForeignKey("PrinterImage", on_delete=SET_NULL)


class PrinterImage(Model):
    printer = ForeignKey(Printer, on_delete=CASCADE)
    timestamp = DateTimeField()
    image = ImageField()

