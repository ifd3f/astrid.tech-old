import markdown
from lxml import etree


def get_markdown_links(md_content):
    html = markdown.markdown(md_content)
    doc = etree.fromstring(html)
    for link in doc.xpath('//a'):
        yield link.get('href')
