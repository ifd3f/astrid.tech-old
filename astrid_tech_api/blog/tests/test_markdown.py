import unittest

from blog.posting import get_markdown_links


class MyTestCase(unittest.TestCase):
    def test_something(self):
        # noinspection HttpUrlsUsage
        body_markdown = "This is an [inline link](http://google.com). This is a [non inline link][1]\r\n\r\n  [1]: " \
                        "http://yahoo.com "
        links = list(get_markdown_links(body_markdown))
        self.assertEqual(['http://google.com', 'http://yahoo.com'], links)


if __name__ == '__main__':
    unittest.main()
