Today was a great day. I got astrid.tech's backend deployed today!

- Found the cause of my tests not working. The reason my tests worked on SQLite
  but not Postgres was because the tests relied on deterministic primary keys.
  On a normal `TestCase`, the SQLite database uses running on in-memory
  databases getting erased every test iteration, meaning autoincrement would get
  reset all the time. Postgres, however, didn't reset autoincrement. Using
  `LiveServerTestCase` fixed the consistency issues at the cost of making it
  slightly slower.
- I set up automated testing on Docker Hub using `docker-compose.test.yml`.
- I redesigned the API to have less endpoints to manage.
- Fixing the frontend wasn't too hard, since most of the UI logic and markup was
  in place already.
- I set up LetsEncrypt with certbot. It took a very long time because I kept
  failing and hitting the rate limit. It turns out the reason I was failing was
  the usual dumb mistake: I didn't open port 80 and 443.
- Finally, after much waiting for rate limits, I got the site deployed!
