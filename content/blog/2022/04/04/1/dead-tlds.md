---
title: What happens to TLDs when their country stops existing?
date: 2022-04-04 20:00:07-07:00
ordinal: 1
tags:
  - research
  - history
  - internet
  - /projects/qr-tattoo
---

**EDIT Sun May 15 10:38:34 PM PDT 2022:**
[Hello Hacker News!](https://news.ycombinator.com/item?id=31390600) I've made
some corrections to this post after
[Keith Winstein](https://cs.stanford.edu/~keithw/) pointed out a small
historical inaccuracy. Turns out, ICANN did not exist until
[it was created by the Clinton admin on September 18, 1998](https://en.wikipedia.org/wiki/ICANN#History),
at which point IANA was merged into ICANN. So, most of the 90's decisions around
TLD's were made by Jon Postel and Joyce K. Reynolds at the USC Information
Sciences Institute, which ran IANA. I have changed this article to reflect this.

Additionally, many have pointed out that there are other examples where a
country has disappeared, such as with Netherlands Antilles. I will admit that I
probably spent 2 hours at most on researching and writing this article, so I
didn't get to look at every ccTLD. There are lots of other ccTLDs that are not
actually countries, but exist anyways (such as .cc, .cx being islands but also
territories of Australia).

jasonjei pointed out that `.hk` is in an interesting political situation. I did
some quick digging and it seems that `.hk` was established in 1990, and it is
managed by the [HKIRC](https://news.ycombinator.com/item?id=31390600), which is
"the only organization endorsed by the Hong Kong Government to undertake the
administration of 'hk' domain names,"
[according to Wikipedia](https://en.wikipedia.org/wiki/.hk). Despite what the 五
毛 s say, the Hong Kong Government is completely at the mercy of the Chinese
government, so if China wanted to get rid of `.hk` right now, all it would take
is telling their puppets to do it, and within the next decade, `.hk` will be
gone.

However, in 2010, another ccTLD was launched, `.香港` (Hong Kong in English and
[`.xn--j6w193g` in punycode](https://en.wikipedia.org/wiki/Punycode)). At first,
it might seem that if they're going to add more domains, they're probably in
favor of keeping `.hk`. However, to put this in historical context, Hong Kong
was transfered in 1997, and during the 2000s,
[the PRC did attempt to tighten its grip](https://en.wikipedia.org/wiki/2000s_in_Hong_Kong),
although relatively unsuccessfully compared to what has happened recently. So,
this ccTLD was created in a Hong Kong that had much more autonomy. Basically, I
have no idea, go ask someone who actually knows about east Asian geopolitics.

---

When I was deciding on what TLD to get for my [QR tattoo](/projects/qr-tattoo),
one of my first thoughts was to just use a subdomain of `aay.tw`. Makes sense;
it's short, and I don't have to pay any extra money.

However, someone at a
[Homebrew Website Club](https://indieweb.org/Homebrew_Website_Club) meeting
brought up a good point -- can I trust that `.tw` will be around for the next
few decades? After all, `.tw` is Taiwan's domain, and if you know anything about
contemporary geopolitics, there's a lot of variables involved here. So, I ended
up being safe and just picking out a new `.org` because those will only go away
when ICANN goes away.

Still, this is an interesting thing to think about. Suppose China ends up
invading and successfully annexing Taiwan; what would happen to the
[2.7 million registered domains under `.tw`](https://domainnamestat.com/statistics/tld/tw-TLD_ID-1285)?
Or, suppose Russia, for some reason, ends up annexing Ukraine (I really think
this is nearly impossible, but hey, it's just a thought experiment). What will
happen to the
[nearly 700 thousand domains under `.ua`](https://domaintyper.com/domain-names/top-level-domains/ccTLD/ua-domain)?

In general, if a country stops existing, what happens to its TLD?

## Historical precedent

Note that this section is mostly sourced from Wikipedia articles so take what
you will of that.

### The collapse of the Soviet Union

Back in 1989, the Eastern Bloc countries were assigned `.su` (Soviet Union),
`.pl` (Poland), `.cs` (Czechoslovakia), `.yu` (Yugoslavia) and `.dd` (East
Germany). domain. 15 months later, in 1991, the Soviet Union collapsed, and it
seems that IANA had to deal with the fallout of that.[^su]

This situation is not the same as the one with Taiwan or Ukraine; the Soviet
Union did not collapse because it was invaded, but because of internal political
failures that are out of scope for this article. It is still an example of a
state ceasing to exist, so there might be some historical precedent there.

[^su]: https://en.wikipedia.org/wiki/.su

#### `.pl`

Although the Polish People's Republic ended, its successor, the Republic of
Poland, continued existing afterwards. In other words, there was a different
state, but it was still the same country. Thus, it inherited `.pl`.

#### `.su`

The Union of Soviet Sovialist Republics collapsed, and its constituent republics
broken off of it (i.e. Ukraine, the Baltic States) got their own domains. IANA
introduced `.ru` in 1994 to phase out `.su`, but the Russian government nad
internet users wanted to keep it, so IANA, and eventually ICANN, let it happen.
This repeated several times up until the present day, where

It seems that ICANN wants to terminate it, IANA states that it's being phased
out, but Russia still wants to keep it, and there's still 100,000 domains
registered with it, so it seems to be in a gray area. Apparently, because of
this grayness and lax and outdated terms of use, it's host to lots of cool sites
such as white supremacist site Daily Stormer (to escape deplatforming on
literally everywhere else), cybercrime activities, the pro-Putin youth movement
Nashi up until 2019, the Dontesk People's Republic, and more.

#### `.dd`

`.dd` was only ever used internally by some East German universities, so it was
just terminated. East and West Germany reunited only a year after `.dd` was
introduced, and West Germany already had `.de`, so they just used that.

#### `.yu`

When the Socialist Federal Republic of Yugoslavia dissolved, some of its
constituent states broke off and got their own TLDs, like Slovenia and Croatia.
However, Serbia and Montenegro came together to form the Federal Republic of
Yugoslavia, and they kept the `.yu` domain. But then, they named themselves
Serbia and Montenegro in 2003, and they finally dissolved in 2006 and each got
their own TLDs. `.yu` ended up being put on a transition period before being
terminated in 2010. All of the `.yu` sites were simply wiped out.

#### `.cs`

Czechoslovakia was split in 1993, and `.cs` was deleted in 1995. There were
2,300 hosts on `.cs` before its termination.

### Other change of governments

In Afghanistan, Libya, and Iraq, they changed their government, but control over
their respective TLDs (`.af`, `.ly`, `.iq`) were always given to the new
government.

On an interesting side note, although Iraq's TLD was introduced in 1997, it was
in limbo for a few years because the delegated manager was imprisoned in Texas
for a while, until ICANN redelegated it in 2005.

### Summary

| TLD   | Country's fate                         | TLD's fate                |
| ----- | -------------------------------------- | ------------------------- |
| `.pl` | Transfer of power                      | Alive                     |
| `.su` | Broken up/Transfer of power for Russia | Alive, but kinda in limbo |
| `.dd` | Unified with/into West Germany         | Dead                      |
| `.cs` | Broken up                              | Dead                      |
| `.yu` | Broken up                              | Dead                      |
| `.af` | Transfer of power                      | Alive                     |
| `.ly` | Transfer of power                      | Alive                     |
| `.iq` | Transfer of power                      | Alive                     |

This list is full of countries breaking up or changing governments, but no
annexations. I guess you could sorta view East Germany as being annexed into
West Germany, but at the same time, almost no one used `.dd` so there was also
that going for it. There isn't too much historical precedent to look at.

## The ICANN agreements

ICANN does have
[a publicly-available list of its ccTLD agreements that it has made with all countries with ccTLDs](https://www.icann.org/resources/pages/cctlds/cctlds-en).
This includes
[Taiwan's](https://www.icann.org/resources/unthemed-pages/sponsorship-agmt-2003-03-26-en)
and
[Ukraine's](https://www.icann.org/en/system/files/files/ua-icann-af-10feb15-en.pdf).

Based on my (admittedly cursory) scan of the Taiwan one, there's a lot of
mention about "the territory of the Governmental Authority," which implies that
if the Governmental Authority no longer has territory, things might become
somewhat hairy.

## Conclusion

At this point, I'm too lazy to look deeper into the ICANN agreements. Too much
legalese. I guess if there's a takeaway from this research, it's that a ccTLD
could stop existing after the country stops existing.

## EDIT, a month after writing this article: The part where I wildly speculate on geopolitics and talk about situations I don't completely understand

From my understanding of the political situation, China has been repeatedly
affirming that the country of Taiwan is not a country, but is actually a rogue
province and fully part of China. Therefore, if China were to take over Taiwan,
then getting rid of the .tw domain would be one of the many symbolic ways it
would strip Taiwan of its status as a sovereign nation, not to mention all the
other material ways they would do so. It would probably not be immediate,
because in the past, the process of removing domains has involved transition
periods.

Additionally, the situation with the special warcrime operation in Ukraine has
progressed since I first published this post. It has become extremely obvious
that Russia doesn't have the power to annex Ukraine, no matter how hard they
try. However, if they had annexed Ukraine, then taking into account their
framing of Ukraine as """rightful Russian territory,""" they would probably get
rid of `.ua` too.
