from PIL import Image

TRANSPARENT = (0, 0, 0, 0)


def dark_to_alpha(im):
    w, h = im.size
    for y in range(h):
        for x in range(w):
            r, g, b, a = im.getpixel((x, y))
            if (r + g + b) < 3 * 60:
                im.putpixel((x, y), TRANSPARENT)


def set_alpha(im, a):
    w, h = im.size
    for y in range(h):
        for x in range(w):
            r, g, b, a_orig = im.getpixel((x, y))
            if a_orig > 0:
                im.putpixel((x, y), (r, g, b, a))


def shooting_frames():
    orig = Image.open("assets/shootingstar.gif")
    for i in range(0, orig.n_frames - 1):
        orig.seek(i)
        conv = orig.convert('RGBA')
        w, h = conv.size
        conv = conv.crop((w * 0.2, 0, w * 0.8, h))
        dark_to_alpha(conv)
        set_alpha(conv, 80)
        yield conv


def twinkling_frames():
    im = Image.open("assets/stars.webp")
    for i in range(0, im.n_frames):
        im.seek(i)
        yield im


def astral_frames():
    # from https://cooltext.com/Logo-Design-Glitter
    im = Image.open("assets/astral.gif")
    for i in range(0, im.n_frames):
        im.seek(i)
        conv = im.convert('RGBA')
        conv = conv.resize((80, 20))
        dark_to_alpha(conv)
        yield conv


def domain_frames():
    # Comes from https://cooltext.com/Logo-Design-Blinkie
    im = Image.open("assets/domain.gif")
    for i in range(0, im.n_frames):
        im.seek(i)
        conv = im.convert('RGBA')
        conv = conv.resize((80, 30))
        dark_to_alpha(conv)
        yield conv


def rendered_frames():
    frames = zip(shooting_frames(), twinkling_frames(), astral_frames(), domain_frames())
    for shooting, twinkling, astral, domain in frames:
        background = twinkling.crop((0, 10, 100, 50)).resize((88, 31))
        shooting2 = shooting.resize((88, 31))
        background.paste(shooting2, (0, 0), shooting2)
        background.paste(astral, (-4, 0), astral)
        background.paste(domain, (14, 10), domain)
        yield background.convert("RGB")


def main():
    frames = list(rendered_frames())
    frames[0].save(
        'banner-88x31.gif',
        format='GIF',
        append_images=frames[1:],
        save_all=True,
        duration=300,
        loop=0
    )


if __name__ == '__main__':
    main()
