from PIL import Image

TRANSPARENT = (0, 0, 0, 0)


def dark_to_alpha(im):
    w, h = im.size
    for y in range(h):
        for x in range(w):
            r, g, b, a = im.getpixel((x, y))
            if (r + g + b) < 3 * 60:
                im.putpixel((x, y), TRANSPARENT)


def shooting_frames():
    orig = Image.open("assets/shootingstar.gif")
    for i in range(0, orig.n_frames - 1):
        orig.seek(i)
        conv = orig.convert('RGBA')
        w, h = conv.size
        conv = conv.crop((w * 0.2, 0, w * 0.8, h))
        dark_to_alpha(conv)
        yield conv


def twinkling_frames():
    im = Image.open("assets/stars.webp")
    for i in range(0, im.n_frames):
        im.seek(i)
        yield im


def words_img():
    # Comes from https://cooltext.com/Logo-Design-Princess
    im = Image.open("assets/text.png")
    return im


def rendered_frames():
    words = words_img()
    frames = zip(shooting_frames(), twinkling_frames())
    for shooting, twinkling in frames:
        background = twinkling.crop((0, 10, 88, 50)).resize((88, 31))
        shooting2 = shooting.resize((88, 31))
        background.paste(shooting2, (0, 0), shooting2)
        background.paste(words, (-10, 0), words)
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
