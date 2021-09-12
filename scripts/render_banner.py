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
    with Image.open("assets/shootingstar.gif") as orig:
        for i in range(1, orig.n_frames):
            orig.seek(i)
            with orig.convert('RGBA') as conv:
                dark_to_alpha(conv)
                yield conv


def twinkling_frames():
    with Image.open("assets/stars.webp") as im:
        for i in range(0, im.n_frames):
            im.seek(i)
            yield im


def main():
    frames = zip(shooting_frames(), twinkling_frames())
    for shooting, twinkling in frames:
        shooting.show()
        break


if __name__ == '__main__':
    main()
