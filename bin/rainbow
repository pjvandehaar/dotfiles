#!/usr/bin/env python3

## setup: pip3 install pillow

from PIL import Image, ImageDraw, ImageFont, ImageGrab
import time, subprocess, re, urllib.request, sys, pyperclip


def download_img(url):
    headers = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36'}
    request = urllib.request.Request(url, headers=headers)
    with urllib.request.urlopen(request) as resp:
        return Image.open(resp)
URL_regex = re.compile(
    r'^(?:http|ftp)s?://' # http:// or https://
    r'(?:(?:[A-Z0-9](?:[A-Z0-9-]{0,61}[A-Z0-9])?\.)+(?:[A-Z]{2,6}\.?|[A-Z0-9-]{2,}\.?)|' #domain...
    r'localhost|' #localhost...
    r'\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})' # ...or ip
    r'(?::\d+)?' # optional port
    r'(?:/?|[/?]\S+)$', re.IGNORECASE)
IMG_regex = re.compile(r'\.(?:png|gif|jpg|jpeg|tif|bmp|webp)$', re.IGNORECASE)
def get_img():
    if len(sys.argv)>1 and sys.argv[1]:
        if URL_regex.match(sys.argv[1]) and IMG_regex.search(sys.argv[1]):
            return download_img(sys.argv[1])
        return Image.open(sys.argv[1])
    clipboard_contents = pyperclip.paste()
    if URL_regex.match(clipboard_contents) and IMG_regex.search(clipboard_contents):
        return download_img(clipboard_contents)
    try:
        subprocess.check_output(['pngpaste', '/tmp/image.png'], stderr=subprocess.DEVNULL)
        return Image.open('/tmp/image.png') # TODO: use BytesIO
    except subprocess.CalledProcessError: pass
    time.sleep(1) # give time to hide terminal
    return ImageGrab.grab()


def overlay_text(base, text):
    size = (33, 7)
    txt = Image.new('RGB', size, (255,255,255))
    fnt = ImageFont.load_default()
    d = ImageDraw.Draw(txt)
    d.text((0,-3), text, font=fnt, fill=(0,0,0))
    final_dim = (base.size[0]//8, int(base.size[0]/8*size[1]/size[0]))
    txt = txt.resize(final_dim)
    base.paste(txt)

screenshot = get_img()
bands = screenshot.split()

def make_image(channel_order):
    img = Image.merge('RGB', [bands[i] for i in channel_order])
    overlay_text(img, ' '.join('RGB'[i] for i in channel_order))
    return img
orders, duration = [(1,2,0), (2,0,1)], 1000
images = [make_image(order) for order in orders]

def ql_image(img):
    img.save('/tmp/x.png')
    subprocess.call('qlmanage -p /tmp/x.png'.split(), stderr=subprocess.DEVNULL, stdout=subprocess.DEVNULL)
for img in images: ql_image(img)
#for img in images: img.show()
