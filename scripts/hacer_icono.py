# Genera los iconos de la app (www/icon-*.png, apple-touch-icon.png, favicon-32.png):
#   python3 scripts/hacer_icono.py www/logo.jpg <fuente Playfair Display Bold .ttf/.woff> www
# Fondo navy del dashboard, logo de Vizcaya en tarjeta blanca, linea de pulso y "Pulso".
import sys
from PIL import Image, ImageDraw, ImageFont
logo_path, fuente, out = sys.argv[1], sys.argv[2], sys.argv[3]
N = 1024; NAVY = (27, 45, 91); CELESTE = (127, 179, 230)
im = Image.new("RGB", (N, N), NAVY); d = ImageDraw.Draw(im)
# tarjeta blanca con el logo de Vizcaya (dentro de la zona segura 80%)
logo = Image.open(logo_path).convert("RGB")
lw = 500; lh = round(logo.height * lw / logo.width); logo = logo.resize((lw, lh), Image.LANCZOS)
cw, ch = lw + 70, lh + 50; cx, cy = (N - cw) // 2, 140
d.rounded_rectangle([cx, cy, cx + cw, cy + ch], radius=28, fill=(255, 255, 255))
im.paste(logo, (cx + 35, cy + 25))
# linea de pulso
y0 = 590; xs = [150, 360, 410, 455, 505, 560, 610, 874]; ys = [y0, y0, y0 - 60, y0 + 70, y0 - 110, y0 + 35, y0, y0]
d.line(list(zip(xs, ys)), fill=CELESTE, width=22, joint="curve")
for x, y in [(xs[0], ys[0]), (xs[-1], ys[-1])]: d.ellipse([x - 11, y - 11, x + 11, y + 11], fill=CELESTE)
# "Pulso"
f = ImageFont.truetype(fuente, 230); txt = "Pulso"
b = d.textbbox((0, 0), txt, font=f); tw = b[2] - b[0]
d.text(((N - tw) // 2 - b[0], 655), txt, font=f, fill=(255, 255, 255))
im.save(out + "/icon-1024.png")
for s, name in [(512, "icon-512.png"), (192, "icon-192.png"), (180, "apple-touch-icon.png"), (32, "favicon-32.png")]:
    im.resize((s, s), Image.LANCZOS).save(out + "/" + name)
print("ok")
