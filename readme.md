# Pigment
[Online Demo](https://fdietze.github.io/pigment#AAACAQQDAAAAZmaOQo9L2EKAEvE%2FAwAAAAAANkKwoXdCFHMtQAMAAACamaNChMTiQuhKOz8DAAAAAACZQrDD1EI%2FlZ5AAwEDAAAAZmbmQdC6vkJreps%2FAAAA)

![demo](demo.gif)

# Motivation in German
Die klassischen Farbwähler basieren auf dem RGB/HSV-Farbmodell und erlauben meistens nur eine Farbe auszuwählen. Eine Farbfläche über RGB/HSV, hat jedoch keine verlässliche Bedeutung für den Menschen. Z.b. ist Lila im HSV-Modell immer dunkler als Gelb mit gleicher Helligkeit und Sättigung. Das Lab-Farbmodell wurde entwickelt, um mit der euklidischen Distanz im Farbraum die vom Meschen wahrgenommene Ähnlichkeit wiederzuspiegeln. Das wissen über diesen Farbraum ist aber unter Entwicklern kaum verbreitet und etwas komplizierter zu programmieren.

**Pigment** lässt den Benutzer mit mehreren Farben gleichzeitig im Lab/Lch Farbmodell arbeiten und stellt die Wahrgenommenen Farbdistanzen nach CIEDE2000 in echtzeit grafisch dar.

Geplant sind weitere Visualisierungen der Farbpalette und die integration von Optimierungs-Algorithmen um beliebige Farb-Eigenschaften in einer Palette zu optimieren.
