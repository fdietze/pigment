# Pigment
[Online Demo](https://fdietze.github.io/pigment#%7B%22colorScheme%22%3A%7B%221%22%3A%5B%5B72%2C103.31%2C1.83%5D%2C%5B48.5%2C53.06%2C2.51%5D%2C%5B81.7%2C110.71%2C0.73%5D%2C%5B73.5%2C109.32%2C4.95%5D%5D%2C%223%22%3A%5B%5B31.6%2C93.72%2C1.22%5D%5D%7D%2C%22locked%22%3A%5B%5D%2C%22fitnessFunction%22%3A%7B%22terms%22%3A%5B%5D%7D%7D)

![demo](demo.gif)

# Motivation in German
Die klassischen Farbwähler basieren auf dem RGB/HSV-Farbmodell und erlauben meistens nur eine Farbe auszuwählen. Eine Farbfläche über RGB/HSV, hat jedoch keine verlässliche Bedeutung für den Menschen. Z.b. ist Lila im HSV-Modell immer dunkler als Gelb mit gleicher Helligkeit und Sättigung. Das Lab-Farbmodell wurde entwickelt, um mit der euklidischen Distanz im Farbraum die vom Meschen wahrgenommene Ähnlichkeit wiederzuspiegeln. Das wissen über diesen Farbraum ist aber unter Entwicklern kaum verbreitet und etwas komplizierter zu programmieren.

**Pigment** lässt den Benutzer mit mehreren Farben gleichzeitig im Lab/Lch Farbmodell arbeiten und stellt die Wahrgenommenen Farbdistanzen nach CIEDE2000 in echtzeit grafisch dar.

Geplant sind weitere Visualisierungen der Farbpalette und die integration von Optimierungs-Algorithmen um beliebige Farb-Eigenschaften in einer Palette zu optimieren.
