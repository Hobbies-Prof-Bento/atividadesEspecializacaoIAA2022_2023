from google.colab import drive
drive.mount('/drive')
from google.colab.patches import cv2_imshow
from matplotlib import pyplot as plt
import cv2
import numpy as np

img = cv2.imread('/drive/MyDrive/images/DSC01816_copia.JPG')
blur = cv2.blur(img,(21,21))

hsv = cv2.cvtColor(blur, cv2.COLOR_BGR2HSV)

(h, s, v) = cv2.split(hsv)
cv2_imshow(blur)
#cv2_imshowcoração
#hs = (h*1.0/255)*(s*1.0/255)
max1 = h.max() /1.0
max2 = s.max() / 1.0
print('max: ', max1,' - max2: ', max2)
new_image = np.zeros(img.shape, img.dtype)
for y in range(0, img.shape[0]):
for x in range(0, img.shape[1]):
new_image[y, x] = np.clip(((h[y, x]/max1)*(s[y,x]/max2)*255), 0 , 255)
##new_image = cv2.convertScaleAbs(image, alpha=alpha, beta=beta)

(T, threshInv) = cv2.threshold(new_image, 90, 255,
cv2.THRESH_BINARY_INV)

cv2_imshow(new_image)
cv2_imshow(threshInv)
cv2_imshow(s)
#cv2_imshow(hs)
#gray = cv2.cvtColor(hs, cv2.COLOR_BGR2GRAY)
#cv2_imshow(canalAzul)

b = cv2.calcHist([new_image], [0], None, [256], [0, 256])
g = cv2.calcHist([h], [0], None, [256], [0, 256])
r = cv2.calcHist([s], [0], None, [256], [0, 256])

plt.figure()
plt.title("Histograma P&B")
plt.xlabel("Intensidade")
plt.ylabel("Qtde de Pixels")
plt.plot(b,"b")
#plt.plot(g,"g")
#plt.plot(r,"r")
plt.xlim([0, 256])
plt.show()
cv2.waitKey(0)