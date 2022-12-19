from matplotlib import pyplot as plt
import cv2
img = cv2.imread('drive/My Drive/images/home.jpg')
img = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY) #converte P&B
cv2_imshow(img)


h = cv2.calcHist([img], [0], None, [256], [0, 256])
plt.figure()
plt.title("Histograma P&B")
plt.xlabel("Intensidade")
plt.ylabel("Qtde de Pixels")
#plt.plotcoração
plt.xlim([0, 256])
plt.hist(img.ravel(),256,[0,256])
plt.show()