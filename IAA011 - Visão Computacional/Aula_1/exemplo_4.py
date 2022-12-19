from google.colab.patches import cv2_imshow
import cv2
import numpy as np
from matplotlib import pyplot as plt

img = cv2.imread('drive/My Drive/images/Lena.jpeg',0)
cv2_imshow(img)
cv2.waitKey(0)

hist,bins = np.histogram(img.flatten(),256,[0,256])

cdf = hist.cumsum()
cdf_normalized = cdf * hist.max()/ cdf.max()

plt.plot(cdf_normalized, color = 'b')
plt.hist(img.flatten(),256,[0,256], color = 'r')
plt.xlim([0,256])
plt.legend(('Somatorio','histograma'), loc = 'upper left')
plt.show()

h_eq = cv2.equalizeHist(img)
cv2_imshow(h_eq)

hist2,bins2 = np.histogram(h_eq.flatten(),256,[0,256])

cdf2 = hist2.cumsum()
cdf2_normalized = cdf2 * hist2.max()/ cdf2.max()

plt.plot(cdf2_normalized, color = 'b')
plt.hist(h_eq.flatten(),256,[0,256], color = 'r')
plt.xlim([0,256])
plt.legend(('Somatorio','histograma'), loc = 'upper left')
plt.show()