import cv2
import numpy as np
from matplotlib import pyplot as plt

img = cv2.imread('drive/MyDrive/images/images_Lena.jpeg')
img = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
print(img.shape)
f = np.fft.fft2(img) #do the fourier transform
fshift1=np.fft.fftshift(f)#shiftthezerotothecenter
#spectrumimage
magnitude_spectrum = 20 * np.log(np.abs(fshift1))
f_ishift=np.fft.ifftshift(fshift1)#inverseshift
img_back=np.fft.ifft2(f_ishift)#inversefouriertransform
img_back=np.abs(img_back)
plt.subplot(121),plt.imshow(img_back,cmap='gray')
plt.title('Input Image'),plt.xticks([]),plt.yticks([])
plt.subplot(122),plt.imshow(magnitude_spectrum,cmap='gray')
plt.title('Magnitude Spectrum'),plt.xticks([]),plt.yticks([])
plt.show()