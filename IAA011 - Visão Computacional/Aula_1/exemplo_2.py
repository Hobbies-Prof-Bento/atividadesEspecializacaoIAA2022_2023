from google.colab.patches import cv2_imshow

import cv2

import numpy as np



alpha = 2.0

beta = 0

image = cv2.imread('drive/My Drive/images/messi5.jpg')

(b, g, r) = image[10,10]

print('O pixel (10, 10) tem os valores nos canais:')

print('Vermelho:', r, 'Verde:', g, 'Azul:', b)

cv2_imshow(image)



new_image = np.zeros(image.shape, image.dtype)

#for y in range(0, image.shape[0]):

#  for x in range(0, image.shape[1]):

#    new_image[y, x] = np.clip(alpha * image[y,x] + beta, 0 , 255)

new_image = cv2.convertScaleAbs(image, alpha=alpha, beta=beta)

(b, g, r) = new_image[10,10]

print('O pixel (10, 10) tem os valores nos canais:')

print('Vermelho:', r, 'Verde:', g, 'Azul:', b)

cv2_imshow(new_image)

#cv2.waitKey(0)