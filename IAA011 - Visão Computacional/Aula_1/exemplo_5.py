from google.colab.patches import cv2_imshow
import cv2

img = cv2.imread('drive/My Drive/images/messi5.jpg')

cv2_imshow(img)
gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
#cv2_imshow(gray)
hsv = cv2.cvtColor(img, cv2.COLOR_BGR2HSV)

lab = cv2.cvtColor(img, cv2.COLOR_BGR2LAB)
cv2_imshow(lab)
#(h, s, v) = cv2.split(lab)
#cv2_imshowcoração
#cv2_imshow(s)
#cv2_imshow(v)