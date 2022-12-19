from google.colab.patches import cv2_imshow

# Importacao das bibliotecas

import cv2

# Ler a imagem com a funcao imread()

imagem = cv2.imread('drive/My Drive/images/messi5.jpg', cv2.IMREAD_UNCHANGED)



print('Largura: ', imagem.shape[1]) #largura da imagem

print('Altura: ', imagem.shape[0]) #altura da imagem

print('Canais de Cor: ', imagem.shape[2])

# Mostra a imagem 

cv2_imshow(imagem)

imagem2 = cv2.cvtColor(imagem, cv2.COLOR_BGR2GRAY)

print('Largura: ', imagem2.shape[1]) #largura da imagem

print('Altura: ', imagem2.shape[0]) #altura da imagem

cv2_imshow(imagem2)