---
type: project

title: Hairnet
status: complete
description: A neural network that detects a person's face and hair
startDate: 2019-05-29
endDate: 2019-09-25
tags:
  - tensorflow
  - image-processing
  - python
  - data-science
  - anaconda
keywords:
  - TensorFlow
  - Python
  - Data Science
  - Computer Vision
url: null
featured: true
source: [https://github.com/Plenglin/Hair-Net]
thumbnail: ./thumbnail.png
highlights:
  - Designed and trained a convolutional neural network using Python and Tensorflow.
  - Synthetically expanded the dataset to improve accuracy by applying random distortions to training data, such as blurs, occlusions, translations, rotations, and more.
  - Successfully detected people’s hair and faces in real-time on a laptop webcam.
---

![Results of the CNN on a single image](./thumbnail.png)

A convolutional neural network (CNN) that accomplishes an image segmentation task. It identifies people's faces as well as what part of their face is hair.

The dataset was synthetically expanded to improve accuracy by randomly applying the following distortions:

- Occlusions (randomly-sized rectangle at a random angle and offset)
- Blurring the image
- Translations, rotations, and zooms of the image

It could successfully detect a person's face in real time on a laptop webcam.

It was originally going to be a component in an image processing pipeline that would allow the user to simulate different hairstyles. The missing components for this were:

- Another neural network that determined face pose from images
- A thing that takes this hair detector's results, takes the face pose results, and replaces the user's hair with a 3D hair model.
