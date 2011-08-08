% Andrew Kordik
clear all; close all; clc;

[original_image,b,c] = imread('lena','bmp');
original_image_info = imfinfo('lena', 'bmp');

[countsR,binsR] =  imhist(original_image(:,:,1));
[countsG,binsG] =  imhist(original_image(:,:,2));
[countsB,binsB] =  imhist(original_image(:,:,3));

figure(1)
title('Please Be a title')
% plot(binsR,countsR, 'r', binsG, countsG, 'g', binsB, countsB, 'b');
subplot(2,2,1);
stem(binsR,countsR, 'r');
title('Red')
xlabel('Pixel Value');
ylabel('Count');
subplot(2,2,2);
stem(binsG, countsG, 'g');
title('Green')
xlabel('Pixel Value');
ylabel('Count');
subplot(2,2,3);
stem(binsB, countsB, 'b');
title('Blue')
xlabel('Pixel Value');
ylabel('Count');
subplot(2,2,4);
hold on
stem(binsR,countsR, 'r');
stem(binsG, countsG, 'g');
stem(binsB, countsB, 'b');
title('All')
xlabel('Pixel Value');
ylabel('Count');


[original_image,b,c] = imread('FullHistEq','bmp');
original_image_info = imfinfo('FullHistEq', 'bmp');

[countsR,binsR] =  imhist(original_image(:,:,1));
[countsG,binsG] =  imhist(original_image(:,:,2));
[countsB,binsB] =  imhist(original_image(:,:,3));

figure(2)
subplot(2,2,1);
stem(binsR,countsR, 'r');
title('Red')
xlabel('Pixel Value');
ylabel('Count');
subplot(2,2,2);
stem(binsG, countsG, 'g');
title('Green')
xlabel('Pixel Value');
ylabel('Count');
subplot(2,2,3);
stem(binsB, countsB, 'b');
title('Blue')
xlabel('Pixel Value');
ylabel('Count');
subplot(2,2,4);
hold on
stem(binsR,countsR, 'r');
stem(binsG, countsG, 'g');
stem(binsB, countsB, 'b');
% plot(binsR,countsR, 'r', binsG, countsG, 'g', binsB, countsB, 'b');
title('All')
xlabel('Pixel Value');
ylabel('Count');


