% Benchmaking Results

%
% This was generated using Averaging filters from git repo

numIterations  = [1,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150];
FIPlib = [378.9968,374.0718,368.2750,368.6029,371.2390,374.9595,371.6154,371.2094,375.0534,373.4769,369.9306,368.4957,377.1861, 370.7231,380.8140,380.1079]
FIPerr = [11.48849,6.925908,7.777367,5.907173,6.829335,6.056652,6.608153,6.211078,7.538131,6.897612,6.998396,6.901190,10.99272,7.637936,20.69016,22.46980]

C = [0.003685, 0.035255, 0.069794, 0.103634, 0.139487, 0.172198, 0.208406, 0.242812, 0.276208, 0.311052, 0.353391, 0.378796,0.413669 ,0.447875 ,0.480440 ,0.514075]


figure
hold on
errorbar(numIterations, FIPlib, FIPerr,'-*');
plot(numIterations,C.*1000, '-x');
hold off
legend('FIPlib', 'C')
title('Execution Time for Succesive Window Filters')
xlabel('number of filters applied')
ylabel('execution time (ms)')




