- When trying to find out the values predicted by the model 
  - Can either use fitted() or predict()
  - The difference between the two is explained [here](https://stackoverflow.com/questions/12201439/is-there-a-difference-between-the-r-functions-fitted-and-predict)
    - Basically, predict is before the exponential is applied 
    - Fitted is after we've applied the exponentiate, to correct the values back! 
    
- I've realised that there is a difference when you have model outputs from fitted() or predict() or ggpredict() 
  - I've created another script to test out and try to explain the differences
  - So that I can figure out which is the best for my purposes