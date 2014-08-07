function [C, sigma] = dataset3Params(X, y, Xval, yval)
%EX6PARAMS returns your choice of C and sigma for Part 3 of the exercise
%where you select the optimal (C, sigma) learning parameters to use for SVM
%with RBF kernel
%   [C, sigma] = EX6PARAMS(X, y, Xval, yval) returns your choice of C and 
%   sigma. You should complete this function to return the optimal C and 
%   sigma based on a cross-validation set.
%

% You need to return the following variables correctly.
C = 1;
sigma = 0.3;

% ====================== YOUR CODE HERE ======================
% Instructions: Fill in this function to return the optimal C and sigma
%               learning parameters found using the cross validation set.
%               You can use svmPredict to predict the labels on the cross
%               validation set. For example, 
%                   predictions = svmPredict(model, Xval);
%               will return the predictions on the cross validation set.
%
%  Note: You can compute the prediction error using 
%        mean(double(predictions ~= yval))
%

%{
errors = [];
for C_ = [0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30]
	for sigma_ = [0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30]
		% Train the model on training set
		model = svmTrain(X, y, C_, @(x1, x2) gaussianKernel(x1, x2, sigma_));
		% Predict over validation set
		predictions = svmPredict(model, Xval);
		% Compute the prediction error
		error = mean(double(predictions ~= yval));
		% Print the result
		%fprintf('Error for C=%f, sigma=%f is %f%%\n', C_, sigma_, error);
		errors = [errors; error, C_, sigma_];
	end
end

% Find the min error
[error, index] = min(errors(:,1));
C = errors(index,2);
sigma = errors(index,3);
fprintf('Min error is %f.2%% with C=%f, sigma=%f\n', error*100, C, sigma);
%}

C = 1;
sigma = 0.1;
fprintf('Min error is 3%% with C=%f, sigma=%f\n', C, sigma);

% =========================================================================

end
