function labeling = grabcut(im,options,masks)
% GRABCUT Grab Cut algorithm-based segmentation
%
% Sypnosis:
%   grabcut(im)
%   grabcut(im, options)
%   grabcut(im, options, masks)
%
% Description:
%   Based on paper: Rother, Kolmogorov, Blake - "GrabCut": interactive
%   foreground extraction using iterated graph cuts [Rother04.pdf]
%
% Input:
%   im - input image to be segmented
%   options
%       .lambda double[1] weighting factor between DataCost and SmoothnessCost 
%       .nIter int[1] number of GrabCut iterations
%       .disp logical[1] display the resulting labeling
%       .dispIter logical[1] display different by colour each iteration
%   masks logical[m x n x 5] see help graphcut
%
% Output:
%   labeling logical[m x n] mask of the resulting labeling
%
% Examples:
%   figure; imshow(grabcut(imread('images/llama.jpg')));
%   grabcut(imread('images/llama.jpg'),{'lambda',50,'nIter',5,'disp',1,'dispIter',1});
%
% See also GC

% Modifications:
% 30-apr-2008, Pavol Vlcek, created

if exist('GraphCut')~=2 || exist('GraphCutMex')~=3 || ...
      exist('GraphCutConstr')~=3 ,
  disp('ERROR: It appears that the GraphCut Matlab wrapper is not installed.');
  disp('Please install it from ') ;
  disp('         http://www.wisdom.weizmann.ac.il/~bagon/matlab.html') ;
  disp('to directory ../matlab_code/graphcut.') ;
  error([ 'GraphCut wrapper not installed.'])
end ;

if exist('emgmm')~=2 
  disp('ERROR: It appears that the STPRTool is not installed.');
  error([ 'STPRTool not installed.'])
end ;

% process input arguments
if nargin < 2, options = []; else options = c2s(options); end
if ~isfield( options, 'lambda'), options.lambda = 10; end
if ~isfield( options, 'nIter'), options.nIter = 3; end
if ~isfield( options, 'disp'), options.disp = 0; end
if ~isfield( options, 'dispIter'), options.dispIter = 0; end

% 'cooling schedule' of lambdas
lambdas = options.lambda;
if numel(lambdas)==1 lambdas = repmat(lambdas,1,options.nIter); end

if nargin < 3,
    % user grabs the object to segment
    fprintf('\nGrab the object to be segmented with the mouse!\n');
    figure(1);clf;imshow(im);hold on;

    % crop the image
    [n c] = imcrop; [imy imx]=size(im);
    w = round(mean([c(3),c(4)])/20); % width of the background mask
    b = round([max(c(2)-w,1),min(c(2)+c(4)+w,imy),max(c(1)-w,1),min(c(1)+c(3)+w,imx)]); 
    im = im(b(1):b(2),b(3):b(4),:);     
   
    %% prepare the masks
    w = round([max(0,min(w,c(2))),max(0,min(w,imy-(c(2)+c(4)+w))),max(0,min(w,c(1))),max(0,min(w,imx-(c(1)+c(3)+w)))]);    
    l = logical(zeros(size(im(:,:,1)))); mskIN = l; l(1+w(1):end-w(2),1+w(3):end-w(4)) = 1; 
    mskin = l; mskout = ~mskin; mskOUT = mskout;
    masks = cat(3,mskin,mskout,mskIN,mskOUT);
end

% display initial segmentation and masks
if options.disp, figure(1); clf; imshow(im); hold on; contour(masks(:,:,1),[1 1],'b','LineWidth',2); end
if options.dispIter, figure(2); clf; imshow(im); hold on; title('Iterations (1. green, 2. blue, 3. yellow, 4. magnetta, 5. black, 6. white, 7.green ...)'); end

% iterate graphcut minimisation
model = []; cols = 'gbymkw';
for iter = 1:options.nIter; options.iter = iter;
    options.lambda = lambdas(iter);
    masks = gc(im,options,masks,model);
    if options.dispIter, figure(2); contour(masks(:,:,1),[1 1],cols(mod(iter-1,numel(cols))+1),'LineWidth',2); drawnow; end
end
labeling = masks(:,:,1);

% show the result
if options.disp, figure(1); contour(labeling,[1 1],'r','LineWidth',2); title('Initial (blue) and resulting (red) segmentation.'); end