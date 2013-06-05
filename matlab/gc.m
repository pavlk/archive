function [masks model]=gc(im,options,masks,model)
% GRAPHCUT Graph Cut 2-label 2D segmentation
%
% Sypnosis:
%   gc(im)
%   gc(im, options)
%   gc(im, options, masks)
%   gc(im, options, masks, model)
%
% Description: 
%   Based on paper: Boykov, Jolly - Interactive graph cuts for optimal
%   boundary & region segmentation of objects in N-D images [Boykov01.pdf]
%   - matlab wrapper for Graph Cuts: http://www.wisdom.weizmann.ac.il/~bagon/matlab.html
%
% Inputs:
%   im  double[m x n]  Input image to be segmented.
%   masks logical[m x n x 5] concatenated masks (5 layers):
%       1=maskin, foreground pixels mask
%       2=maskout, background pixels mask
%       3=maskIN (optional), hard constraints mask for the foreground
%       4=maskOUT (optional), hard constraints mask for the background
%
%   options
%       .lambda - double[1] weighting factor between DataCost and SmoothnessCost 
%
% Example:
%   masks = gc(imread('images/llama.jpg')); 
%   figure; imshow(masks(:,:,1); 

% Modifications:
% 05-may-2008, Pavol Vlcek, created

% used libraries, change paths if necessary
if isempty(strfind(path,'GCmatlab')) addpath ../libs/GCmatlab/; end
if isempty(strfind(path,'stprtool')) addpath ../libs/stprtool/; stprpath('../libs/stprtool/'); end

[ny,nx,nc] = size(im); 
im = im2double(im);

% process input arguments
if nargin < 2, options = []; else options = c2s(options); end
if nargin < 3, d = reshape(im,ny*nx,nc); [l0 c] = kmeans(d,2); l0 = logical(reshape(l0-1,ny,nx)); masks = cat(3,l0,~l0); figure;  imshow(l0); end
if nargin < 4, model = []; end
if ~isfield( options, 'verb'), options.verb = 0; end
if ~isfield( options, 'lambda'), options.lambda = 300; end
if ~isfield( options, 'emgmm'), for i=1:2, options.emgmm(i) = struct('init','kmeans','ncomp',5,'verb',0,'tmax',1,'type','full'); end; end
if size(masks,3) < 4, mskIN=masks(:,:,1); mskOUT=masks(:,:,2); else mskIN=masks(:,:,3); mskOUT=masks(:,:,4); end
mskin=masks(:,:,1); mskout=masks(:,:,2); 

% compute initial GMM model
d = reshape( im, ny*nx, nc );
if ~isfield(model,'gmm')
    if options.verb, fprintf('Initializing the Foreground and Background GMM model ... \n'); end
    model(1).gmm = emgmm(d(mskin(:),:)',options.emgmm(1));
    model(2).gmm = emgmm(d(mskout(:),:)',options.emgmm(2));
    if options.verb, fprintf('Segmenting ... \n'); end
end

% weight the pixels, recompute GMMs and compute pixel probabilities
if options.verb, fprintf('Recomputing the Foreground and Background GMM model ... \n'); end
[val ind1]=max(repmat(model(1).gmm.Prior',1,size(d,1)).*pdfgauss(d',model(1).gmm));
[val ind2]=max(repmat(model(2).gmm.Prior',1,size(d,1)).*pdfgauss(d',model(2).gmm));
k=[ind1;ind2];
for i = 1:2
    for j = 1:size(model(i).gmm.options,2)
        ind_=masks(:,:,i);ind=(k(i,:)==j);
        ind=logical(ind.*ind_(:)'); pixels=d(ind,:);
        model(i).gmm.Mean(:,j)=mean(pixels)';
        model(i).gmm.Cov(:,:,j)=cov(pixels);
        model(i).gmm.Prior(j)=size(pixels,1)/sum(ind_(:));
    end
    p(i).p=reshape( pdfgmm(d',model(i).gmm)', ny, nx );
end
p = cat(3,p(1).p,p(2).p); p = (p-min(p(:)))/(max(p(:))-min(p(:)));

% compute the DataCost
Dc = zeros(size(p)); B = 5; mult = 1;
Dc(p<1e-5) = B;Dc(p>=1e-5) = -log(p(p>=1e-5));

if isfield(options,'Shc') Dc = Dc + options.Shc; end % for objcut
Dc = mult*Dc; Dc = floor(Dc);

% initialize the SmoothnessCost cost
Sc=ones(2)-eye(2);

% compute the spatialy altering smoothness costs
imR=im(:,:,1);imG=im(:,:,2);imB=im(:,:,3);
Hc=[diff([imR';imR(:,size(im,2))']).^2 + diff([imG';imG(:,size(im,2))']).^2 + diff([imB';imB(:,size(im,2))']).^2]';
Vc=[diff([imR;imR(size(im,1),:)]).^2 + diff([imG;imG(size(im,1),:)]).^2 + diff([imB;imB(size(im,1),:)]).^2];
beta=1/(2*mean([Vc(:);Hc(:)]));
Vc=exp(-(beta*Vc));Hc=exp(-(beta*Hc));

% set the hard constraints
K = 1 + 4*max([Vc(:); Hc(:)]);
tmp = Dc(:,:,1); tmp(mskIN)=0; tmp(mskOUT) = K; Dc(:,:,1) = tmp;
tmp = Dc(:,:,2); tmp(mskIN)=K; tmp(mskOUT) = 0; Dc(:,:,2) = tmp;

% minimize the graph energy
gch = GraphCut( 'open',Dc,mult*options.lambda*Sc,Vc,Hc);
[gch l] = GraphCut( 'expand', gch);
[gch se de] = GraphCut('energy', gch);en = se + de;
gch = GraphCut( 'close', gch );

% postporcessing of the labelling (fill in small holes)
% lb=(l+1==1);
% lb=imdilate(lb,strel('disk',1))-lb;
% [x y]=find(lb);round([mean(y) mean(x)]);
% lfill=logical(imfill(logical(lb),round([mean(y) mean(x)])));
% lfill=imdilate(imerode(lfill,strel('disk',2)),strel('disk',1));
% lb=imdilate(lfill,strel('disk',1))-lfill;

masks(:,:,1) = (l+1==1);
masks(:,:,2) = ~masks(:,:,1);