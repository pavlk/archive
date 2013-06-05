function samples = sampleShape(im,in_shape,in_samples,options);
% sample shapes (4 parameters Cx, Cy, scale, alfa)
% TODO: rewrite - sampling in the parameter space should be independent and
%       then a sample2contour and eval_sample functions used

if nargin < 3, options = []; else options = c2s(options); end
if ~isfield( options, 'box'), options.box = [size(im,2)/3 size(im,1)/3 0.1 pi/8]; end
if ~isfield( options, 'nsamples'), options.nsamples = [35 35 8 3]; end
if ~isfield( options, 'beta2'), options.beta2 = 0.05; end
if ~isfield( options, 'threshold'), options.threshold = 25; end

samples = [];
for i=1:numel(in_samples)
    N = size(in_shape.C,2); mxy = repmat([mean(in_shape.C(1,:));mean(in_shape.C(2,:))],1,N);
    shape = in_samples(i).s.*in_samples(i).T*[in_shape.C-mxy] + mxy + repmat(in_samples(i).Cxy,1,N);
    W = in_samples(i).w;
    inner = in_samples(i).inner;

    nPoints = 30; ind = randperm(size(shape,2)); shp = shape(:,ind(1:nPoints));
    N = size(shp,2); smpls = repmat(struct('inner',[1 1]','s',1,'T',eye(2),'Cxy',[1 1]','w',0),1,3); min=1;
    mxy = repmat([mean(shp(1,:));mean(shp(2,:))],1,N);
    for Cx = linspace(-options.box(1),options.box(1),options.nsamples(1))
        for Cy = linspace(-options.box(2),options.box(2),options.nsamples(2))
            for s = linspace(1-options.box(3),1+options.box(3),options.nsamples(3))
                for alfa = linspace(-options.box(4),options.box(4),options.nsamples(4));
                    T = [cos(alfa) sin(alfa);-sin(alfa) cos(alfa)];
                    C = s.*T*[shp-mxy] + mxy + repmat([Cx;Cy],1,N);
                    dst = eval_contour(im,C(1,:),C(2,:),options.threshold);
                    w = W*1./(1+exp(options.beta2*(dst)));
                    if w > smpls(min).w,
                        sh.inner = s.*T*[inner-mxy(:,1)] + mxy(:,1) + [Cx;Cy];
                        sh.s = s; sh.T = T; sh.Cxy = [Cx;Cy];
                        sh.w = w; smpls(min) = sh;
                        for i = 1:size(smpls,2) if smpls(i).w<smpls(min).w; min = i; end; end; end
                end
            end
        end
    end
end
samples = [samples smpls];

if sum([samples.w]), for i=1:numel(samples) samples(i).w = samples(i).w/sum([samples.w]); end; end
%samples(1).C = shape; samples(1).w = 1; samples(1).inner = inner;

function d = eval_contour(dt,x,y,d_thr)
[imy imx] = size(dt); u = round(x); v = round(y);
idx = find(u<imx & u>0 & v<imy & v>0); if isempty(idx), d = 1; return; end;
D = dt(sub2ind(size(dt),v(idx),u(idx)));
d = (sqrt(sum(D(D<d_thr).^2)/numel(idx)));