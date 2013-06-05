function Shc = createShc(im,samples,shape,beta)
% creates Shape constraint matrix by concatenating weighted energy matrices corresponding to the contour samples

Shc=zeros([size(im),2]); [ny nx] = size(im);
N = size(shape.C,2); mxy = repmat([mean(shape.C(1,:));mean(shape.C(2,:))],1,N);
for i=1:numel(samples)
    msk=logical(zeros(size(im)));
    if samples(i).inner(1)>=1&samples(i).inner(1)<=nx&samples(i).inner(2)>=1&samples(i).inner(2)<=ny

        samples(i).C = samples(i).s.*samples(i).T*[shape.C-mxy] + mxy + repmat(samples(i).Cxy,1,N);
        ind = find(samples(i).C(1,:)>=1&samples(i).C(1,:)<=nx&samples(i).C(2,:)>=1&samples(i).C(2,:)<=ny);
        msk((round(samples(i).C(1,ind))-1).*size(msk,1)+round(samples(i).C(2,ind))) = 1;

        d = bwdist(msk,'euclidean');  fil = imfill(msk, round([samples(i).inner(2) samples(i).inner(1)]));
        d(fil) = -d(fil); p = 1./(1+exp(beta*d));
        Shc = Shc + samples(i).w*cat(3,-log(p),-log(1-p));
    end
end