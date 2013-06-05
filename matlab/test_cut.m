function test_cut(algorithm)
directory = './data';
curdir = pwd; eval(['cd ' directory]); d = dir; d(1:2) = []; eval(['cd ' curdir]);
del = []; for i = 1:size(d,1) if ~strcmp(d(i).name(end-2:end),'jpg') del = [del i]; end; end; d(del) = [];

fprintf('\nTesting algorithm ''%s''.\n',algorithm);
fprintf(' - the format is: good_pixels/all_pixels(%% of good pixels), for ForeGround and BackGround pixels respectively\n\n');

for i = 1:size(d,1)
   
    basename = d(i).name(1:end-3);
    im = imread([directory '/' basename 'jpg']); 
    m = imread([directory '/boundary_GT_rect/' basename 'bmp']); 
    gt = logical(imread([directory '/boundary_GT/' basename 'bmp']));

    mskin = logical(zeros(size(m))); mskin(m==128) = 1;
    mskout = logical(zeros(size(m))); mskout(m==64) = 1;
    masks = cat(3,mskin,mskout,logical(zeros(size(m))),~mskin);

    options.lambda = 80*[1,1,1,1,1,.9,.8,.7,.6,.6,.6,.6,.6,.6];
    options.nIter = 3;
    options.dispIter = 1;
    options.beta = 0.03;
    options.beta2 = 0.05;
    
    switch algorithm
        case 'objcut'
            options.box = [size(im,2)/8 size(im,1)/8 0.05 pi/8];
            options.nsamples = [30 30 3 3];
            options.gt = gt;
            gc = objcut(im,gt,options,masks);

        case 'grabcut'
            gc = grabcut(im,options,masks);
    end
    
    figure(i);clf;
    subplot(221);imshow(im); title('Image');
    subplot(222);imshow(m); title('Initial Trimap');
    subplot(223);imshow(gc); title('Segmentation');
    subplot(224);imshow(gt); title('Ground Truth');
    drawnow;

    rect = mskin+mskout; nfg(i) = sum(gt(:)); nbg(i) = sum((~gt(:))&rect(:)); 
    nfg_gc(i) = sum(gc(:)&gt(:)); nbg_gc(i) = sum(~gc(:)&~gt(:)&rect(:));
    succ_bg(i) = 100*nbg_gc(i)/nbg(i);
    succ_fg(i) = 100*nfg_gc(i)/nfg(i);

    fprintf('[%d.] %s, \t %d/%d(%.2f%%),\t%d/%d(%.2f%%)\n',i,[basename 'jpg'],nfg_gc(i),nfg(i),100*nfg_gc(i)/nfg(i),nbg_gc(i),nbg(i),100*nbg_gc(i)/nbg(i));
     try,catch
        fprintf('[%d.] %s, GC error!\n',i,[basename 'jpg']);
    end
end
fprintf('\n[MEAN:] \t %d/%d(%.2f%%),\t%d/%d(%.2f%%)\n',sum(nfg_gc),sum(nfg),100*sum(nfg_gc)/sum(nfg),sum(nbg_gc),sum(nbg),100*sum(nbg_gc)/sum(nbg));
fprintf('\nSTD: \t%d \t:%d\n', std(succ_fg), std(succ_bg));