function jurie(nExamples, directory)
% elementary demo of the "Jurie template tracker" 
% paper: Jurie, Dhome - Real Time Robust Template Matching (2002)
%
% nExamples - int(1), number of example templates, clicked by the user
% directory - string, directory containing images of a video sequence
%
% Example: jurie(3,'./images/seq1/');

nObs = 100;             % number of observation pixels over the template
nPoses = 1000;          % number of training examples generated in the translation+scale space
ranges = [10,10,0.1];   % ranges for the training examples 

clc; N = nPoses*nExamples; iptsetpref('ImshowBorder', 'tight'); 
eval(['cd ' directory]); d = dir; d(1:2) = []; I = zeros(nObs,N); T = zeros(3,N); c = 0;

%% prepare the data, (matrices T, I)
for i = 1:ceil(numel(d)/nExamples):numel(d), c=c+1; fprintf('Learning from frame %d, ',i);

    % initialize t and obs (patch observation indexes)
    img = im2double(imread(d(i).name)); figure(1); clf; imshow(img); hold on; fprintf('\nSelect the patch to be tracked...\n')
    [n b] = imcrop; scale = round(b(4)/2); tt = [b(1)+scale b(2)+scale 1];
     
    if i==1, init_t = tt; [ny nx] = size(img); box = [-1,1,-1,1;-1,-1,1,1]*scale; 
        [x y] = meshgrid(-scale:scale,-scale:scale);    % select observationd randomly from a grid
        ind = randperm(numel(x)); obs = [x(ind(1:nObs));y(ind(1:nObs))]; end

    % generate the training set
    for k = 1:nPoses, j = (c-1)*nPoses+k;
        T(:,j) = [(2*rand(1,2) -1).*ranges(1:2) 1+(2*rand(1)-1).*ranges(3)]';
        t = [tt(1:2) + T(1:2,j)' tt(3)*T(3,j)]';
        nind = round(obs*t(3)+repmat(t(1:2),1,size(obs,2)));  ind = find(nind(1,:)>0&nind(1,:)<=nx&nind(2,:)>0&nind(2,:)<=ny);
        I(ind,j) = img(((nind(1,ind)-1).*size(img,1)+nind(2,ind))); end
end

%% learn the linear mapping - the LSQ minimisation of the training error
tic; H = T*I'*inv(I*I'); timeL = toc; 

%% track the sequence
t = init_t; %save H H obs t;
for i = 1:numel(d)
    img = im2double(imread(d(i).name)); 
      
    tic, nind = round(obs*t(3)+repmat(t(1:2)',1,size(obs,2))); ind = find(nind(1,:)>0&nind(1,:)<=nx&nind(2,:)>0&nind(2,:)<=ny);
    l(ind) = img((nind(1,ind)-1).*size(img,1)+nind(2,ind));
    dt = H*l'; t = [t(1:2)-dt(1:2)' t(3)/dt(3)]; 
    time(i) = toc; 
   
    figure(2); clf; imshow(img); hold on; 
    bb = round(box*t(3) + repmat(t([2,1])',1,size(box,2))); plot([bb(2,[1,2]);bb(2,[1,3]);bb(2,[2,4]);bb(2,[3,4])]',[bb(1,[1,2]);bb(1,[1,3]);bb(1,[2,4]);bb(1,[3,4])]','r'); drawnow;
end

fprintf('\n\nLearning framerate: %f fps\n',1/timeL); fprintf('Tracking framerate: %f fps\n',1/mean(time))