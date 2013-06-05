function browse(filename)
% BROWSE simple nonlinear video browser
% 
% Usage: 
%   - click on the bar on the bottom to move to a particular frame
%   - click to the left/right of the bar to step one frame backward/forward
%
% See also MP_OPEN, MP_GETFRM, VIDINFO

mp_open(filename);          % open mplayer daemon
inf = vidinfo(filename);    % get the NumFrames information

options = [];
if inf.format == 'MJPG', inf.NumFrames = inf.NumFrames/2; options.mode = 'raw'; end

frame = 2; im = mp_getfrm(frame);
try % because closing the window while waitforbuttonpress gives an error
    while(1)
        figure(1); clf; hold on; iptsetpref('ImshowBorder', 'tight'); 
        p2 = subplot(212); set(p2,'position',[0.06 0.08 .9 .03]); axis([1 inf.NumFrames 0 1]);
        hold on; plot([frame,frame],[0,1],'r');  xlabel(['Frame: ' num2str(frame)]);

        p1 = subplot(211); hold on; set(p1,'position',[0.05 0.1 1 .85]); imshow(im); title(filename);

        waitforbuttonpress;
        u = get(p2,'currentpoint'); u = round(u(1));

        if u > inf.NumFrames
            frame = frame+1;
            im = mp_getfrm();
        else
            if u < 1
                frame = frame-1;
            else
                frame = u;
            end
            im = mp_getfrm(frame,options);
        end
    end
catch; end;