classdef SealtestMode
    methods (Static)
        function activate(app)
            % Stop other things that might have been running
            app.stopAll;
            app.mode = 'sealtest';

            % set default positions
            modes.SealtestMode.sizing(app);
            modes.SealtestMode.resize(app);

            % no figure cache
            app.fig_cache = [];

            % Show figure
            app.fig.Visible = 'on';
        end

        function sizing(app)
            delete(app.panel);
            app.panel = uipanel('Parent',app.fig,'Position',[0 0 1 1]);

            delete(app.axes);
            app.axes = axes('Parent',app.panel,'Position',[0.05 0.05 0.95 0.95],...
                'GridLineStyle','-','XColor', 0.15*[1 1 1],'YColor', 0.15*[1 1 1]);
            set(app.axes,'NextPlot','replacechildren','XLimMode','manual');
            set(app.axes,'XGrid','on','YGrid','on','Tag','Axes','Box','on');
            app.axes.YLabel.String = 'Current (pA)';
            app.axes.YLabel.Color = 'k';
            app.axes.XLabel.String = 'Time (s)';
            app.axes.XLabel.Color = 'k';
            app.axes.YLim = [-800, 800];
            app.axes.XLim = [0, 0.05];

            % now make the buttons

            % Use plain-text labels instead of HTML for MATLAB UI compatibility
            zoomOutLabel = '-';
            scrollDownLabel = char(8595); % down arrow
            resetLabel = 'R';
            scrollUpLabel = char(8593);   % up arrow
            zoomInLabel = '+';

            % y-axis
            app.ybuts = [];
            app.ybuts(1) = uicontrol('Parent', app.panel, 'String', zoomOutLabel,...
                'callback', @(~,~) zoom_y('out'));
            app.ybuts(2) = uicontrol('Parent', app.panel, 'String', scrollDownLabel,...
                'callback', @(~,~) scroll_y('down'));
            app.ybuts(3) = uicontrol('Parent', app.panel, 'String', resetLabel,...
                'callback', @(~,~) reset_fig);
            app.ybuts(4) = uicontrol('Parent', app.panel, 'String', scrollUpLabel,...
                'callback', @(~,~) scroll_y('up'));
            app.ybuts(5) = uicontrol('Parent', app.panel, 'String', zoomInLabel,...
                'callback', @(~,~) zoom_y('in'));

            function zoom_y(str)
                if strcmp(str,'in')
                    app.axes.YLim = 1/2*get(app.axes(1),'YLim');
                elseif strcmp(str,'out')
                    app.axes.YLim = 2*get(app.axes(1),'YLim');
                end
            end

            function scroll_y(str)
                if strcmp(str,'up')
                    app.axes.YLim = get(app.axes(1),'YLim') + [1 1]*diff(get(app.axes(1),'YLim'))/5;
                elseif strcmp(str,'down')
                    app.axes.YLim = get(app.axes(1),'YLim') - [1 1]*diff(get(app.axes(1),'YLim'))/5;
                end
            end

            function reset_fig
                app.axes.XLim = [0, 0.1];
                app.axes.YLim = [-800, 800];
            end

            % top
            app.tbuts = [];
            app.tbuts(1) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Record.png'), ...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'sealtest_record');
            app.tbuts(2) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Play.png'), ...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'sealtest_play');
            % set the resize function
            set(app.panel, 'ResizeFcn', @(~,~) app.sealtestResizeFcn);
            % and call it to set default positions
            modes.SealtestMode.resize(app);

            % Show figure
            app.fig.Visible = 'on';
        end

        function resize(app)
            % get size of panel in pixels
            sz = app.getPixelPos(app.panel);
            % position the axes object
            sz(1) = sz(1) + app.DEFS.PADDING + app.DEFS.LABELWIDTH + app.DEFS.BUTTONSIZE; % left
            sz(3) = sz(3) - sz(1); % width
            sz(2) = sz(2) + app.DEFS.PADDING + app.DEFS.LABELWIDTH; % bottom
            sz(4) = sz(4) - sz(2) - app.DEFS.BIGBUTTONSIZE - 3*app.DEFS.PADDING; % height
            set(app.axes,'Position',max(1,sz),'Units','Pixels');
            % get size of axes in pixels
            sz = app.getPixelPos(app.axes);
            % figure out where the y middle is
            midle = sz(4)/2 + sz(2);
            % position the buttons
            for i=1:numel(app.ybuts)
                set(app.ybuts(i),'Position', ...
                    max(1,[app.DEFS.PADDING, ...
                    midle+(i-numel(app.ybuts)/2-1)*app.DEFS.BUTTONSIZE, ...
                    app.DEFS.BUTTONSIZE, ...
                    app.DEFS.BUTTONSIZE]));
            end
            % figure out where the x middle is
            midle = sz(3)/2 + sz(1);
            % position the buttons
            for i=1:numel(app.tbuts)
                set(app.tbuts(i),'Position', ...
                    max(1,[midle+(i-numel(app.tbuts)/2-1)*app.DEFS.BIGBUTTONSIZE, ...
                    sz(2) + sz(4) + app.DEFS.PADDING, ...
                    app.DEFS.BIGBUTTONSIZE, ...
                    app.DEFS.BIGBUTTONSIZE]));
            end
        end
    end
end
