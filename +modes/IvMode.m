classdef IvMode
    methods (Static)
        function activate(app)
            % Stop other things that might have been running
            app.stopAll;
            app.mode = 'iv';

            % set default positions
            modes.IvMode.sizing(app);
            modes.IvMode.resize(app);

            % No figure cache necessary
            app.fig_cache = [];

            % Show figure
            app.fig.Visible = 'on';
        end

        function sizing(app)
            delete(app.panel);
            app.panel = uipanel('Parent',app.fig,'Position',[0 0 1 1]);

            delete(app.axes);
            app.axes(1) = axes('Parent',app.panel,'Position',[0.05 0.05 0.45 0.90],...
                'GridLineStyle','-','XColor', 0.15*[1 1 1],'YColor', 0.15*[1 1 1]);
            set(app.axes(1),'NextPlot','add','XLimMode','manual');
            set(app.axes(1),'XGrid','on','YGrid','on','Tag','Axes','Box','on');
            app.axes(1).YLabel.String = 'Current (pA)';
            app.axes(1).YLabel.Color = 'k';
            app.axes(1).XLabel.String = 'Time (s)';
            app.axes(1).XLabel.Color = 'k';
            app.axes(1).YLim = [-800 800];
            app.axes(1).XLim = [0 0.8];

            app.axes(2) = axes('Parent',app.panel,'Position',[0.55 0.05 0.45 0.90],...
                'GridLineStyle','-','XColor', 0.15*[1 1 1],'YColor', 0.15*[1 1 1]);
            set(app.axes(2),'NextPlot','add','XLimMode','manual');
            set(app.axes(2),'XGrid','on','YGrid','on','Tag','Axes','Box','on');
            app.axes(2).YLabel.String = 'Mean Current (pA)';
            app.axes(2).YLabel.Color = 'k';
            app.axes(2).XLabel.String = 'Voltage (mV)';
            app.axes(2).XLabel.Color = 'k';
            app.axes(2).YLim = [-800 800];
            app.axes(2).XLim = [-200 200];

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
                    lims = 1/2*get(app.axes(1),'YLim');
                elseif strcmp(str,'out')
                    lims = 2*get(app.axes(1),'YLim');
                end
                app.axes(1).YLim = lims;
                app.axes(2).YLim = lims;
            end

            function scroll_y(str)
                if strcmp(str,'up')
                    lims = get(app.axes(1),'YLim') + [1 1]*diff(get(app.axes(1),'YLim'))/5;
                elseif strcmp(str,'down')
                    lims = get(app.axes(1),'YLim') - [1 1]*diff(get(app.axes(1),'YLim'))/5;
                end
                app.axes(1).YLim = lims;
                app.axes(2).YLim = lims;
            end

            function reset_fig
                app.axes(1).YLim = [-800 800];
                app.axes(2).YLim = [-800 800];
                app.axes(1).XLim = [0 0.8];
            end

            % top
            app.tbuts = [];
            app.tbuts(1) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Sweep_Up.png'),...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'iv_sweep_up');
            app.tbuts(2) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Sweep_Bi.png'),...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'iv_sweep_bi');
            app.tbuts(3) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Sweep_Down.png'),...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'iv_sweep_down');
            % set the resize function
            set(app.panel, 'ResizeFcn', @(~,~) app.ivResizeFcn);
            % and call it to set default positions
            modes.IvMode.resize(app);

            % Show figure
            app.fig.Visible = 'on';
        end

        function resize(app)
            % position the axes1 object
            sz = app.getPixelPos(app.panel);
            sz(1) = sz(1) + app.DEFS.PADDING + app.DEFS.LABELWIDTH + app.DEFS.BUTTONSIZE; % left
            sz(3) = sz(3)/2 - app.DEFS.PADDING - 1.5*app.DEFS.LABELWIDTH - app.DEFS.BUTTONSIZE - 2*app.DEFS.PADDING; % width
            sz(2) = sz(2) + app.DEFS.PADDING + app.DEFS.LABELWIDTH; % bottom
            sz(4) = sz(4) - sz(2) - app.DEFS.BIGBUTTONSIZE - 3*app.DEFS.PADDING; % height
            set(app.axes(1),'Position',max(1,sz),'Units','Pixels');
            set(app.axes(1),'Position',max(1,sz),'Units','Pixels');
            % figure out where the y middle is
            midle = sz(4)/2 + sz(2);
            % position the buttons
            for i=1:numel(app.ybuts)
                set(app.ybuts(i),'Position', ...
                    [app.DEFS.PADDING, ...
                    midle+(i-numel(app.ybuts)/2-1)*app.DEFS.BUTTONSIZE, ...
                    app.DEFS.BUTTONSIZE, ...
                    app.DEFS.BUTTONSIZE]);
            end
            % position the axes2 object
            sz = app.getPixelPos(app.panel);
            sz(1) = sz(1) + sz(3)/2 + app.DEFS.PADDING + app.DEFS.LABELWIDTH + app.DEFS.BUTTONSIZE; % left
            sz(3) = sz(3)/2 - app.DEFS.PADDING - 1.5*app.DEFS.LABELWIDTH - app.DEFS.BUTTONSIZE - 2*app.DEFS.PADDING; % width
            sz(2) = sz(2) + app.DEFS.PADDING + app.DEFS.LABELWIDTH; % bottom
            sz(4) = sz(4) - sz(2) - app.DEFS.BIGBUTTONSIZE - 3*app.DEFS.PADDING; % height
            set(app.axes(2),'Position',max(1,sz),'Units','Pixels');
            % position the top button
            bottom = sz(2) + sz(4) + app.DEFS.PADDING;
            sz = app.getPixelPos(app.panel);
            midle = sz(1)+sz(3)/2;
            for i=1:numel(app.tbuts)
                set(app.tbuts(i),'Position', ...
                    max(1,[midle+(i-numel(app.tbuts)/2-1)*app.DEFS.BIGBUTTONSIZE, ...
                    bottom, ...
                    app.DEFS.BIGBUTTONSIZE, ...
                    app.DEFS.BIGBUTTONSIZE]));
            end
        end
    end
end
