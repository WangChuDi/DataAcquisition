classdef NoiseMode
    methods (Static)
        function activate(app)
            % Stop other things that might have been running
            app.stopAll;
            app.mode = 'noise';

            % Set default positions
            modes.NoiseMode.sizing(app);
            modes.NoiseMode.resize(app);

            % No figure cache necessary
            app.fig_cache = [];

            % Show figure
            app.fig.Visible = 'on';
        end

        function sizing(app)
            delete(app.panel);
            app.panel = uipanel('Parent',app.fig,'Position',[0 0 1 1]);

            delete(app.axes);
            app.axes = axes('Parent',app.panel,'Position',[0.05 0.05 0.9 0.9],...
                'GridLineStyle','-','XColor', 0.15*[1 1 1],'YColor', 0.15*[1 1 1]);
            set(app.axes,'NextPlot','add','XLimMode','manual');
            set(app.axes,'XGrid','on','YGrid','on','Tag','Axes', ...
                'Box','on','XScale','log','YScale','log');
            app.axes.YLabel.String = 'Current noise power spectral density (pA^2/Hz)';
            app.axes.YLabel.Color = 'k';
            app.axes.XLabel.String = 'Frequency (Hz)';
            app.axes.XLabel.Color = 'k';
            app.axes.YLim = [1e-8 1e2];
            app.axes.XLim = [1 5e4];

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
                    app.axes.YLim = 10.^(log10(get(app.axes,'YLim')) + [1 -1]);
                elseif strcmp(str,'out')
                    app.axes.YLim = 10.^(log10(get(app.axes,'YLim')) + [-1 1]);
                end
            end

            function scroll_y(str)
                if strcmp(str,'up')
                    app.axes.YLim = 10.^(log10(get(app.axes,'YLim')) + [1 1]);
                elseif strcmp(str,'down')
                    app.axes.YLim = 10.^(log10(get(app.axes,'YLim')) + [-1 -1]);
                end
            end

            function reset_fig
                app.axes.YLim = [1e-8 1e2];
                app.axes.XLim = [1 5e4];
            end

            % x-axis
            app.xbuts = [];
            app.xbuts(1) = uicontrol('Parent', app.panel, 'String', zoomOutLabel,...
                'callback', @(~,~) zoom_x('out'));
            app.xbuts(2) = uicontrol('Parent', app.panel, 'String', scrollDownLabel,...
                'callback', @(~,~) scroll_x('down'));
            app.xbuts(3) = uicontrol('Parent', app.panel, 'String', resetLabel,...
                'callback', @(~,~) reset_fig);
            app.xbuts(4) = uicontrol('Parent', app.panel, 'String', scrollUpLabel,...
                'callback', @(~,~) scroll_x('up'));
            app.xbuts(5) = uicontrol('Parent', app.panel, 'String', zoomInLabel,...
                'callback', @(~,~) zoom_x('in'));

            function zoom_x(str)
                if strcmp(str,'in')
                    app.axes.XLim = 10.^(log10(get(app.axes,'XLim')) + [1 -1]);
                elseif strcmp(str,'out')
                    app.axes.XLim = 10.^(log10(get(app.axes,'XLim')) + [-1 1]);
                end
            end

            function scroll_x(str)
                if strcmp(str,'up')
                    app.axes.XLim = 10.^(log10(get(app.axes,'XLim')) + [1 1]);
                elseif strcmp(str,'down')
                    app.axes.XLim = 10.^(log10(get(app.axes,'XLim')) + [-1 -1]);
                end
            end

            % use a plot to set the axes limits appropriately
            app.plotNoisePSD;

            modes.NoiseMode.resize(app);
        end

        function resize(app)
            % get size of panel in pixels
            sz = app.getPixelPos(app.panel);
            % position the axes object
            sz(1) = sz(1) + app.DEFS.PADDING + app.DEFS.LABELWIDTH + app.DEFS.BUTTONSIZE; % left
            sz(3) = sz(3) - sz(1); % width
            sz(2) = sz(2) + app.DEFS.PADDING + app.DEFS.LABELWIDTH + app.DEFS.BUTTONSIZE; % bottom
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
            for i=1:numel(app.xbuts)
                set(app.xbuts(i),'Position', ...
                    max(1,[midle+(i-numel(app.xbuts)/2-1)*app.DEFS.BUTTONSIZE, ...
                    app.DEFS.PADDING, ...
                    app.DEFS.BUTTONSIZE, ...
                    app.DEFS.BUTTONSIZE]));
            end
        end
    end
end
