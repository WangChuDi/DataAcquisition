classdef NormalMode
    methods (Static)
        function activate(app)
            % Stop other things that might have been running
            app.stopAll;
            app.mode = 'normal';

            % Set default positions
            modes.NormalMode.sizing(app);
            modes.NormalMode.resize(app);

            % Reset split view controls for normal mode
            app.splitMode = false;
            app.clearSplitView();
            if ~isempty(app.splitButton) && isvalid(app.splitButton)
                set(app.splitButton, 'Value', 0);
            end

            % Set initial channel visibility based on enabled state
            app.updateChannelDisplay();

            % Show figure
            app.fig.Visible = 'on';
        end

        function sizing(app)
            delete(app.panel);
            app.panel = uipanel('Parent',app.fig,'Position',[0 0 1 1]);

            if ~isempty(app.mainOscilloscope) && isvalid(app.mainOscilloscope)
                delete(app.mainOscilloscope);
            end

            app.mainOscilloscope = OscilloscopeWindow(app.panel, app.alpha, 5, [0.05 0.05 0.9 0.9]);
            app.axes = app.mainOscilloscope.Axes;
            app.fig_cache = app.mainOscilloscope.Cache;
            app.ybuts = app.mainOscilloscope.YButtons;
            app.xbuts = app.mainOscilloscope.XButtons;

            % split view toggle
            app.splitButton = uicontrol('Parent', app.panel, 'Style', 'togglebutton', ...
                'String', 'Split', 'FontWeight', 'bold', ...
                'callback', @(src,~) app.toggleSplitView(src));

            % top
            app.tbuts = [];
            app.tbuts(1) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_NEGZAP_On.png'),...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'set_negative_zap_voltage');
            app.tbuts(2) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_NV_Off.png'),...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'set_negative_higher_voltage');
            app.tbuts(3) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_NV_Off.png'),...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'set_negative_high_voltage');
            app.tbuts(4) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_NV_Off.png'),...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'set_negative_midtwo_voltage');
            app.tbuts(5) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_NV_Off.png'),...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'set_negative_mid_voltage');
            app.tbuts(6) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_NV_Off.png'),...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'set_negative_low_voltage');
            app.tbuts(7) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Record.png'),...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'record');
            app.tbuts(8) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Pause.png'),...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'pause');
            app.tbuts(9) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_PV_Off.png'),...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'set_positive_low_voltage');
            app.tbuts(10) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_PV_Off.png'),...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'set_positive_mid_voltage');
            app.tbuts(11) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_PV_Off.png'),...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'set_positive_midtwo_voltage');
            app.tbuts(12) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_PV_Off.png'),...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'set_positive_high_voltage');
            app.tbuts(13) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_PV_On.png'),...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'set_positive_higher_voltage');
            app.tbuts(14) = uicontrol('Parent', app.panel, ...
                'Style', 'togglebutton', 'CData', imread('Voltage_POSZAP_On.png'),...
                'callback', @(src,~) app.stateDecision(src), 'tag', 'set_positive_zap_voltage');

            app.chbuts = [];
            labels = {'Ch0','Ch1','Ch2','Ch3','Ch4','Ch5','Ch6','Ch7'};
            for ii = 1:numel(app.allChannels)
                app.chbuts(ii) = uicontrol('Parent', app.panel, ...
                    'Style', 'togglebutton', 'String', labels{ii},...
                    'Value', app.channelVisible(ii), ...
                    'callback', @(src,~) app.toggleChannelVisible(src));
            end

            % tag each channel button by its index for easy lookup
            for ii = 1:numel(app.chbuts)
                set(app.chbuts(ii), 'Tag', num2str(ii));
            end

            set(app.fig, 'ResizeFcn', @(~,~) app.normalResizeFcn);
            set(app.panel, 'ResizeFcn', @(~,~) app.normalResizeFcn);
            modes.NormalMode.resize(app);
        end

        function resize(app)
            % Switch layout depending on split view state
            if app.splitMode
                app.layoutSplitAxes();
                return;
            end

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
            % position the split toggle button to the right of x-buttons
            if ~isempty(app.splitButton) && isvalid(app.splitButton)
                set(app.splitButton, 'Position', ...
                    max(1,[midle + (numel(app.xbuts)/2+0.5)*app.DEFS.BUTTONSIZE, ...
                    app.DEFS.PADDING, app.DEFS.BIGBUTTONSIZE, app.DEFS.BUTTONSIZE]));
            end
            % position the channel toggle buttons (above x-axis buttons)
            if ~isempty(app.chbuts)
                for i=1:numel(app.chbuts)
                    set(app.chbuts(i),'Position', ...
                        max(1,[midle - (numel(app.chbuts)/2 - i + 0.5)*app.DEFS.BIGBUTTONSIZE, ...
                        app.DEFS.PADDING + app.DEFS.BUTTONSIZE + app.DEFS.PADDING, ...
                        app.DEFS.BIGBUTTONSIZE, ...
                        app.DEFS.BUTTONSIZE]));
                end
            end
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
